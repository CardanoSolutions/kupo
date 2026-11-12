-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.App.Database.MigrationSpec
    ( spec
    ) where

import Kupo.Prelude

import Control.Tracer
    ( Tracer (..)
    , emit
    )
import Database.SQLite.Simple
    ( Connection
    , Only (..)
    , SQLData
    , SQLError (..)
    , execute_
    , query
    , query_
    , withConnection
    )
import Kupo.App.Database.Migration
    ( checkMigrations
    , migrations
    , runMigrations
    )
import Kupo.App.Database.Migration.Definition
    ( BatchRepair (..)
    , BatchResult (..)
    , Migration (..)
    , MigrationAction (..)
    , MigrationKind (..)
    , migrationKind
    )
import Kupo.App.Database.Migration.Plan
    ( Caller (..)
    , DatabaseState (..)
    , MigrationRequiredException (..)
    , MigrationState (..)
    , Plan (..)
    , PlannedAction (..)
    , PlannedStep (..)
    , Registry (..)
    , planMigrations
    )
import Kupo.App.Database.Types
    ( TraceDatabase (..)
    )
import Kupo.Control.MonadLog
    ( nullTracer
    )
import Test.Hspec
    ( Selector
    , Spec
    , context
    , parallel
    , shouldBe
    , shouldReturn
    , shouldThrow
    , specify
    )

import qualified Data.Text as T

spec :: Spec
spec = parallel $ do
    context "migrations" $ do
        -- Append only: never reorder, edit the id of, or remove an entry.
        specify "ids are exactly the known ones, in order" $
            migrationId <$> migrations `shouldBe` knownIds

        specify "ids are unique" $
            ordNub (migrationId <$> migrations) `shouldBe` migrationId <$> migrations

        specify "ids increase along the list" $ do
            let keys = idOrder . migrationId <$> migrations
            and (zipWith (<) keys (drop 1 keys)) `shouldBe` True

        specify "the known migrations are all automatic" $
            migrationKind <$> migrations `shouldBe` (AutomaticKind <$ knownIds)

        specify "a repair names an earlier migration as the one that introduced its data" $
            [ repairId
            | (position, Migration{migrationId = repairId, migrationAction = Repair repair}) <- zip [0 ..] migrations
            , repairDataIntroducedBy repair `notElem` (migrationId <$> take position migrations)
            ]
                `shouldBe` []

    context "planMigrations" $ do
        context "on a fresh database" $ do
            specify "applies automatic and blocking migrations, and skips repairs" $
                summarise (planMigrations Startup FreshDatabase (UserVersionOnly 0)
                    [ synthetic AutomaticKind "a", synthetic BlockingKind "x", synthetic RepairKind "r" ])
                    `shouldBe` ([("a", Apply ""), ("x", Apply ""), ("r", RegisterSkipped)], [], [])

        context "on an existing database" $ do
            specify "applies automatic migrations" $
                summarise (planMigrations Startup ExistingDatabase (registry [("a", Applied)])
                    [ synthetic AutomaticKind "a", synthetic AutomaticKind "b" ])
                    `shouldBe` ([("b", Apply "")], [], [])

            specify "registers repairs as pending and carries on past them" $
                summarise (planMigrations Startup ExistingDatabase (registry [("a", Applied)])
                    [ synthetic AutomaticKind "a", synthetic RepairKind "r", synthetic AutomaticKind "b" ])
                    `shouldBe` ([("r", RegisterPending), ("b", Apply "")], [], ["r"])

            specify "skips a repair when the plan also applies the migration that introduced its data" $
                summarise (planMigrations Startup ExistingDatabase (UserVersionOnly 1)
                    [ synthetic AutomaticKind "a", synthetic AutomaticKind "b", repairOfDataFrom "b" ])
                    `shouldBe` ([("b", Apply ""), ("r", RegisterSkipped)], [], [])

            specify "registers a repair as pending when its data predates the plan" $
                summarise (planMigrations Startup ExistingDatabase (registry [("a", Applied), ("b", Applied)])
                    [ synthetic AutomaticKind "a", synthetic AutomaticKind "b", repairOfDataFrom "b" ])
                    `shouldBe` ([("r", RegisterPending)], [], ["r"])

            specify "stops at a blocking migration and names what is pending" $
                summarise (planMigrations Startup ExistingDatabase (registry [])
                    [ synthetic AutomaticKind "a", synthetic BlockingKind "x", synthetic AutomaticKind "b", synthetic RepairKind "r" ])
                    `shouldBe` ([("a", Apply "")], ["x", "b", "r"], [])

        context "run by the operator" $ do
            specify "applies blocking migrations, and registers repairs as pending" $
                summarise (planMigrations Operator ExistingDatabase (registry [("a", Applied)])
                    [ synthetic AutomaticKind "a", synthetic BlockingKind "x", synthetic RepairKind "r", synthetic AutomaticKind "b" ])
                    `shouldBe` ([("x", Apply ""), ("r", RegisterPending), ("b", Apply "")], [], ["r"])

            specify "on a fresh database, skips repairs" $
                summarise (planMigrations Operator FreshDatabase (UserVersionOnly 0)
                    [ synthetic BlockingKind "x", synthetic RepairKind "r" ])
                    `shouldBe` ([("x", Apply ""), ("r", RegisterSkipped)], [], [])

        context "ordering" $ do
            specify "an automatic migration passes a pending repair" $
                summarise (planMigrations Startup ExistingDatabase (registry [("a", Applied), ("r", Pending)])
                    [ synthetic AutomaticKind "a", synthetic RepairKind "r", synthetic AutomaticKind "b" ])
                    `shouldBe` ([("b", Apply "")], [], ["r"])

            specify "nothing passes a pending blocking migration" $
                summarise (planMigrations Startup ExistingDatabase (registry [("r", Pending)])
                    [ synthetic RepairKind "r", synthetic BlockingKind "x", synthetic RepairKind "s", synthetic AutomaticKind "b" ])
                    `shouldBe` ([], ["x", "s", "b"], ["r"])

            specify "steps keep their position in the list" $
                stepPosition <$> planSteps (planMigrations Startup ExistingDatabase (registry [("a", Applied), ("r", Pending)])
                    [ synthetic AutomaticKind "a", synthetic RepairKind "r", synthetic AutomaticKind "b", synthetic RepairKind "s" ])
                    `shouldBe` [3, 4]

        context "registry" $ do
            specify "before the registry exists, the first user_version entries count as applied" $
                summarise (planMigrations Startup ExistingDatabase (UserVersionOnly 2)
                    [ synthetic AutomaticKind "a", synthetic BlockingKind "x", synthetic AutomaticKind "b" ])
                    `shouldBe` ([("b", Apply "")], [], [])

            specify "skips migrations already in the registry" $
                summarise (planMigrations Startup ExistingDatabase (registry [("a", Applied), ("r", Skipped), ("x", Applied)])
                    [ synthetic AutomaticKind "a", synthetic RepairKind "r", synthetic BlockingKind "x" ])
                    `shouldBe` ([], [], [])

    context "runMigrations" $ do
        specify "an upgrade from user_version 10 backfills the registry" $ withDatabase $ \conn -> do
            runMigrations nullTracer conn (take 10 migrations)
            userVersion conn `shouldReturn` [10]
            addCheckpoint conn
            runMigrations nullTracer conn migrations
            userVersion conn `shouldReturn` [11]
            -- Backfilled rows have no kupo_version and no applied_at.
            registryRows conn `shouldReturn`
                [ (migrationId, "applied", isRegistry, isRegistry)
                | Migration{migrationId} <- migrations
                , let isRegistry = migrationId == "v2.12.0/001"
                ]

        specify "a fresh database records repairs as skipped" $ withDatabase $ \conn -> do
            runMigrations nullTracer conn (migrations <> [synthetic RepairKind "test/repair"])
            userVersion conn `shouldReturn` [12]
            registryRow conn "test/repair" `shouldReturn` [("skipped", Nothing)]

        specify "an existing database registers repairs as pending, with a cutoff slot" $
            withDatabase $ \conn -> do
                runMigrations nullTracer conn migrations
                addCheckpoint conn
                runMigrations nullTracer conn $
                    migrations <> [synthetic RepairKind "test/repair", syntheticTable Automatic "after"]
                userVersion conn `shouldReturn` [13]
                registryRow conn "test/repair" `shouldReturn` [("pending", Just 42)]
                registryRow conn "test/after" `shouldReturn` [("applied", Nothing)]
                syntheticTables conn `shouldReturn` ["synthetic_after"]

        specify "a blocking migration on an existing database stops the runner" $
            withDatabase $ \conn -> do
                runMigrations nullTracer conn migrations
                addCheckpoint conn
                let extended = migrations <>
                        [ syntheticTable Automatic "before"
                        , syntheticTable Blocking "blocking"
                        , syntheticTable Automatic "after"
                        ]
                runMigrations nullTracer conn extended
                    `shouldThrow` isMigrationRequiredFor ["test/blocking", "test/after"]
                userVersion conn `shouldReturn` [12]
                syntheticTables conn `shouldReturn` ["synthetic_before"]
                registryRow conn "test/before" `shouldReturn` [("applied", Nothing)]
                registryRow conn "test/blocking" `shouldReturn` []

        specify "applies a blocking migration on a fresh database" $ withDatabase $ \conn -> do
            runMigrations nullTracer conn (migrations <> [syntheticTable Blocking "blocking"])
            syntheticTables conn `shouldReturn` ["synthetic_blocking"]
            registryRow conn "test/blocking" `shouldReturn` [("applied", Nothing)]

        specify "running twice changes nothing" $ withDatabase $ \conn -> do
            runMigrations nullTracer conn migrations
            addCheckpoint conn
            let extended = migrations <> [synthetic RepairKind "test/repair", syntheticTable Automatic "after"]
            runMigrations nullTracer conn extended
            before <- snapshot conn
            runMigrations nullTracer conn extended
            snapshot conn `shouldReturn` before

        specify "runs a statement that holds a ';', such as a trigger body" $ withDatabase $ \conn -> do
            let trigger = (synthetic AutomaticKind "test/trigger")
                    { migrationAction = Automatic
                        "CREATE TABLE synthetic_trigger (x INTEGER); \
                        \CREATE TRIGGER double_x AFTER INSERT ON synthetic_trigger \
                        \BEGIN UPDATE synthetic_trigger SET x = 2 * x; END;"
                    }
            runMigrations nullTracer conn (migrations <> [trigger])
            execute_ conn "INSERT INTO synthetic_trigger VALUES (21)"
            fmap fromOnly <$> query_ conn "SELECT x FROM synthetic_trigger" `shouldReturn` [42 :: Int]

        specify "a failing migration rolls back, and the runner logs it and stops" $ withDatabase $ \conn -> do
            (tr, readFailures) <- recordFailures
            let failing = (synthetic AutomaticKind "test/failing")
                    { migrationAction = Automatic
                        "CREATE TABLE synthetic_failing (x INTEGER); \
                        \INSERT INTO synthetic_failing VALUES (1); \
                        \SELECT * FROM no_such_table;"
                    }
            let extended = migrations <> [syntheticTable Automatic "before", failing, syntheticTable Automatic "after"]
            runMigrations tr conn extended
                `shouldThrow` \SQLError{} -> True
            readFailures `shouldReturn` ["test/failing"]
            -- As the step before the failing one left it.
            userVersion conn `shouldReturn` [length migrations + 1]
            syntheticTables conn `shouldReturn` ["synthetic_before"]
            registryRow conn "test/before" `shouldReturn` [("applied", Nothing)]
            registryRow conn "test/failing" `shouldReturn` []
            registryRow conn "test/after" `shouldReturn` []

    context "checkMigrations" $ do
        specify "refuses a blocked database, without writing" $ withDatabase $ \conn -> do
            runMigrations nullTracer conn migrations
            addCheckpoint conn
            before <- snapshot conn
            checkMigrations nullTracer conn
                (migrations <> [synthetic RepairKind "test/repair", syntheticTable Blocking "blocking"])
                `shouldThrow` isMigrationRequiredFor ["test/blocking"]
            snapshot conn `shouldReturn` before

        specify "carries on, without writing, when the writer has not migrated yet" $
            withDatabase $ \conn -> do
                runMigrations nullTracer conn (take 10 migrations)
                addCheckpoint conn
                before <- (,) <$> userVersion conn <*> schema conn
                checkMigrations nullTracer conn (migrations <> [synthetic RepairKind "test/repair"])
                ((,) <$> userVersion conn <*> schema conn) `shouldReturn` before

knownIds :: [Text]
knownIds =
    [ "v1.0.0-beta/001"
    , "v1.0.0/001"
    , "v1.0.0/002"
    , "v1.0.1/001"
    , "v2.0.0-beta/001"
    , "v2.1.0/001"
    , "v2.1.0/002"
    , "v2.1.0/003"
    , "v2.2.0/001"
    , "v2.10.0/001"
    , "v2.12.0/001"
    ]

-- Sorts ids such as @v1.0.0-beta/001@ by version, a pre-release before its release, then by
-- sequence number.
idOrder :: Text -> ([Int], Bool, Text)
idOrder migrationId =
    (mapMaybe (readMaybe . toString) (T.splitOn "." numbers), T.null preRelease, sequenceNumber)
  where
    (version, sequenceNumber) = T.breakOn "/" migrationId
    (numbers, preRelease) = T.breakOn "-" (T.drop 1 version)

summarise :: Plan -> ([(Text, PlannedAction)], [Text], [Text])
summarise Plan{planSteps, planBlocked, planPendingRepairs} =
    ( [ (migrationId stepMigration, stepAction) | PlannedStep{stepMigration, stepAction} <- planSteps ]
    , migrationId <$> planBlocked
    , migrationId <$> planPendingRepairs
    )

-- A tracer that records the id of every migration or repair whose failure it gets.
recordFailures :: IO (Tracer IO TraceDatabase, IO [Text])
recordFailures = do
    failures <- newIORef []
    let record = \case
            DatabaseMigrationFailed{failedMigration} -> modifyIORef' failures (<> [failedMigration])
            _otherEvent -> pass
    pure (Tracer $ emit record, readIORef failures)

isMigrationRequiredFor :: [Text] -> Selector MigrationRequiredException
isMigrationRequiredFor ids (MigrationRequiredException pendingMigrations) =
    (T.takeWhile (/= ' ') <$> pendingMigrations) == ids

withDatabase :: (Connection -> IO a) -> IO a
withDatabase =
    withConnection ":memory:"

addCheckpoint :: Connection -> IO ()
addCheckpoint conn =
    execute_ conn "INSERT INTO checkpoints (header_hash, slot_no) VALUES (x'00', 42)"

userVersion :: Connection -> IO [Int]
userVersion conn =
    fmap fromOnly <$> query_ conn "PRAGMA user_version"

registryRows :: Connection -> IO [(Text, Text, Bool, Bool)]
registryRows conn =
    query_ conn
        "SELECT id, state, kupo_version IS NOT NULL, applied_at IS NOT NULL \
        \FROM migrations ORDER BY rowid"

registryRow :: Connection -> Text -> IO [(Text, Maybe Int)]
registryRow conn migrationId =
    query conn "SELECT state, cutoff_slot FROM migrations WHERE id = ?" (Only migrationId)

syntheticTables :: Connection -> IO [Text]
syntheticTables conn =
    fmap fromOnly <$> query_ conn
        "SELECT name FROM sqlite_master WHERE type = 'table' AND name LIKE 'synthetic%' ORDER BY name"

schema :: Connection -> IO [[SQLData]]
schema conn =
    query_ conn "SELECT type, name, sql FROM sqlite_master ORDER BY name"

snapshot :: Connection -> IO ([Int], [[SQLData]], [[SQLData]])
snapshot conn =
    (,,) <$> userVersion conn <*> schema conn <*> query_ conn "SELECT * FROM migrations ORDER BY id"

registry :: [(Text, MigrationState)] -> Registry
registry =
    RegistryRows . fromList

-- A synthetic migration that does nothing. A repair is of data that no known migration introduced.
synthetic :: MigrationKind -> Text -> Migration
synthetic kind migrationId =
    Migration
        { migrationId
        , migrationSummary = "A synthetic migration"
        , migrationAction =
            case kind of
                AutomaticKind -> Automatic ""
                BlockingKind -> Blocking ""
                RepairKind -> Repair $ idleRepair "test/none"
        }

-- A synthetic migration with id @test/<name>@ that creates the table @synthetic_<name>@.
syntheticTable :: (ByteString -> MigrationAction) -> Text -> Migration
syntheticTable toAction name =
    (synthetic AutomaticKind $ "test/" <> name)
        { migrationAction = toAction . encodeUtf8 $ "CREATE TABLE synthetic_" <> name <> " (x INTEGER)" }

-- The repair @r@ of the data that the migration @introducedBy@ introduced.
repairOfDataFrom :: Text -> Migration
repairOfDataFrom introducedBy =
    (synthetic RepairKind "r") { migrationAction = Repair $ idleRepair introducedBy }

-- A repair with nothing to do, of the data that the migration @introducedBy@ introduced.
idleRepair :: Text -> BatchRepair
idleRepair introducedBy =
    BatchRepair
        { repairDataIntroducedBy = introducedBy
        , repairPrecondition = \_ _ -> pure $ Right ()
        , repairEstimate = \_ _ -> pure ""
        , repairBatch = \_ _ _ -> pure $ RepairDone ""
        }
