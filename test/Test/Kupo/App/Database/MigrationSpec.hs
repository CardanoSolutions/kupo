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
    , execute
    , execute_
    , query
    , query_
    , withConnection
    )
import Kupo
    ( Env (..)
    , newEnvironment
    )
import Kupo.App.Database
    ( newDBPool
    )
import Kupo.App.Database.Migration
    ( UnexpectedPendingMigrationException (..)
    , checkMigrations
    , migrateDatabase
    , migrations
    , readStatus
    , runMigrations
    )
import Kupo.App.Database.Migration.Command
    ( DatabaseCommandException (..)
    , databaseMigrate
    , withExclusiveDatabase
    , withReadOnlyDatabase
    )
import Kupo.App.Database.Migration.Definition
    ( BatchRepair (..)
    , BatchResult (..)
    , Migration (..)
    , MigrationAction (..)
    , MigrationKind (..)
    , RepairContext (..)
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
    , UnknownMigration (..)
    , UnknownMigrationsException (..)
    , planMigrations
    )
import Kupo.App.Database.Migration.Status
    ( MigrationStatus (..)
    , StatusReport (..)
    , renderStatus
    , statusExitCode
    )
import Kupo.App.Database.Types
    ( DBPool (..)
    , TraceDatabase (..)
    )
import Kupo.Control.MonadLog
    ( nullTracer
    )
import Kupo.Control.MonadThrow
    ( MonadThrow (..)
    )
import Kupo.Data.Cardano.SlotNo
    ( SlotNo (..)
    )
import Kupo.Data.Configuration
    ( ChainProducer (..)
    , Configuration (..)
    , DatabaseLocation (..)
    , DeferIndexesInstallation (..)
    , InputManagement (..)
    )
import System.Directory
    ( doesFileExist
    )
import System.Exit
    ( ExitCode (..)
    )
import System.FilePath
    ( (</>)
    )
import System.IO.Temp
    ( withSystemTempDirectory
    )
import Test.Hspec
    ( Selector
    , Spec
    , context
    , parallel
    , shouldBe
    , shouldContain
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

        specify "the known migrations are automatic, apart from the spent input repair" $
            migrationKind <$> migrations `shouldBe` (AutomaticKind <$ take 11 knownIds) <> [RepairKind]

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

            specify "names the ids it does not know" $
                planUnknown (planMigrations Startup ExistingDatabase (registry [("a", Applied), ("v9.9.9/001", Applied)])
                    [ synthetic AutomaticKind "a", synthetic AutomaticKind "b" ])
                    `shouldBe` ["v9.9.9/001"]

    context "withDatabaseExclusiveWriter" $ do
        specify "refuses a database that a newer Kupo migrated, before a PRAGMA changes its file" $
            withSystemTempDirectory "kupo-migration" $ \dir -> do
                let databaseFile = dir </> "kupo.sqlite3"
                withConnection databaseFile $ \conn -> do
                    runMigrations nullTracer conn migrations
                    addUnknownMigration conn
                before <- readFileBS databaseFile
                pool <- newDBPool nullTracer False (Dir dir) 43200
                withDatabaseExclusiveWriter pool InstallIndexesIfNotExist (const pass)
                    `shouldThrow` isUnknownMigrations ["v9.9.9/001"]
                readFileBS databaseFile `shouldReturn` before
                doesFileExist (databaseFile <> "-wal") `shouldReturn` False
                withConnection databaseFile $ \conn -> do
                    fmap fromOnly <$> query_ conn "PRAGMA journal_mode" `shouldReturn` ["delete" :: Text]
                    fmap fromOnly <$> query_ conn "SELECT name FROM sqlite_master WHERE name = 'sqlite_stat1'"
                        `shouldReturn` ([] :: [Text])

        specify "creates a new database with 32 KiB pages" $
            withSystemTempDirectory "kupo-migration" $ \dir -> do
                pool <- newDBPool nullTracer False (Dir dir) 43200
                withDatabaseExclusiveWriter pool InstallIndexesIfNotExist (const pass)
                withConnection (dir </> "kupo.sqlite3") $ \conn ->
                    fmap fromOnly <$> query_ conn "PRAGMA page_size" `shouldReturn` [32768 :: Int]

    context "runMigrations" $ do
        specify "an upgrade from user_version 10 backfills the registry" $ withDatabase $ \conn -> do
            runMigrations nullTracer conn (take 10 migrations)
            userVersion conn `shouldReturn` [10]
            addCheckpoint conn
            runMigrations nullTracer conn migrations
            userVersion conn `shouldReturn` [12]
            -- Backfilled rows have no kupo_version and no applied_at.
            registryRows conn `shouldReturn`
                [ (migrationId, "applied", isRegistry, isRegistry)
                | Migration{migrationId} <- take 11 migrations
                , let isRegistry = migrationId == "v2.12.0/001"
                ]
                <> [("v2.12.0/002", "pending", True, False)]

        specify "a fresh database records repairs as skipped" $ withDatabase $ \conn -> do
            runMigrations nullTracer conn (migrations <> [synthetic RepairKind "test/repair"])
            userVersion conn `shouldReturn` [13]
            registryRow conn "v2.12.0/002" `shouldReturn` [("skipped", Nothing)]
            registryRow conn "test/repair" `shouldReturn` [("skipped", Nothing)]

        specify "an existing database registers repairs as pending, with a cutoff slot" $
            withDatabase $ \conn -> do
                runMigrations nullTracer conn migrations
                addCheckpoint conn
                runMigrations nullTracer conn $
                    migrations <> [synthetic RepairKind "test/repair", syntheticTable Automatic "after"]
                userVersion conn `shouldReturn` [14]
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
                userVersion conn `shouldReturn` [13]
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

        specify "refuses a database that a newer Kupo migrated, without writing, and names that Kupo" $
            withDatabase $ \conn -> do
                runMigrations nullTracer conn migrations
                addCheckpoint conn
                addUnknownMigration conn
                before <- snapshot conn
                (tr, readRefusals) <- recordRefusals
                runMigrations tr conn (migrations <> [syntheticTable Automatic "after"])
                    `shouldThrow` (== UnknownMigrationsException [UnknownMigration "v9.9.9/001" (Just "v9.9.9")])
                readRefusals `shouldReturn` [["v9.9.9/001 (recorded by Kupo v9.9.9)"]]
                snapshot conn `shouldReturn` before

        specify "lists each unknown migration with the version of Kupo that recorded it, when known" $
            displayException
                (UnknownMigrationsException
                    [ UnknownMigration "v9.9.9/001" (Just "v9.9.9+abcdef")
                    , UnknownMigration "v9.9.9/002" Nothing
                    ])
                `shouldBe` toString (T.unlines
                    [ "This database was migrated by a newer Kupo. Run that version, or a later one, instead."
                    , "Unknown migrations:"
                    , "  v9.9.9/001 (recorded by Kupo v9.9.9+abcdef)"
                    , "  v9.9.9/002"
                    ])

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

    context "startup exit code" $ do
        specify "a blocked startup exits with code 3, any other failure with 1" $ do
            env <- newEnvironment Configuration
                { chainProducer = CardanoNode
                    { nodeSocket = "/dev/null"
                    , nodeConfig = "/dev/null"
                    , networkParameters = ()
                    }
                , databaseLocation = InMemory Nothing
                , serverHost = "127.0.0.1"
                , serverPort = 0
                , since = Nothing
                , until = Nothing
                , patterns = fromList []
                , inputManagement = MarkSpentInputs
                , longestRollback = 43200
                , garbageCollectionInterval = 180
                , deferIndexes = InstallIndexesIfNotExist
                }
            crashWith env (toException $ MigrationRequiredException ["test/blocking"])
                `shouldThrow` (== ExitFailure 3)
            crashWith env (toException $ UnknownMigrationsException [UnknownMigration "v9.9.9/001" (Just "v9.9.9")])
                `shouldThrow` (== ExitFailure 1)

    context "checkMigrations" $ do
        specify "refuses a blocked database, without writing" $ withDatabase $ \conn -> do
            runMigrations nullTracer conn migrations
            addCheckpoint conn
            before <- snapshot conn
            checkMigrations nullTracer conn
                (migrations <> [synthetic RepairKind "test/repair", syntheticTable Blocking "blocking"])
                `shouldThrow` isMigrationRequiredFor ["test/blocking"]
            snapshot conn `shouldReturn` before

        specify "refuses a database that a newer Kupo migrated" $ withDatabase $ \conn -> do
            runMigrations nullTracer conn migrations
            addUnknownMigration conn
            checkMigrations nullTracer conn migrations
                `shouldThrow` (== UnknownMigrationsException [UnknownMigration "v9.9.9/001" (Just "v9.9.9")])

        specify "carries on, without writing, when the writer has not migrated yet" $
            withDatabase $ \conn -> do
                runMigrations nullTracer conn (take 10 migrations)
                addCheckpoint conn
                before <- (,) <$> userVersion conn <*> schema conn
                checkMigrations nullTracer conn (migrations <> [synthetic RepairKind "test/repair"])
                ((,) <$> userVersion conn <*> schema conn) `shouldReturn` before

    context "migrateDatabase" $ do
        specify "applies a blocking migration, after which Kupo starts" $ withDatabase $ \conn -> do
            runMigrations nullTracer conn migrations
            addCheckpoint conn
            let extended = migrations <> [syntheticTable Blocking "blocking"]
            runMigrations nullTracer conn extended
                `shouldThrow` isMigrationRequiredFor ["test/blocking"]
            migrateDatabase nullTracer conn False extended `shouldReturn` []
            syntheticTables conn `shouldReturn` ["synthetic_blocking"]
            registryRow conn "test/blocking" `shouldReturn` [("applied", Nothing)]
            runMigrations nullTracer conn extended

        specify "runs a pending repair batch by batch, then marks it applied" $
            withRepairTarget $ \conn -> do
                batches <- newIORef []
                let extended = migrations <> [countingRepair batches]
                runMigrations nullTracer conn extended
                migrateDatabase nullTracer conn False extended `shouldReturn` []
                readIORef batches `shouldReturn` [Nothing, Just "3", Just "6"]
                repairTarget conn `shouldReturn` replicate 9 1
                repairRow conn `shouldReturn` [("applied", Nothing, True, True)]

        specify "logs the summary of a repair once it is done" $ withRepairTarget $ \conn -> do
            batches <- newIORef []
            (tr, readSummaries) <- recordSummaries
            migrateDatabase tr conn False (migrations <> [countingRepair batches]) `shouldReturn` []
            readSummaries `shouldReturn` [("test/repair", "9 rows")]

        specify "resumes after a crash, without repeating a batch" $
            withRepairTarget $ \conn -> do
                batches <- newIORef []
                migrateDatabase nullTracer conn False (migrations <> [crashingRepair batches])
                    `shouldThrow` (== SimulatedCrash)
                repairRow conn `shouldReturn` [("pending", Just "3", True, False)]
                repairTarget conn `shouldReturn` replicate 3 1 <> replicate 6 0
                writeIORef batches []
                migrateDatabase nullTracer conn False (migrations <> [countingRepair batches])
                    `shouldReturn` []
                readIORef batches `shouldReturn` [Just "3", Just "6"]
                repairTarget conn `shouldReturn` replicate 9 1
                repairRow conn `shouldReturn` [("applied", Nothing, True, True)]

        specify "logs the repair whose batch fails" $ withRepairTarget $ \conn -> do
            batches <- newIORef []
            (tr, readFailures) <- recordFailures
            migrateDatabase tr conn False (migrations <> [crashingRepair batches])
                `shouldThrow` (== SimulatedCrash)
            readFailures `shouldReturn` ["test/repair"]

        specify "running twice changes nothing" $ withRepairTarget $ \conn -> do
            batches <- newIORef []
            let extended = migrations <> [countingRepair batches]
            migrateDatabase nullTracer conn False extended `shouldReturn` []
            before <- (,) <$> snapshot conn <*> repairTarget conn
            writeIORef batches []
            migrateDatabase nullTracer conn False extended `shouldReturn` []
            readIORef batches `shouldReturn` []
            ((,) <$> snapshot conn <*> repairTarget conn) `shouldReturn` before

        specify "leaves a repair pending while its precondition fails" $
            withRepairTarget $ \conn -> do
                contexts <- newIORef []
                let extended = migrations <> [confirmingRepair contexts]
                migrateDatabase nullTracer conn False extended `shouldReturn` ["test/repair"]
                repairRow conn `shouldReturn` [("pending", Nothing, True, False)]
                repairTarget conn `shouldReturn` replicate 9 0
                migrateDatabase nullTracer conn True extended `shouldReturn` []
                repairRow conn `shouldReturn` [("applied", Nothing, True, True)]
                repairTarget conn `shouldReturn` replicate 9 1
                readIORef contexts `shouldReturn` [(42, False), (42, True)]

        specify "refuses a database that a newer Kupo migrated, without writing" $
            withDatabase $ \conn -> do
                runMigrations nullTracer conn migrations
                addCheckpoint conn
                addUnknownMigration conn
                before <- snapshot conn
                migrateDatabase nullTracer conn False (migrations <> [syntheticTable Blocking "blocking"])
                    `shouldThrow` isUnknownMigrations ["v9.9.9/001"]
                snapshot conn `shouldReturn` before

        specify "refuses a pending registry row of a migration that is not a repair" $
            withDatabase $ \conn -> do
                runMigrations nullTracer conn migrations
                execute_ conn "UPDATE migrations SET state = 'pending', cutoff_slot = 42 WHERE id = 'v2.10.0/001'"
                migrateDatabase nullTracer conn False migrations
                    `shouldThrow` isUnexpectedPendingMigration "v2.10.0/001"

    context "readStatus" $ do
        specify "reports every migration, without writing" $ withRepairTarget $ \conn -> do
            batches <- newIORef []
            let extended = migrations <> [countingRepair batches, syntheticTable Blocking "blocking"]
            runMigrations nullTracer conn extended
                `shouldThrow` isMigrationRequiredFor ["test/blocking"]
            execute_ conn "UPDATE migrations SET cursor = CAST('3' AS BLOB) WHERE id = 'test/repair'"
            before <- snapshot conn
            StatusReport{reportMigrations, reportBlocked, reportUnknown} <- readStatus conn extended
            snapshot conn `shouldReturn` before
            reportBlocked `shouldBe` ["test/blocking"]
            reportUnknown `shouldBe` []
            -- The database was fresh when Kupo registered the spent input repair.
            statusState <$> reportMigrations
                `shouldBe` (Just Applied <$ take 11 migrations) <> [Just Skipped, Just Pending, Nothing]
            drop (length migrations) reportMigrations `shouldBe`
                [ MigrationStatus
                    { statusId = "test/repair"
                    , statusKind = RepairKind
                    , statusState = Just Pending
                    , statusSummary = "A synthetic migration"
                    , statusEstimate = Just "9 rows up to slot 42"
                    , statusCursor = Just "3"
                    }
                , MigrationStatus
                    { statusId = "test/blocking"
                    , statusKind = BlockingKind
                    , statusState = Nothing
                    , statusSummary = "A synthetic migration"
                    , statusEstimate = Nothing
                    , statusCursor = Nothing
                    }
                ]

        specify "lists the ids it does not know, instead of refusing" $ withDatabase $ \conn -> do
            runMigrations nullTracer conn migrations
            addUnknownMigration conn
            reportUnknown <$> readStatus conn migrations
                `shouldReturn` [UnknownMigration "v9.9.9/001" (Just "v9.9.9")]

        specify "names the version that recorded each unknown id" $ withDatabase $ \conn -> do
            runMigrations nullTracer conn migrations
            execute_ conn
                "INSERT INTO migrations (id, state, kupo_version, registered_at) \
                \VALUES ('v9.9.9/001', 'applied', 'v9.9.9+abc123', 0), ('v9.9.9/002', 'applied', NULL, 0)"
            report <- readStatus conn migrations
            reportUnknown report `shouldBe`
                [ UnknownMigration "v9.9.9/001" (Just "v9.9.9+abc123")
                , UnknownMigration "v9.9.9/002" Nothing
                ]
            T.lines (renderStatus report) `shouldContain`
                [ "  v9.9.9/001 (recorded by Kupo v9.9.9+abc123)"
                , "  v9.9.9/002"
                ]

        specify "on an empty database, nothing is applied and nothing blocks" $
            withDatabase $ \conn -> do
                StatusReport{reportMigrations, reportBlocked} <-
                    readStatus conn (migrations <> [syntheticTable Blocking "blocking"])
                all (isNothing . statusState) reportMigrations `shouldBe` True
                reportBlocked `shouldBe` []

    context "renderStatus" $ do
        specify "aligns columns, details repairs, and ends with hints" $
            renderStatus StatusReport
                { reportMigrations =
                    [ MigrationStatus "v1.0.0/001" AutomaticKind (Just Applied) "Create a table" Nothing Nothing
                    , MigrationStatus "test/repair" RepairKind (Just Pending) "Fix rows" (Just "9 rows") (Just "3")
                    , MigrationStatus "test/blocking" BlockingKind Nothing "Rewrite rows" Nothing Nothing
                    ]
                , reportBlocked = ["test/blocking"]
                , reportUnknown = [UnknownMigration "v9.9.9/001" (Just "v9.9.9+abc123")]
                }
                `shouldBe` T.unlines
                    [ "v1.0.0/001     automatic  applied          Create a table"
                    , "test/repair    repair     pending          Fix rows"
                    , "    estimate: 9 rows"
                    , "    resume cursor: 33"
                    , "test/blocking  blocking   not yet applied  Rewrite rows"
                    , ""
                    , "Unknown migrations:"
                    , "  v9.9.9/001 (recorded by Kupo v9.9.9+abc123)"
                    , ""
                    , "This database was migrated by a newer Kupo. Run that version, or a later one, instead."
                    , "The database needs migrations that Kupo does not apply on its own. \
                      \Run 'kupo db migrate' to apply them, then start Kupo again."
                    , "Data repairs are pending. Run 'kupo db migrate' to apply them."
                    ]

        specify "says when the database is up to date" $
            renderStatus StatusReport
                { reportMigrations =
                    [ MigrationStatus "v1.0.0/001" AutomaticKind (Just Applied) "Create a table" Nothing Nothing
                    , MigrationStatus "test/repair" RepairKind (Just Skipped) "Fix rows" Nothing Nothing
                    ]
                , reportBlocked = []
                , reportUnknown = []
                }
                `shouldBe` T.unlines
                    [ "v1.0.0/001   automatic  applied          Create a table"
                    , "test/repair  repair     skipped          Fix rows"
                    , ""
                    , "The database is up to date."
                    ]

    context "statusExitCode" $ do
        specify "succeeds when Kupo would start" $
            statusExitCode (statusReport [] []) `shouldBe` ExitSuccess

        specify "exits with 3 when a blocking migration is pending" $
            statusExitCode (statusReport ["test/blocking"] []) `shouldBe` ExitFailure 3

        specify "exits with 1 when a newer Kupo migrated the database, as Kupo does" $ do
            statusExitCode (statusReport [] [UnknownMigration "v9.9.9/001" Nothing]) `shouldBe` ExitFailure 1
            statusExitCode (statusReport ["test/blocking"] [UnknownMigration "v9.9.9/001" Nothing]) `shouldBe` ExitFailure 1

    context "database access" $ do
        specify "reading never creates the database" $
            withSystemTempDirectory "kupo-migration" $ \dir -> do
                withReadOnlyDatabase dir (const pass)
                    `shouldThrow` (== DatabaseNotFound (dir </> "kupo.sqlite3"))
                doesFileExist (dir </> "kupo.sqlite3") `shouldReturn` False

        specify "the exclusive lock fails while another connection holds a write lock" $
            withSystemTempDirectory "kupo-migration" $ \dir ->
                withConnection (dir </> "kupo.sqlite3") $ \conn -> do
                    runMigrations nullTracer conn migrations
                    execute_ conn "BEGIN IMMEDIATE"
                    execute_ conn "INSERT INTO patterns (pattern) VALUES ('*')"
                    withExclusiveDatabase dir (const pass) `shouldThrow` (== DatabaseBusy)
                    execute_ conn "ROLLBACK"

        specify "the exclusive lock fails while another connection is open in WAL mode" $
            withSystemTempDirectory "kupo-migration" $ \dir ->
                withConnection (dir </> "kupo.sqlite3") $ \conn -> do
                    execute_ conn "PRAGMA journal_mode = WAL"
                    runMigrations nullTracer conn migrations
                    withExclusiveDatabase dir (const pass) `shouldThrow` (== DatabaseBusy)

        specify "the exclusive lock keeps other connections out until it is released" $
            withSystemTempDirectory "kupo-migration" $ \dir -> do
                withConnection (dir </> "kupo.sqlite3") $ \conn ->
                    runMigrations nullTracer conn migrations
                withExclusiveDatabase dir $ \_ ->
                    withReadOnlyDatabase dir (`readStatus` migrations)
                        `shouldThrow` (== DatabaseLocked)
                withExclusiveDatabase dir (\conn -> migrateDatabase nullTracer conn False migrations)
                    `shouldReturn` []

        specify "migrating refuses a database that a newer Kupo migrated, without writing" $
            withSystemTempDirectory "kupo-migration" $ \dir -> do
                let databaseFile = dir </> "kupo.sqlite3"
                withConnection databaseFile $ \conn -> do
                    runMigrations nullTracer conn migrations
                    addUnknownMigration conn
                before <- readFileBS databaseFile
                (tr, readRefusals) <- recordRefusals
                databaseMigrate tr dir False `shouldReturn` ExitFailure 1
                readRefusals `shouldReturn` [["v9.9.9/001 (recorded by Kupo v9.9.9)"]]
                readFileBS databaseFile `shouldReturn` before
                doesFileExist (databaseFile <> "-wal") `shouldReturn` False
                doesFileExist (databaseFile <> "-journal") `shouldReturn` False
                withConnection databaseFile $ \conn ->
                    fmap fromOnly <$> query_ conn "PRAGMA journal_mode" `shouldReturn` ["delete" :: Text]

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
    , "v2.12.0/002"
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

statusReport :: [Text] -> [UnknownMigration] -> StatusReport
statusReport reportBlocked reportUnknown =
    StatusReport{reportMigrations = [], reportBlocked, reportUnknown}

-- A tracer that records the repair id and summary of every applied repair.
recordSummaries :: IO (Tracer IO TraceDatabase, IO [(Text, Text)])
recordSummaries = do
    summaries <- newIORef []
    let record = \case
            DatabaseRepairApplied{repair, summary} -> modifyIORef' summaries (<> [(repair, summary)])
            _otherEvent -> pass
    pure (Tracer $ emit record, readIORef summaries)

-- A tracer that records the id of every migration or repair whose failure it gets.
recordFailures :: IO (Tracer IO TraceDatabase, IO [Text])
recordFailures = do
    failures <- newIORef []
    let record = \case
            DatabaseMigrationFailed{failedMigration} -> modifyIORef' failures (<> [failedMigration])
            DatabaseRepairFailed{repair} -> modifyIORef' failures (<> [repair])
            _otherEvent -> pass
    pure (Tracer $ emit record, readIORef failures)

-- A tracer that records the unknown migrations of every refusal it gets.
recordRefusals :: IO (Tracer IO TraceDatabase, IO [[Text]])
recordRefusals = do
    refusals <- newIORef []
    let record = \case
            DatabaseMigratedByNewerKupo{unknownMigrations} -> modifyIORef' refusals (<> [unknownMigrations])
            _otherEvent -> pass
    pure (Tracer $ emit record, readIORef refusals)

isMigrationRequiredFor :: [Text] -> Selector MigrationRequiredException
isMigrationRequiredFor ids (MigrationRequiredException pendingMigrations) =
    (T.takeWhile (/= ' ') <$> pendingMigrations) == ids

isUnknownMigrations :: [Text] -> Selector UnknownMigrationsException
isUnknownMigrations ids (UnknownMigrationsException unknownMigrations) =
    (unknownId <$> unknownMigrations) == ids

isUnexpectedPendingMigration :: Text -> Selector UnexpectedPendingMigrationException
isUnexpectedPendingMigration expectedId (UnexpectedPendingMigration migrationId) =
    migrationId == expectedId

withDatabase :: (Connection -> IO a) -> IO a
withDatabase =
    withConnection ":memory:"

addCheckpoint :: Connection -> IO ()
addCheckpoint conn =
    execute_ conn "INSERT INTO checkpoints (header_hash, slot_no) VALUES (x'00', 42)"

-- As if a newer Kupo had applied a migration this version does not know.
addUnknownMigration :: Connection -> IO ()
addUnknownMigration conn =
    execute_ conn
        "INSERT INTO migrations (id, state, kupo_version, registered_at) \
        \VALUES ('v9.9.9/001', 'applied', 'v9.9.9', 0)"

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

-- The state of @test/repair@, its cursor, and whether it records a version and an apply time.
repairRow :: Connection -> IO [(Text, Maybe ByteString, Bool, Bool)]
repairRow conn =
    query_ conn
        "SELECT state, cursor, kupo_version IS NOT NULL, applied_at IS NOT NULL \
        \FROM migrations WHERE id = 'test/repair'"

-- A database with a checkpoint and nine rows for a synthetic repair to fix.
withRepairTarget :: (Connection -> IO a) -> IO a
withRepairTarget action = withDatabase $ \conn -> do
    runMigrations nullTracer conn migrations
    addCheckpoint conn
    execute_ conn "CREATE TABLE repair_target (k INTEGER PRIMARY KEY, n INTEGER NOT NULL)"
    forM_ [1 .. 9 :: Int] $ \k ->
        execute conn "INSERT INTO repair_target (k, n) VALUES (?, 0)" (Only k)
    action conn

repairTarget :: Connection -> IO [Int]
repairTarget conn =
    fmap fromOnly <$> query_ conn "SELECT n FROM repair_target ORDER BY k"

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

-- The repair @test/repair@, which adds 1 to three rows of @repair_target@ per batch, so that a
-- batch run twice shows. It records the cursor of every batch it runs.
countingRepair :: IORef [Maybe ByteString] -> Migration
countingRepair batches =
    syntheticRepair (\_ _ -> pure $ Right ()) $ \conn cursor -> do
        modifyIORef' batches (<> [cursor])
        incrementRows conn cursor

-- Like 'countingRepair', but its second batch crashes after its work, before it commits.
crashingRepair :: IORef [Maybe ByteString] -> Migration
crashingRepair batches =
    syntheticRepair (\_ _ -> pure $ Right ()) $ \conn cursor -> do
        modifyIORef' batches (<> [cursor])
        result <- incrementRows conn cursor
        when (cursor == Just "3") $ throwIO SimulatedCrash
        pure result

-- Like 'countingRepair', but it needs the operator to confirm a complete index. It records the
-- cutoff slot and the confirmation that each precondition check gets.
confirmingRepair :: IORef [(Word64, Bool)] -> Migration
confirmingRepair contexts =
    syntheticRepair precondition incrementRows
  where
    precondition _ RepairContext{repairCutoffSlot, repairCompleteIndexConfirmed} = do
        modifyIORef' contexts (<> [(unSlotNo repairCutoffSlot, repairCompleteIndexConfirmed)])
        pure $ if repairCompleteIndexConfirmed then Right () else Left "The index may be incomplete."

syntheticRepair
    :: (Connection -> RepairContext -> IO (Either Text ()))
    -> (Connection -> Maybe ByteString -> IO BatchResult)
    -> Migration
syntheticRepair precondition doBatch =
    (synthetic RepairKind "test/repair")
        { migrationAction =
            Repair BatchRepair
                { repairDataIntroducedBy = "v1.0.0-beta/001"
                , repairPrecondition = precondition
                , repairEstimate = \_ RepairContext{repairCutoffSlot} ->
                    pure $ "9 rows up to slot " <> show (unSlotNo repairCutoffSlot)
                , repairBatch = const . doBatch
                }
        }

-- The cursor is the last row done, in decimal.
incrementRows :: Connection -> Maybe ByteString -> IO BatchResult
incrementRows conn cursor = do
    let lastDone = fromMaybe 0 $ readMaybe . decodeUtf8 =<< cursor
    let nextDone = lastDone + 3 :: Int
    execute conn "UPDATE repair_target SET n = n + 1 WHERE k > ? AND k <= ?" (lastDone, nextDone)
    pure $ if nextDone >= 9 then RepairDone "9 rows" else NextBatch (show nextDone)

data SimulatedCrash = SimulatedCrash
    deriving (Eq, Show)

instance Exception SimulatedCrash
