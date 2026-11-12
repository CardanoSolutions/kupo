-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.App.Database.MigrationSpec
    ( spec
    ) where

import Kupo.Prelude

import Kupo.App.Database.Migration
    ( migrations
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
    , MigrationState (..)
    , Plan (..)
    , PlannedAction (..)
    , PlannedStep (..)
    , Registry (..)
    , planMigrations
    )
import Test.Hspec
    ( Spec
    , context
    , parallel
    , shouldBe
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
