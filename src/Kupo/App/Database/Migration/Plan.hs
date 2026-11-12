--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.Database.Migration.Plan
    ( -- * Plan
      planMigrations
    , Caller (..)
    , DatabaseState (..)
    , Plan (..)
    , PlannedStep (..)
    , PlannedAction (..)

      -- * Registry
    , Registry (..)
    , MigrationState (..)
    , registryState
    ) where

import Kupo.Prelude

import Kupo.App.Database.Migration.Definition
    ( BatchRepair (..)
    , Migration (..)
    , MigrationAction (..)
    , MigrationKind (..)
    , migrationKind
    )

import qualified Data.Map as Map

-- | Decide what runs, in list order. Migrations already in the registry are skipped.
planMigrations
    :: Caller
    -- ^ Who runs the plan.
    -> DatabaseState
    -- ^ Whether the database has a checkpoint.
    -> Registry
    -- ^ What the database records about its migrations.
    -> [Migration]
    -- ^ All known migrations, oldest first.
    -> Plan
planMigrations caller databaseState registry knownMigrations = Plan
    { planSteps = steps
    , planBlocked = snd <$> blocked
    , planPendingRepairs =
        [ migration
        | (position, migration) <- indexed
        , registryState registry position migration == Just Pending
            || position `elem` newlyPending
        ]
    }
  where
    indexed = zip [1 ..] knownMigrations
    unprocessed = filter (isNothing . uncurry (registryState registry)) indexed
    (runnable, blocked) = break (isBlocking . snd) unprocessed
    steps = uncurry planStep <$> runnable
    newlyPending = [ stepPosition | PlannedStep{stepPosition, stepAction = RegisterPending} <- steps ]
    newlyApplied = [ migrationId | (_, migration@Migration{migrationId}) <- runnable, migrationKind migration /= RepairKind ]

    -- A fresh database has nothing to protect, so a blocking migration runs there.
    isBlocking migration =
        caller == Startup && databaseState == ExistingDatabase && migrationKind migration == BlockingKind

    planStep position migration@Migration{migrationAction} = PlannedStep position migration $
        case migrationAction of
            Automatic sql -> Apply sql
            Blocking sql -> Apply sql
            Repair batchRepair | databaseState == FreshDatabase || hasNoDataToRepair batchRepair -> RegisterSkipped
                               | otherwise -> RegisterPending

    -- The plan applies the migration that introduced the data to repair, so there is none yet.
    hasNoDataToRepair BatchRepair{repairDataIntroducedBy} =
        repairDataIntroducedBy `elem` newlyApplied

-- | Who runs the migrations.
data Caller
    = Startup
    -- ^ Kupo, as it starts: it stops at a 'Blocking' migration on an existing database.
    | Operator
    -- ^ The operator, with @kupo db migrate@: it applies 'Blocking' migrations too.
    deriving (Generic, Eq, Show)

-- | Whether the database holds any indexed data.
data DatabaseState
    = FreshDatabase
    -- ^ No checkpoint yet: there is no data to protect or to repair.
    | ExistingDatabase
    -- ^ At least one checkpoint.
    deriving (Generic, Eq, Show)

-- | What 'planMigrations' decides.
data Plan = Plan
    { planSteps :: ![PlannedStep]
    -- ^ Steps to run, in order, each in its own transaction.
    , planBlocked :: ![Migration]
    -- ^ The first pending 'Blocking' migration and every unprocessed one after it; none of them runs.
    , planPendingRepairs :: ![Migration]
    -- ^ Repairs that are still pending once the steps have run.
    }

data PlannedStep = PlannedStep
    { stepPosition :: !Int
    -- ^ One-based position in the migration list, which becomes the @user_version@.
    , stepMigration :: !Migration
    , stepAction :: !PlannedAction
    }

data PlannedAction
    = Apply !ByteString
    -- ^ Run the migration's SQL and record it as 'Applied'.
    | RegisterPending
    -- ^ Record a repair as 'Pending', without running it.
    | RegisterSkipped
    -- ^ Record a repair as 'Skipped': the database has nothing to repair.
    deriving (Generic, Eq, Show)

-- | What a database records about its migrations.
data Registry
    = UserVersionOnly !Int
    -- ^ No registry table yet: the first @user_version@ migrations count as applied.
    | RegistryRows !(Map Text MigrationState)
    -- ^ The state of each migration in the registry table, by id.
    deriving (Generic, Eq, Show)

data MigrationState
    = Applied
    | Pending
    | Skipped
    deriving (Generic, Eq, Show, Enum, Bounded)

-- | What the registry records about the migration at a one-based position, if anything.
registryState :: Registry -> Int -> Migration -> Maybe MigrationState
registryState registry position Migration{migrationId} =
    case registry of
        UserVersionOnly userVersion ->
            Applied <$ guard (position <= userVersion)
        RegistryRows rows ->
            Map.lookup migrationId rows
