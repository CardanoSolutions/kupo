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
    , migrationStateToText
    , registryState

      -- * Startup gate
    , MigrationRequiredException (..)
    , migrationRequiredHint
    , exitCodeMigrationRequired
    , UnknownMigrationsException (..)
    , UnknownMigration (..)
    , unknownMigrationsHint
    , describeUnknownMigration
    , describeMigration
    ) where

import Kupo.Prelude

import Kupo.App.Database.Migration.Definition
    ( BatchRepair (..)
    , Migration (..)
    , MigrationAction (..)
    , MigrationKind (..)
    , migrationKind
    , migrationKindToText
    )

import qualified Data.Map as Map
import qualified Data.Text as T

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
    , planUnknown = unknownIds registry
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

    unknownIds = \case
        UserVersionOnly{} -> []
        RegistryRows rows -> filter (`notElem` (migrationId <$> knownMigrations)) $ Map.keys rows

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
    , planUnknown :: ![Text]
    -- ^ Registry ids this version does not know: a newer Kupo migrated the database.
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

migrationStateToText :: MigrationState -> Text
migrationStateToText = \case
    Applied -> "applied"
    Pending -> "pending"
    Skipped -> "skipped"

-- | Kupo refuses to start: the listed migrations, rendered by 'describeMigration', need
-- @kupo db migrate@.
newtype MigrationRequiredException = MigrationRequiredException [Text]
    deriving (Show)

instance Exception MigrationRequiredException where
    displayException (MigrationRequiredException pendingMigrations) =
        toString . T.unlines $ migrationRequiredHint : "Pending migrations:" : fmap ("  " <>) pendingMigrations

migrationRequiredHint :: Text
migrationRequiredHint =
    "The database needs migrations that Kupo does not apply on its own. \
    \Run 'kupo db migrate' to apply them, then start Kupo again."

-- | Exit code when the database needs @kupo db migrate@ before Kupo can start.
exitCodeMigrationRequired :: Int
exitCodeMigrationRequired = 3

-- | Kupo refuses the database: its registry lists migrations that this version does not know.
newtype UnknownMigrationsException = UnknownMigrationsException [UnknownMigration]
    deriving (Eq, Show)

instance Exception UnknownMigrationsException where
    displayException (UnknownMigrationsException unknownMigrations) =
        toString . T.unlines $ unknownMigrationsHint : "Unknown migrations:"
            : fmap (("  " <>) . describeUnknownMigration) unknownMigrations

-- | A registry row that this version of Kupo does not know.
data UnknownMigration = UnknownMigration
    { unknownId :: !Text
    , unknownKupoVersion :: !(Maybe Text)
    -- ^ The version of Kupo that recorded the row, if the row holds one.
    }
    deriving (Generic, Eq, Show)

unknownMigrationsHint :: Text
unknownMigrationsHint =
    "This database was migrated by a newer Kupo. Run that version, or a later one, instead."

-- | One line naming an unknown migration, and the version of Kupo that recorded it when known.
describeUnknownMigration
    :: UnknownMigration
    -- ^ The migration to name.
    -> Text
describeUnknownMigration UnknownMigration{unknownId, unknownKupoVersion} =
    unknownId <> maybe "" (\kupoVersion -> " (recorded by Kupo " <> kupoVersion <> ")") unknownKupoVersion

-- | One line naming a migration: its id, kind and summary.
describeMigration
    :: Migration
    -- ^ The migration to name.
    -> Text
describeMigration migration@Migration{migrationId, migrationSummary} =
    migrationId <> " (" <> migrationKindToText (migrationKind migration) <> "): " <> migrationSummary

-- | What the registry records about the migration at a one-based position, if anything.
registryState :: Registry -> Int -> Migration -> Maybe MigrationState
registryState registry position Migration{migrationId} =
    case registry of
        UserVersionOnly userVersion ->
            Applied <$ guard (position <= userVersion)
        RegistryRows rows ->
            Map.lookup migrationId rows
