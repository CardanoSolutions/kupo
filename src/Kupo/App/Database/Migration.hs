--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.Database.Migration
    ( -- * Migrations
      migrations

      -- * Runner
    , runMigrations
    , checkMigrations

      -- * Exceptions
    , UnexpectedUserVersionException (..)
    , UnexpectedMigrationStateException (..)
    ) where

import Kupo.Prelude

import Control.Exception.Safe
    ( withException
    )
import Control.Monad
    ( foldM_
    )
import Control.Tracer
    ( Tracer
    , traceWith
    )
import Database.SQLite.Simple
    ( Connection
    , Only (..)
    , Query (..)
    , connectionHandle
    , execute
    , executeMany
    , execute_
    , nextRow
    , query
    , query_
    , withStatement
    )
import Kupo.App.Database.Migration.Definition
    ( Migration (..)
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
    , describeMigration
    , migrationRequiredHint
    , migrationStateToText
    , planMigrations
    )
import Kupo.App.Database.SQLite.Transaction
    ( withTransaction
    )
import Kupo.App.Database.Types
    ( ConnectionType (..)
    , TraceDatabase (..)
    )
import Kupo.Control.MonadThrow
    ( MonadThrow (..)
    )

import qualified Database.SQLite3 as Sqlite3
import qualified Kupo.App.Database.Migration.V1_0_0_001 as V1_0_0_001
import qualified Kupo.App.Database.Migration.V1_0_0_002 as V1_0_0_002
import qualified Kupo.App.Database.Migration.V1_0_0_beta_001 as V1_0_0_beta_001
import qualified Kupo.App.Database.Migration.V1_0_1_001 as V1_0_1_001
import qualified Kupo.App.Database.Migration.V2_0_0_beta_001 as V2_0_0_beta_001
import qualified Kupo.App.Database.Migration.V2_10_0_001 as V2_10_0_001
import qualified Kupo.App.Database.Migration.V2_12_0_001 as V2_12_0_001
import qualified Kupo.App.Database.Migration.V2_1_0_001 as V2_1_0_001
import qualified Kupo.App.Database.Migration.V2_1_0_002 as V2_1_0_002
import qualified Kupo.App.Database.Migration.V2_1_0_003 as V2_1_0_003
import qualified Kupo.App.Database.Migration.V2_2_0_001 as V2_2_0_001
import qualified Kupo.Version as Kupo

--
-- Migrations
--

-- | Every migration, oldest first. Append only: the position of an entry is its @user_version@.
migrations :: [Migration]
migrations =
    [ V1_0_0_beta_001.migration
    , V1_0_0_001.migration
    , V1_0_0_002.migration
    , V1_0_1_001.migration
    , V2_0_0_beta_001.migration
    , V2_1_0_001.migration
    , V2_1_0_002.migration
    , V2_1_0_003.migration
    , V2_2_0_001.migration
    , V2_10_0_001.migration
    , V2_12_0_001.migration
    ]

-- | Run, in order, what 'planMigrations' allows. Then throw 'MigrationRequiredException' if a
-- 'Blocking' migration is pending, and warn about pending repairs.
runMigrations
    :: Tracer IO TraceDatabase
    -- ^ Where to log progress.
    -> Connection
    -- ^ A read-write connection to the database.
    -> [Migration]
    -- ^ All known migrations, oldest first.
    -> IO ()
runMigrations tr conn knownMigrations =
    applyMigrations tr conn Startup knownMigrations >>= enforceGate tr

-- | Plan like 'runMigrations' without writing anything, for read-only replicas.
checkMigrations
    :: Tracer IO TraceDatabase
    -- ^ Where to log the outcome.
    -> Connection
    -- ^ A read-only connection to the database.
    -> [Migration]
    -- ^ All known migrations, oldest first.
    -> IO ()
checkMigrations tr conn knownMigrations = do
    (currentVersion, plan@Plan{planSteps}) <- withTransaction conn ReadOnly $ do
        userVersion <- databaseVersion conn
        (userVersion,) <$> readPlan conn Startup userVersion knownMigrations
    traceWith tr (DatabaseCurrentVersion currentVersion)
    unless (null planSteps) $
        traceWith tr DatabaseReplicaNotMigrated
            { unappliedMigrations = describeMigration . stepMigration <$> planSteps
            , hint = "The writer has not run these migrations yet. The replica carries on meanwhile."
            }
    enforceGate tr plan

-- Run what the plan allows.
applyMigrations :: Tracer IO TraceDatabase -> Connection -> Caller -> [Migration] -> IO Plan
applyMigrations tr conn caller knownMigrations = do
    currentVersion <- databaseVersion conn
    traceWith tr (DatabaseCurrentVersion currentVersion)
    plan@Plan{planSteps} <- readPlan conn caller currentVersion knownMigrations
    if null planSteps then
        traceWith tr DatabaseNoMigrationNeeded
    else do
        let targetVersion = foldl' max currentVersion (stepPosition <$> planSteps)
        traceWith tr $ DatabaseRunningMigration currentVersion targetVersion
        hasRegistry <- hasTable conn "migrations"
        foldM_ (runPlannedStep tr conn knownMigrations currentVersion) hasRegistry planSteps
    pure plan

-- Read the database state once, before anything runs.
readPlan :: Connection -> Caller -> Int -> [Migration] -> IO Plan
readPlan conn caller currentVersion knownMigrations = do
    databaseState <- readDatabaseState conn
    registry <- readRegistry conn currentVersion
    pure $ planMigrations caller databaseState registry knownMigrations

-- Warn about pending repairs, and refuse to go past a pending 'Blocking' migration.
enforceGate :: Tracer IO TraceDatabase -> Plan -> IO ()
enforceGate tr Plan{planBlocked, planPendingRepairs} = do
    unless (null planPendingRepairs) $
        traceWith tr DatabasePendingRepairs
            { pendingRepairs = describeMigration <$> planPendingRepairs
            , hint = "Kupo runs, but these data repairs are pending. Run 'kupo db migrate' to apply them."
            }
    unless (null planBlocked) $ do
        let pendingMigrations = describeMigration <$> planBlocked
        traceWith tr DatabaseMigrationRequired { pendingMigrations, hint = migrationRequiredHint }
        throwIO $ MigrationRequiredException pendingMigrations

databaseVersion :: Connection -> IO Int
databaseVersion conn =
    withStatement conn "PRAGMA user_version" $ \stmt -> do
        nextRow stmt >>= \case
            Just (Only version) ->
                pure version
            _unexpectedVersion ->
                throwIO UnexpectedUserVersion

-- Run a step, its registry row and the new @user_version@ in one transaction. Registry rows start
-- with the migration that creates the table, which also backfills every earlier id.
runPlannedStep :: Tracer IO TraceDatabase -> Connection -> [Migration] -> Int -> Bool -> PlannedStep -> IO Bool
runPlannedStep tr conn knownMigrations currentVersion hasRegistry PlannedStep{stepPosition, stepMigration, stepAction} =
    traceFailure tr (DatabaseMigrationFailed $ migrationId stepMigration) . withTransaction conn ReadWrite $ do
        execute_ conn $ Query $ "PRAGMA user_version = " <> show (max currentVersion stepPosition) <> ";"
        case stepAction of
            Apply sql -> do
                -- SQLite splits the script into statements itself, so a statement may hold a @;@.
                Sqlite3.exec (connectionHandle conn) $ decodeUtf8 sql
                hasRegistryNow <- if hasRegistry then pure True else hasTable conn "migrations"
                when (hasRegistryNow && not hasRegistry) $
                    registerBackfilled conn $ take (stepPosition - 1) knownMigrations
                when hasRegistryNow $
                    registerApplied conn stepMigration
                pure hasRegistryNow
            RegisterPending -> do
                registerRepair conn Pending stepMigration
                pure hasRegistry
            RegisterSkipped -> do
                registerRepair conn Skipped stepMigration
                pure hasRegistry

-- Log why an action fails, then rethrow.
traceFailure :: Tracer IO TraceDatabase -> (Text -> TraceDatabase) -> IO a -> IO a
traceFailure tr toEvent action =
    action `withException` \(failure :: SomeException) ->
        traceWith tr . toEvent . toText $ displayException failure

--
-- Registry
--

readRegistry :: Connection -> Int -> IO Registry
readRegistry conn currentVersion = do
    hasRegistry <- hasTable conn "migrations"
    if hasRegistry then do
        rows <- query_ conn "SELECT id, state FROM migrations"
        RegistryRows . fromList <$> traverse decodeRow rows
    else
        pure (UserVersionOnly currentVersion)
  where
    decodeRow (migrationId, migrationState) =
        (migrationId,) <$> decodeMigrationState migrationId migrationState

decodeMigrationState :: Text -> Text -> IO MigrationState
decodeMigrationState migrationId = \case
    "applied" -> pure Applied
    "pending" -> pure Pending
    "skipped" -> pure Skipped
    migrationState -> throwIO $ UnexpectedMigrationState migrationId migrationState

registerApplied :: Connection -> Migration -> IO ()
registerApplied conn Migration{migrationId} =
    execute conn
        ( "INSERT INTO migrations (id, state, kupo_version, registered_at, applied_at) \
          \VALUES (?, ?, ?, " <> unixNow <> ", " <> unixNow <> ")"
        )
        (migrationId, migrationStateToText Applied, Kupo.version)

-- Migrations applied before the registry existed: when, and by which version, is unknown.
registerBackfilled :: Connection -> [Migration] -> IO ()
registerBackfilled conn backfilled =
    executeMany conn
        ("INSERT INTO migrations (id, state, registered_at) VALUES (?, ?, " <> unixNow <> ")")
        [ (migrationId, migrationStateToText Applied) | Migration{migrationId} <- backfilled ]

-- The cutoff is the latest checkpoint at registration, so a fresh database records none.
registerRepair :: Connection -> MigrationState -> Migration -> IO ()
registerRepair conn migrationState Migration{migrationId} =
    execute conn
        ( "INSERT INTO migrations (id, state, cutoff_slot, kupo_version, registered_at) \
          \VALUES (?, ?, (SELECT MAX(slot_no) FROM checkpoints), ?, " <> unixNow <> ")"
        )
        (migrationId, migrationStateToText migrationState, Kupo.version)

unixNow :: Query
unixNow =
    "CAST(strftime('%s', 'now') AS INTEGER)"

-- A database without checkpoints holds no data that a repair could fix.
readDatabaseState :: Connection -> IO DatabaseState
readDatabaseState conn = do
    hasCheckpoints <- hasTable conn "checkpoints"
    if hasCheckpoints then do
        rows <- query_ conn "SELECT 1 FROM checkpoints LIMIT 1"
        pure $ if null (rows :: [Only Int]) then FreshDatabase else ExistingDatabase
    else
        pure FreshDatabase

hasTable :: Connection -> Text -> IO Bool
hasTable conn name = do
    rows <- query conn "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?" (Only name)
    pure . not $ null (rows :: [Only Int])

--
-- Exceptions
--

-- | Somehow, a 'PRAGMA user_version' didn't yield a number but, either nothing
-- or something else?
data UnexpectedUserVersionException
    = UnexpectedUserVersion
    deriving Show
instance Exception UnexpectedUserVersionException

-- | The registry holds a migration state this version of Kupo does not know.
data UnexpectedMigrationStateException
    = UnexpectedMigrationState !Text !Text
    deriving Show

instance Exception UnexpectedMigrationStateException where
    displayException (UnexpectedMigrationState migrationId migrationState) =
        "The registry records migration " <> toString migrationId <> " as '" <> toString migrationState
        <> "', a state that this version of Kupo does not know."
