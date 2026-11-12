--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.Database.Migration.Command
    ( -- * Commands
      databaseStatus
    , databaseMigrate

      -- * Database access
    , withReadOnlyDatabase
    , withExclusiveDatabase

      -- * Exceptions
    , DatabaseCommandException (..)
    ) where

import Kupo.Prelude

import Control.Exception.Safe
    ( handleAny
    )
import Control.Tracer
    ( Tracer
    )
import Database.SQLite.Simple
    ( Connection
    , Error (..)
    , SQLError (..)
    , SQLOpenFlag (..)
    , execute_
    , withConnection'
    )
import Kupo.App.Database.Migration
    ( migrateDatabase
    , migrations
    , readStatus
    )
import Kupo.App.Database.Migration.Status
    ( renderStatus
    , statusExitCode
    )
import Kupo.App.Database.SQLite.Transaction
    ( withTransaction
    )
import Kupo.App.Database.Types
    ( ConnectionType (..)
    , TraceDatabase
    )
import Kupo.Control.MonadCatch
    ( MonadCatch (..)
    )
import Kupo.Control.MonadThrow
    ( MonadThrow (..)
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

import qualified Data.Text.IO as TIO

-- | @kupo db status@: print the state of every migration, without writing to the database. Exit
-- like Kupo would at startup: with 3 when a blocking migration is pending, with 1 when a newer
-- Kupo migrated the database.
databaseStatus
    :: FilePath
    -- ^ The working directory that holds the database.
    -> IO ExitCode
databaseStatus workdir =
    handleCommandException $ do
        report <- withReadOnlyDatabase workdir $ \conn ->
            readStatus conn migrations
        TIO.hPutStr stdout $ renderStatus report
        pure $ statusExitCode report

-- | @kupo db migrate@: apply every pending migration and repair, with the database locked. Exit
-- with 1 when a repair stays pending.
databaseMigrate
    :: Tracer IO TraceDatabase
    -- ^ Where to log progress.
    -> FilePath
    -- ^ The working directory that holds the database.
    -> Bool
    -- ^ Whether the operator confirms that the index matches @*@ from origin.
    -> IO ExitCode
databaseMigrate tr workdir isCompleteIndexConfirmed =
    handleCommandException $ do
        leftPending <- withExclusiveDatabase workdir $ \conn ->
            migrateDatabase tr conn isCompleteIndexConfirmed migrations
        pure $ if null leftPending then ExitSuccess else ExitFailure 1

-- | Open an existing database to read it. Fail with 'DatabaseLocked' while another connection
-- holds an exclusive lock on it.
withReadOnlyDatabase
    :: FilePath
    -- ^ The working directory that holds the database.
    -> (Connection -> IO a)
    -- ^ What to do with the connection.
    -> IO a
withReadOnlyDatabase workdir doWith =
    withExistingDatabase workdir SQLOpenReadOnly $ \conn ->
        handleJust busyError (\() -> throwIO DatabaseLocked) $ doWith conn

-- | Open an existing database with a lock that no other connection can share, until it closes.
-- Fail with 'DatabaseBusy' while another connection is open.
withExclusiveDatabase
    :: FilePath
    -- ^ The working directory that holds the database.
    -> (Connection -> IO a)
    -- ^ What to do with the connection.
    -> IO a
withExclusiveDatabase workdir doWith =
    withExistingDatabase workdir SQLOpenReadWrite $ \conn -> do
        execute_ conn "PRAGMA locking_mode = EXCLUSIVE"
        -- In exclusive locking mode, a connection keeps the locks it takes until it closes.
        handleJust busyError (\() -> throwIO DatabaseBusy) $
            withTransaction conn WriteOnly pass
        doWith conn

-- Never create the database: it holds nothing to report or to migrate.
withExistingDatabase :: FilePath -> SQLOpenFlag -> (Connection -> IO a) -> IO a
withExistingDatabase workdir mode doWith = do
    let databaseFile = workdir </> "kupo.sqlite3"
    unlessM (doesFileExist databaseFile) $
        throwIO $ DatabaseNotFound databaseFile
    withConnection' databaseFile [mode] doWith

busyError :: SQLError -> Maybe ()
busyError SQLError{sqlError} =
    guard $ sqlError `elem` [ErrorBusy, ErrorLocked]

-- Print why the command fails, and exit with 1. The database layer has logged a refusal.
handleCommandException :: IO ExitCode -> IO ExitCode
handleCommandException =
    handleAny $ \e -> do
        TIO.hPutStrLn stderr . toText $ displayException e
        pure $ ExitFailure 1

-- | Why a @kupo db@ command cannot run.
data DatabaseCommandException
    = DatabaseNotFound !FilePath
    | DatabaseBusy
    | DatabaseLocked
    deriving (Eq, Show)

instance Exception DatabaseCommandException where
    displayException = \case
        DatabaseNotFound databaseFile ->
            "There is no database at " <> databaseFile <> "."
        DatabaseBusy ->
            "Kupo appears to be running on this database; stop it first."
        DatabaseLocked ->
            "Another process, such as 'kupo db migrate', holds a lock on this database; \
            \try again once it is done."
