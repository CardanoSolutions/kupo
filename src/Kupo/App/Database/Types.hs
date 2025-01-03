{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Kupo.App.Database.Types
  (
    -- * Database DSL
      Database (..)
    , DBTransaction

    -- * Setup
    , ConnectionType (..)
    , DBPool (..)

    -- * Tracer
    , TraceDatabase (..)
    , TraceConnection (..)
    )
    where


import Kupo.Prelude

import Data.Severity
    ( HasSeverityAnnotation
    , Severity (..)
    )
import Database.SQLite.Simple
    ( Connection
    )
import Kupo.Control.MonadLog
    ( HasSeverityAnnotation (..)
    )
import Kupo.Control.MonadTime
    ( DiffTime
    )
import Kupo.Data.Cardano
    ( BinaryData
    , DatumHash
    , InputIndex
    , Point
    , Script
    , ScriptHash
    , SlotNo (..)
    , TransactionId
    )
import Kupo.Data.Configuration
    ( DeferIndexesInstallation
    , LongestRollback (..)
    )
import Kupo.Data.Database
    ( SortDirection (..)
    )
import Kupo.Data.Http.ReferenceFlag
    ( ReferenceFlag (..)
    )
import Kupo.Data.Http.SlotRange
    ( Range (..)
    )
import Kupo.Data.Http.StatusFlag
    ( StatusFlag (..)
    )
import Kupo.Data.Pattern
    ( Pattern (..)
    , Result
    )

import Control.Monad.Fail
    ()

import qualified Kupo.Data.Database as DB

data ConnectionType = ReadOnly | ReadWrite | WriteOnly
    deriving (Generic, Eq, Show)

instance ToJSON ConnectionType where
    toEncoding = defaultGenericToEncoding

type family DBTransaction (m :: Type -> Type) :: (Type -> Type) where
    DBTransaction IO = ReaderT Connection IO

data Database (m :: Type -> Type) = Database
    { insertInputs
        :: [DB.Input]
        -> DBTransaction m ()

    , deleteInputs
        :: [Pattern]
        -> DBTransaction m Int

    , markInputs
        :: (TransactionId, SlotNo)
        -> [(Pattern, InputIndex, Maybe BinaryData)]
        -> DBTransaction m Int

    , pruneInputs
        :: DBTransaction m Int

    , foldInputs
        :: Pattern
        -> Range SlotNo
        -> StatusFlag
        -> ReferenceFlag
        -> SortDirection
        -> (Result -> m ())
        -> DBTransaction m ()

    , countInputs
        :: Pattern
        -> DBTransaction m Integer

    , foldPolicies
        :: Pattern
        -> (DB.Policy -> m ())
        -> DBTransaction m ()

    , countPolicies
        :: Pattern
        -> DBTransaction m Integer

    , insertPolicies
        :: Set DB.Policy
        -> DBTransaction m ()

    , insertCheckpoints
        :: [Point]
        -> DBTransaction m ()

    , listCheckpointsDesc
        :: DBTransaction m [Point]

    , listAncestorsDesc
        :: SlotNo
        -> Int64 -- Number of ancestors to retrieve
        -> DBTransaction m [Point]

    , insertPatterns
        :: Set Pattern
        -> DBTransaction m ()

    , deletePattern
        :: Pattern
        -> DBTransaction m Int

    , listPatterns
        :: DBTransaction m (Set Pattern)

    , insertBinaryData
        :: [DB.BinaryData]
        -> DBTransaction m ()

    , getBinaryData
        :: DatumHash
        -> DBTransaction m (Maybe BinaryData)

    , pruneBinaryData
        :: DBTransaction m Int

    , insertScripts
        :: [DB.ScriptReference]
        -> DBTransaction m ()

    , getScript
        :: ScriptHash
        -> DBTransaction m (Maybe Script)

    , rollbackTo
        :: SlotNo
        -> DBTransaction m (Maybe Point)

    , optimize
        :: DBTransaction  m ()

    , runTransaction
        :: forall a. ()
        => DBTransaction m a
        -> m a

    , longestRollback
        :: LongestRollback

    , close
        :: m ()
    }

-- | An abstraction of a database connection pool that provides DB connections based on `ConnectionType` and exclusivity
-- Creating this abstraction allows SQLite and PostgreSQL pools to be implemented differently.
-- A `DBPool` should ensure, on its creation, that any necessary DB setup (e.g., creation of a DB file) is completed
-- on instantiation of the `DBPool`.
data DBPool m = DBPool
    { tryWithDatabase
        :: forall res. ()
        => ConnectionType
        -> (Database m -> m res)
        -> m (Maybe res)

    , withDatabaseBlocking
        :: forall res. ()
        => ConnectionType
        -> (Database m -> m res)
        -> m res

    , withDatabaseExclusiveWriter
        :: forall a. ()
        => DeferIndexesInstallation
        -> (Database m -> m a)
        -> m a

    , maxConcurrentReaders
        :: Int

    , maxConcurrentWriters
        :: Int

    , destroyResources
        :: m ()
    }


--
-- Tracer
--

data TraceDatabase where
    DatabaseCreateNew
        :: { filePath :: FilePath }
        -> TraceDatabase
    DatabaseConnection
        :: { message :: TraceConnection }
        -> TraceDatabase
    DatabaseCurrentVersion
        :: { currentVersion :: Int }
        -> TraceDatabase
    DatabaseNoMigrationNeeded
        :: TraceDatabase
    DatabaseRunningMigration
        :: { from :: Int, to :: Int }
        -> TraceDatabase
    DatabaseCreateIndex
        :: { newIndex :: Text }
        -> TraceDatabase
    DatabaseIndexAlreadyExists
        :: { index :: Text }
        -> TraceDatabase
    DatabaseDeferIndexes
        :: { warning :: Text }
        -> TraceDatabase
    DatabaseRunningInMemory
        :: TraceDatabase
    DatabasePathMustBeDirectory
        :: { hint :: Text }
        -> TraceDatabase
    DatabaseMustBeLocal
        :: { errorMessage :: Text }
        -> TraceDatabase
    DatabaseCloneSourceDatabase
        :: TraceDatabase
    DatabaseCleanupOldData
        :: { table :: Text }
        -> TraceDatabase
    DatabaseImportTable
        :: { table :: Text, pattern :: Text }
        -> TraceDatabase
    DatabaseImported
        :: { rows :: Integer }
        -> TraceDatabase
    DatabaseRemoveIncompleteCopy
        :: { filePath :: FilePath }
        -> TraceDatabase
    DatabaseCopyFinalize
        :: TraceDatabase
    DatabaseDebug
        :: Text
        -> TraceDatabase
    deriving stock (Generic, Show)

instance ToJSON TraceDatabase where
    toEncoding =
        defaultGenericToEncoding

instance HasSeverityAnnotation TraceDatabase where
    getSeverityAnnotation = \case
        DatabaseCreateNew{}            -> Notice
        DatabaseConnection { message } -> getSeverityAnnotation message
        DatabaseCurrentVersion{}       -> Info
        DatabaseNoMigrationNeeded{}    -> Debug
        DatabaseRunningMigration{}     -> Notice
        DatabaseRunningInMemory{}      -> Warning
        DatabaseCreateIndex{}          -> Notice
        DatabaseIndexAlreadyExists{}   -> Debug
        DatabasePathMustBeDirectory{}  -> Error
        DatabaseMustBeLocal{}          -> Error
        DatabaseDeferIndexes{}         -> Warning
        DatabaseCloneSourceDatabase{}  -> Notice
        DatabaseCleanupOldData{}       -> Info
        DatabaseImportTable{}          -> Notice
        DatabaseImported{}             -> Info
        DatabaseRemoveIncompleteCopy{} -> Notice
        DatabaseCopyFinalize{}         -> Notice
        DatabaseDebug{}                -> Warning

data TraceConnection where
    ConnectionCreateShortLived
        :: { mode :: ConnectionType }
        -> TraceConnection
    ConnectionDestroyShortLived
        :: { mode :: ConnectionType }
        -> TraceConnection
    ConnectionLocked
        :: { attempts :: Word, retryingIn :: DiffTime }
        -> TraceConnection
    ConnectionBusy
        :: { attempts :: Word, retryingIn :: DiffTime }
        -> TraceConnection
    ConnectionBeginQuery
        :: { beginQuery :: LText }
        -> TraceConnection
    ConnectionExitQuery
        :: { exitQuery :: LText }
        -> TraceConnection
    ConnectionCreateTemporaryIndex
        :: { newTemporaryIndex :: Text }
        -> TraceConnection
    ConnectionCreatedTemporaryIndex
        :: { newTemporaryIndex :: Text }
        -> TraceConnection
    ConnectionRemoveTemporaryIndex
        :: { indexName :: Text }
        -> TraceConnection
    ConnectionRemoveIndex
        :: { indexName :: Text }
        -> TraceConnection
    ConnectionFailedPragma
        :: { pragma :: Text }
        -> TraceConnection
    ConnectionFailedToOpenDatabase
        :: { hint :: Text }
        -> TraceConnection
    deriving stock (Generic, Show)

instance ToJSON TraceConnection where
    toEncoding =
        defaultGenericToEncoding

instance HasSeverityAnnotation TraceConnection where
    getSeverityAnnotation = \case
        ConnectionCreateShortLived{} -> Debug
        ConnectionDestroyShortLived{} -> Debug
        ConnectionLocked{attempts, retryingIn}
            | retryingIn * fromIntegral attempts > 60  -> Warning
        ConnectionLocked{} -> Debug
        ConnectionBusy{attempts, retryingIn}
            | retryingIn * fromIntegral attempts > 60 -> Warning
        ConnectionBusy{} -> Debug
        ConnectionBeginQuery{beginQuery}
            | beginQuery == "PRAGMA optimize" -> Notice
        ConnectionBeginQuery{} -> Debug
        ConnectionExitQuery{exitQuery}
            | exitQuery == "PRAGMA optimize" -> Notice
        ConnectionExitQuery{} -> Debug
        ConnectionCreateTemporaryIndex{} -> Debug
        ConnectionCreatedTemporaryIndex{} -> Debug
        ConnectionRemoveTemporaryIndex{} -> Debug
        ConnectionRemoveIndex{} -> Warning
        ConnectionFailedPragma{} -> Warning
        ConnectionFailedToOpenDatabase{} -> Error
