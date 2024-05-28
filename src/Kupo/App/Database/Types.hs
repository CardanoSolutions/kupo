{-# LANGUAGE CPP #-}

module Kupo.App.Database.Types 
  (
    -- * Database DSL
      Database (..)
    , DBTransaction

    -- * Setup
    , ConnectionType (..)
    , DBPool (..)
    )
    where

import Control.Monad.Fail
    ()
import Database.SQLite.Simple
    ( Connection
    )
import Kupo.Data.Cardano
    ( BinaryData
    , DatumHash
    , Point
    , Script
    , ScriptHash
    , SlotNo (..)
    )
import Kupo.Data.Configuration
    ( DeferIndexesInstallation
    , LongestRollback (..)
    )
import Kupo.Data.Database
    ( SortDirection (..)
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

import Kupo.Prelude

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
        :: Set Pattern
        -> DBTransaction m Int

    , markInputs
        :: SlotNo
        -> Set Pattern
        -> DBTransaction m Int

    , pruneInputs
        :: DBTransaction m Int

    , foldInputs
        :: Pattern
        -> Range SlotNo
        -> StatusFlag
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
data DBPool m = DBPool {
  tryWithDatabase :: forall res. ConnectionType -> (Database m -> m res) -> m (Maybe res)
  , withDatabase :: forall res. ConnectionType -> (Database m -> m res) -> m res
  , withDatabaseExclusiveWriter :: forall a. DeferIndexesInstallation -> (Database m -> m a) -> m a
  , destroyResources :: m ()
}
