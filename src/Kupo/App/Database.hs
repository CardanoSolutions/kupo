--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Kupo.App.Database
    (
      -- ** Queries
      -- *** Inputs
    deleteInputsQry
    , markInputsQry
    , pruneInputsQry
    , foldInputsQry
      -- ** Policies
    , foldPoliciesQry
      -- *** Checkpoints
    , listCheckpointsQry
    , listAncestorQry
      -- *** Binary Data
    , getBinaryDataQry
    , pruneBinaryDataQry
      -- *** Scripts
    , getScriptQry
      -- *** Rollback
    , selectMaxCheckpointQry
    , rollbackQryDeleteInputs
    , rollbackQryUpdateInputs
    , rollbackQryDeleteCheckpoints

      -- * Setup
    , DatabaseFile (..)
    , copyDatabase
    , newDBPool
    , newDBPoolFromFile

      -- * Internal
    , installIndexes
    , installIndex

      -- * Tracer
    , TraceDatabase (..)
    ) where

import Kupo.Prelude

import Kupo.App.Database.Types
    ( DBPool
    )
import Kupo.Control.MonadLog
    ( Tracer
    )
import Kupo.Data.Configuration
    ( DatabaseLocation
    , LongestRollback
    )

#if postgres
import Kupo.App.Database.Postgres
#else
import Kupo.App.Database.SQLite
#endif

newDBPool
    :: (Tracer IO TraceDatabase)
    -> Bool
    -> DatabaseLocation
    -> LongestRollback
    -> IO (DBPool IO)
newDBPool tr isReadOnly cfg longestRollback = do
    dbFile <- newDatabaseFile tr cfg
    newDBPoolFromFile tr isReadOnly dbFile longestRollback
