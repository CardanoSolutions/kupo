--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Kupo.App.Database
    ( -- * Database DSL
    Database (..)
    , DBPool
        ( DBPool
        , tryWithDatabase
        , withDatabaseBlocking
        , withDatabaseExclusiveWriter
        , maxConcurrentReaders
        , maxConcurrentWriters
        )
    , withDBPool
    , DBTransaction
    , ConnectionType (..)
    
      -- ** Queries
      -- *** Inputs
    , deleteInputsQry
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
    , copyDatabase

      -- * Internal
    , installIndexes
    , installIndex

      -- * Tracer
    , TraceDatabase (..)

      -- * Test helpers
    , withTestDatabase
    ) where

import Kupo.App.Database.Types

#if postgres
import Kupo.App.Database.Postgres
#else
import Kupo.App.Database.SQLite
#endif
