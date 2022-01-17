--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Prelude
    ( -- * relude
      module Relude
    ) where

import Relude hiding
    ( MVar
    , Nat
    , Option
    , STM
    , TMVar
    , TVar
    , atomically
    , catchSTM
    , isEmptyTMVar
    , mkWeakTMVar
    , modifyTVar'
    , newEmptyMVar
    , newEmptyTMVar
    , newEmptyTMVarIO
    , newMVar
    , newTMVar
    , newTMVarIO
    , newTVar
    , newTVarIO
    , putMVar
    , putTMVar
    , readMVar
    , readTMVar
    , readTVar
    , readTVarIO
    , swapMVar
    , swapTMVar
    , takeMVar
    , takeTMVar
    , throwSTM
    , traceM
    , tryPutMVar
    , tryPutTMVar
    , tryReadMVar
    , tryReadTMVar
    , tryTakeMVar
    , tryTakeTMVar
    , writeTVar
    )
