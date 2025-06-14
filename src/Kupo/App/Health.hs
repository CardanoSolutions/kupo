--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.Health
    ( readHealth
    , recordCheckpoint
    , initializeHealth
    , connectionStatusToggle
    ) where

import Kupo.Prelude

import Data.Time
    ( UTCTime
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Data.Cardano
    ( Point
    )
import Kupo.Data.Cardano.SlotNo
    ( SlotNo
    )
import Kupo.Data.Health
    ( ConnectionStatus (..)
    , Health (..)
    )

-- | Safe and limited accessor to the health
readHealth
    :: forall m.
        ( MonadSTM m
        )
    => TVar m Health
    -> m Health
readHealth =
    readTVarIO

-- | Creates an isolated effectful function to toggle a health connection status.
connectionStatusToggle
    :: forall m.
        ( MonadSTM m
        )
    => TVar m Health
    -> ConnectionStatusToggle m
connectionStatusToggle health =
    ConnectionStatusToggle
        { toggleConnected = setConnectionStatus Connected
        , toggleDisconnected = setConnectionStatus Disconnected
        }
  where
    setConnectionStatus connectionStatus =
        atomically (modifyTVar' health (\h -> h { connectionStatus }))

-- | A safe setter for the most recent tip and checkpoint.
recordCheckpoint
    :: forall m.
        ( MonadSTM m
        )
    => TVar m Health
    -> UTCTime
    -> SlotNo
    -> Maybe Point
    -> m ()
recordCheckpoint health (Just -> mostRecentClockTick) (Just -> mostRecentNodeTip) mostRecentCheckpoint =
    atomically $ modifyTVar' health $ \h -> h
        { mostRecentNodeTip
        , mostRecentCheckpoint
        , mostRecentClockTick
        }

-- | A safe setter for the most recent checkpoint.
initializeHealth
    :: forall m.
        ( MonadSTM m
        )
    => TVar m Health
    -> Maybe Point
    -> m ()
initializeHealth health mostRecentCheckpoint' =
    atomically $ modifyTVar' health $ \h -> h
        { mostRecentCheckpoint =
            max (mostRecentCheckpoint h) mostRecentCheckpoint'
        }
