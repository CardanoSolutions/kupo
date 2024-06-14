--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.FetchTip.Node
    ( newFetchTipClient
    ) where

import Kupo.Prelude

import Control.Concurrent.Class.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Data.Cardano
    ( Point
    , Tip
    )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined (..)
    , ClientPipelinedStIdle (..)
    , ClientStNext (..)
    )

newFetchTipClient
    :: (MonadSTM m)
    => TMVar m Tip
    -> ChainSyncClientPipelined block Point Tip m ()
newFetchTipClient response =
    ChainSyncClientPipelined $ pure $
        SendMsgRequestNext
            (pure ())
            ClientStNext
                { recvMsgRollForward = \_header tip -> do
                    atomically $ putTMVar response tip
                    pure (SendMsgDone ())
                , recvMsgRollBackward = \_point tip -> do
                    atomically $ putTMVar response tip
                    pure (SendMsgDone ())
                }
