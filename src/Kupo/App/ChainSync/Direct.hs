--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.ChainSync.Direct
    ( mkChainSyncClient
    ) where

import Kupo.Prelude

import Control.Monad.Class.MonadSTM
    ( MonadSTM (..) )
import Kupo.App.Mailbox
    ( Mailbox, putHighFrequencyMessage, putIntermittentMessage )
import Kupo.Control.MonadThrow
    ( MonadThrow (..) )
import Kupo.Data.Cardano
    ( Point, Tip )
import Kupo.Data.ChainSync
    ( IntersectionNotFoundException (..) )
import Network.TypedProtocol.Pipelined
    ( Nat (..), natToInt )
import Ouroboros.Network.Block
    ( getTipSlotNo, pointSlot )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined (..)
    , ClientPipelinedStIdle (..)
    , ClientPipelinedStIntersect (..)
    , ClientStNext (..)
    )

-- | A simple pipeline chain-sync clients which offers maximum pipelining and
-- defer handling of requests to callbacks.
mkChainSyncClient
    :: forall m block.
        ( MonadThrow m
        , MonadSTM m
        )
    => Mailbox m (Tip, block) (Tip, Point)
    -> [Point]
    -> ChainSyncClientPipelined block Point Tip m ()
mkChainSyncClient mailbox pts =
    ChainSyncClientPipelined (pure $ SendMsgFindIntersect pts clientStIntersect)
  where
    clientStIntersect
        :: ClientPipelinedStIntersect block Point Tip m ()
    clientStIntersect = ClientPipelinedStIntersect
        { recvMsgIntersectFound = \_point _tip -> do
            pure $ clientStIdle Zero
        , recvMsgIntersectNotFound = \(getTipSlotNo -> tip) -> do
            let requestedPoints = pointSlot <$> pts
            throwIO $ IntersectionNotFound{requestedPoints,tip}
        }

    clientStIdle
        :: forall n. ()
        => Nat n
        -> ClientPipelinedStIdle n block Point Tip m ()
    clientStIdle n = do
        SendMsgRequestNextPipelined $ CollectResponse
            (guard (natToInt n < maxInFlight) $> pure (clientStIdle $ Succ n))
            (clientStNext n)

    clientStNext
        :: forall n. ()
        => Nat n
        -> ClientStNext n block Point Tip m ()
    clientStNext n =
        ClientStNext
            { recvMsgRollForward = \block tip -> do
                atomically (putHighFrequencyMessage mailbox (tip, block))
                pure (clientStIdle n)
            , recvMsgRollBackward = \point tip -> do
                atomically (putIntermittentMessage mailbox (tip, point))
                pure (clientStIdle n)
            }

-- | Maximum pipelining at any given time. No need to go too high here, it only
-- arms performance beyond a certain point.
--
-- TODO: Make this configurable as it depends on available machine's resources.
maxInFlight :: Int
maxInFlight = 100
