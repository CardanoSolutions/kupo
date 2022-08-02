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
    ( ForcedRollbackHandler (..), IntersectionNotFoundException (..) )
import Kupo.Data.Configuration
    ( maxInFlight )
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
    => TMVar m (Point, ForcedRollbackHandler m)
    -> Mailbox m (Tip, block) (Tip, Point)
    -> [Point]
    -> ChainSyncClientPipelined block Point Tip m ()
mkChainSyncClient forcedRollbackVar mailbox pts =
    ChainSyncClientPipelined $ pure $
        SendMsgFindIntersect pts (clientStIntersect Nothing)
  where
    clientStIntersect
        :: Maybe (Point, ForcedRollbackHandler m)
        -> ClientPipelinedStIntersect block Point Tip m ()
    clientStIntersect forcedRollback = ClientPipelinedStIntersect
        { recvMsgIntersectFound = \_point _tip -> do
            whenJust forcedRollback $ \(_, ForcedRollbackHandler{onSuccess}) ->
                onSuccess
            clientStIdle Zero
        , recvMsgIntersectNotFound = \(getTipSlotNo -> tip) -> do
            case forcedRollback of
                Just (pointSlot -> point, ForcedRollbackHandler{onFailure}) -> do
                    onFailure
                    throwIO $ ForcedIntersectionNotFound{point}
                Nothing ->
                    throwIO $ IntersectionNotFound{requestedPoints,tip}
                  where
                    requestedPoints = pointSlot <$> pts
        }

    clientStIdle
        :: forall n. ()
        => Nat n
        -> m (ClientPipelinedStIdle n block Point Tip m ())
    clientStIdle n = do
        atomically (tryTakeTMVar forcedRollbackVar) >>= \case
            Nothing ->
                pure $ SendMsgRequestNextPipelined $ CollectResponse
                    (guard (natToInt n < maxInFlight) $> clientStIdle (Succ n))
                    (clientStNext n)
            Just (pt, handler) ->
                pure (clientStWrapUp (pt, handler) n)

    -- | When receiving a 'forcedRollback' request, we simply collect and drop
    -- all messages in flight and look for the requested intersection. Note
    -- that, there's a chance here that the given point do not exist and that
    -- finding an intersection would fail. Handling this case is beyond the
    -- client scope and we mitigate this in two ways:
    --
    -- (1) The provider of the point is expected to check upfront that the point
    -- exists. This doesn't guarantee that the finding the intersection will
    -- succeed because for recent enough points, a chain switch / rollback could
    -- happen between the moment we check and the moment we look for an
    -- intersection. Yet, this should happen rarely;
    --
    -- (2) In case the intersection can't be found, an exception is thrown, we
    -- expect the whole client to be restarted with the latest known
    -- checkpoints, as normal. This means that in the worse case scenario, we
    -- interrupt the chain-sync client for a short bit and we continue back
    -- where we were at.
    clientStWrapUp
        :: forall n. ()
        => (Point, ForcedRollbackHandler m)
        -> Nat n
        -> ClientPipelinedStIdle n block Point Tip m ()
    clientStWrapUp (pt, handler) = \case
        Zero ->
            SendMsgFindIntersect [pt] (clientStIntersect (Just (pt, handler)))
        Succ n ->
            CollectResponse
                Nothing
                ( ClientStNext
                    { recvMsgRollForward = \_block _tip -> do
                        pure $ clientStWrapUp (pt, handler) n
                    , recvMsgRollBackward = \_point _tip -> do
                        pure $ clientStWrapUp (pt, handler) n
                    }
                )

    clientStNext
        :: forall n. ()
        => Nat n
        -> ClientStNext n block Point Tip m ()
    clientStNext n =
        ClientStNext
            { recvMsgRollForward = \block tip -> do
                atomically (putHighFrequencyMessage mailbox (tip, block))
                clientStIdle n
            , recvMsgRollBackward = \point tip -> do
                atomically (putIntermittentMessage mailbox (tip, point))
                clientStIdle n
            }
