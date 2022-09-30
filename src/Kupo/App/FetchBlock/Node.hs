--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}

module Kupo.App.FetchBlock.Node
    ( FetchBlockClient
    , newFetchBlockClient
    ) where

import Kupo.Prelude

import Control.Monad.Class.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Data.Cardano
    ( Point
    , Tip
    , pattern GenesisPoint
    )
import Kupo.Data.FetchBlock
    ( FetchBlockClient
    )
import Network.TypedProtocol.Pipelined
    ( N (..)
    )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined (..)
    , ClientPipelinedStIdle (..)
    , ClientPipelinedStIntersect (..)
    , ClientStNext (..)
    )

-- | Callback given to intermediate client states to ease signature / handling.
type BlockFetchHandler (m :: Type -> Type) block =
    Maybe block -> m (ClientPipelinedStIdle Z block Point Tip m ())

-- | A chain-sync client tailored to retrieving specific blocks, identified by their point on chain.
--
-- The client is idle most of the time, waiting for requests from an external actor, and provide
-- requested block in a timely manner as callback.
--
-- Note that the client fetches the block immediately *following* the given point. It is therefore up
-- to the caller to figure out what the previous point on chain of the block they're after is.
newFetchBlockClient
    :: forall m block.
        ( MonadSTM m
        )
    => m (ChainSyncClientPipelined block Point Tip m (), FetchBlockClient m block)
newFetchBlockClient = do
    reqVar <- newEmptyTMVarIO
    pure
        ( ChainSyncClientPipelined $ pure $ SendMsgFindIntersect [GenesisPoint] $
                ClientPipelinedStIntersect
                { recvMsgIntersectFound = \_point _tip -> do
                    clientStIdle $ atomically (readTMVar reqVar)
                , recvMsgIntersectNotFound = \_tip -> do
                    error "mkFetchBlockClient.recvMsgIntersectNotFound: absurd; \
                          \it's always possible to find an intersection against the origin."
                }
        , \point reply -> do
            atomically $ putTMVar reqVar (point, reply)
        )
  where
    clientStIdle
            :: m (Point, Maybe block -> m ())
            -> m (ClientPipelinedStIdle Z block Point Tip m ())
    clientStIdle await =
        await <&> \(point, reply) ->
            SendMsgFindIntersect [point] $ clientStIntersect $ \block -> do
                reply block
                clientStIdle await

    clientStIntersect
        :: BlockFetchHandler m block
        -> ClientPipelinedStIntersect block Point Tip m ()
    clientStIntersect reply =
        ClientPipelinedStIntersect
        { recvMsgIntersectFound = \_point _tip ->
            let st = clientStNextRollback reply
             in pure $ SendMsgRequestNext st (pure st)
        , recvMsgIntersectNotFound = \_tip ->
            reply Nothing
        }

    clientStNextRollback
        :: BlockFetchHandler m block
        -> ClientStNext Z block Point Tip m ()
    clientStNextRollback reply =
        ClientStNext
        { recvMsgRollForward = \_block _tip ->
            error "clientStNextRollback.recvMsgRollForward: absurd; \
                  \After a successful intersection, the node always sends a 'RollBackward' message."
        , recvMsgRollBackward = \_point _tip ->
            let st = clientStNextRollForward reply
             in pure $ SendMsgRequestNext st (pure st)
        }

    clientStNextRollForward
        :: BlockFetchHandler m block
        -> ClientStNext Z block Point Tip m ()
    clientStNextRollForward reply =
        ClientStNext
        { recvMsgRollForward = \block _tip ->
            reply (Just block)
        , recvMsgRollBackward = \_point _tip ->
            reply Nothing
        }
