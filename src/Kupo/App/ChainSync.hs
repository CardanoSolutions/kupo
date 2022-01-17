--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.ChainSync
    ( Handler (..)
    , mkChainSyncClient
    , IntersectionNotFoundException (..)
    ) where

import Kupo.Prelude

import Kupo.Control.MonadThrow
    ( MonadThrow (..) )
import Kupo.Data.ChainSync
    ( IsBlock, Point (..), Tip (..) )
import Network.TypedProtocol.Pipelined
    ( Nat (..), natToInt )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined (..)
    , ClientPipelinedStIdle (..)
    , ClientPipelinedStIntersect (..)
    , ClientStNext (..)
    )

-- | A message handler for the chain-sync client. Messages are guaranteed (by
-- the protocol) to arrive in order.
data Handler m block = Handler
    { onRollBackward :: Point block -> m ()
    , onRollForward :: block -> m ()
    }

-- | A simple pipeline chain-sync clients which offers maximum pipelining and
-- defer handling of requests to callbacks.
mkChainSyncClient
    :: forall m block.
        ( MonadThrow m
        , IsBlock block
        )
    => Handler m block
    -> [Point block]
    -> ChainSyncClientPipelined block (Point block) (Tip block) m ()
mkChainSyncClient Handler{onRollBackward, onRollForward} points =
    ChainSyncClientPipelined (pure $ SendMsgFindIntersect points clientStIntersect)
  where
    clientStIntersect
        :: ClientPipelinedStIntersect block (Point block) (Tip block) m ()
    clientStIntersect = ClientPipelinedStIntersect
        { recvMsgIntersectFound = \_point _tip -> do
            -- TODO: Logging.
            pure $ clientStIdle Zero
        , recvMsgIntersectNotFound = \tip -> do
            throwIO $ IntersectionNotFoundException{points,tip}
        }

    clientStIdle
        :: forall n. ()
        => Nat n
        -> ClientPipelinedStIdle n block (Point block) (Tip block) m ()
    clientStIdle n = do
        SendMsgRequestNextPipelined $ CollectResponse
            (guard (natToInt n < maxInFlight) $> pure (clientStIdle $ Succ n))
            (clientStNext n)

    clientStNext
        :: forall n. ()
        => Nat n
        -> ClientStNext n block (Point block) (Tip block) m ()
    clientStNext n =
        ClientStNext
            { recvMsgRollForward = \block _tip ->
                onRollForward block $> clientStIdle n
            , recvMsgRollBackward = \point _tip ->
                onRollBackward point $> clientStIdle n
            }

maxInFlight :: Int
maxInFlight = 100

-- | Exception thrown when creating a chain-sync client from an invalid list of
-- points.
data IntersectionNotFoundException block = IntersectionNotFoundException
    { points :: [Point block]
        -- ^ Provided points for intersection.
    , tip :: Tip block
        -- ^ Current known tip of the chain.
    } deriving (Show)
instance (IsBlock block) => Exception (IntersectionNotFoundException block)
