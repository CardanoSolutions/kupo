--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-} -- TODO not necessary for using?

module Kupo.App.FetchBlock.NodeToNode
    ( newFetchBlockClient
    , newOnDemandFetchBlockClient
    ) where

import Kupo.Prelude

import Kupo.App.Mailbox
    ( Mailbox
    , flushMailbox
    , putIntermittentMessage
    , putHighFrequencyMessage
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Data.Cardano
    ( Block
    , Point
    , Tip
    )
import Network.TypedProtocol
    ( Nat(..)
    )
import Ouroboros.Consensus.Block.Abstract
    ( Header
    , headerPoint
    )
import Ouroboros.Network.Protocol.BlockFetch.Client
    ( BlockFetchClientPipelined (..)
    , BlockFetchSender (..)
    )
import Ouroboros.Network.Protocol.BlockFetch.Type
    ( ChainRange (..)
    )
import qualified Data.List.NonEmpty as NE


ranges
    :: NE.NonEmpty (Tip, Header Block)
    -> [(Tip, ChainRange Point)]
ranges = map op . toList
  where
    op (tip, hdr) = (tip, ChainRange pt pt)
        where pt = headerPoint hdr

-- | Pipeline node-to-node block fetch client
-- Reads headers from the header mailbox
-- Requests corresponding blocks
-- Puts blocks from replies into block mailbox
newFetchBlockClient
    :: forall m block.
       MonadSTM m
    => Mailbox m (Tip, Header Block) (Tip, Point)
    -> Mailbox m (Tip, block) (Tip, Point)
    -> m (BlockFetchClientPipelined block Point m ())
newFetchBlockClient headerMailbox blockMailbox = do
    BlockFetchClientPipelined <$> fetchBlocks Zero <$> nextRanges
        where
            --nextRanges = ranges <$> atomically (flushHighFrequency headerMailbox)
            nextRanges = do
                msg <- atomically (flushMailbox headerMailbox)
                case msg of
                    Left headers ->
                        pure $ ranges headers
                    Right rollback -> do
                        atomically $ putIntermittentMessage blockMailbox rollback
                        nextRanges

            fetchBlocks _ [] = error "Should not happen"

            fetchBlocks n ((tip,r):rs) =
                SendMsgRequestRangePipelined
                    r
                    rs
                    (handleBlock tip)
                    (collectBlocks (Succ n))

            collectBlocks (Succ n) =
                CollectBlocksPipelined
                    Nothing
                    (\rs -> fetchBlocks n rs)

            handleBlock tip (Just b) [r] = do
                atomically $ putHighFrequencyMessage blockMailbox (tip, b)
                rs <- nextRanges
                pure (r:rs)

            handleBlock tip (Just b) rs = do
                atomically $ putHighFrequencyMessage blockMailbox (tip, b)
                pure rs

            handleBlock _ Nothing rs = do
                pure rs


-- | Build a node-to-node block-fetch client for individual on-demand requests.
-- The request queue contains the point to fetch and the callback to receive the
-- result. A point is requested as a singleton range; the callback receives
-- Nothing when the node has no block for that range.
newOnDemandFetchBlockClient
    :: forall m.
        MonadSTM m
    => TMVar m (Point, Maybe Block -> m ())
    -> m (BlockFetchClientPipelined Block Point m ())
newOnDemandFetchBlockClient requests =
    BlockFetchClientPipelined <$> fetchBlocks Zero <$> nextRequest
  where
    nextRequest = atomically $ takeTMVar requests

    fetchBlocks n (point, respond) =
        SendMsgRequestRangePipelined
            (ChainRange point point)
            (point,respond)
            (handleBlock respond)
            (collectBlocks (Succ n))

    collectBlocks (Succ n) =
        CollectBlocksPipelined
            Nothing
            (\r -> fetchBlocks n r)

    handleBlock respond mBlock _ = do
        respond mBlock
        nextRequest
