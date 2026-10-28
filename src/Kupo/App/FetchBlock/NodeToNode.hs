--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-} -- TODO not necessary for using?

module Kupo.App.FetchBlock.NodeToNode
    ( newFetchBlockClient
    ) where

import Kupo.Prelude

import Kupo.App.Mailbox
    ( Mailbox
    , flushHighFrequency
    , putHighFrequencyMessage
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Data.Cardano
    ( Point
    , Tip
    , getPoint
    )
import Network.TypedProtocol
    ( Nat(..)
    )
import Ouroboros.Consensus.Block.Abstract
    ( Header
    )
import Ouroboros.Network.Protocol.BlockFetch.Client
    ( BlockFetchClientPipelined (..)
    , BlockFetchSender (..)
    )
import Ouroboros.Network.Protocol.BlockFetch.Type
    ( ChainRange (..)
    )
import qualified Data.List.NonEmpty as NE


rangesFromHeaders :: [(Tip,Header block)] -> [(Tip,ChainRange Point)]
rangesFromHeaders _ = []

-- | Pipeline node-to-node block fetch client
-- Reads headers from the header mailbox
-- Requests corresponding blocks
-- Puts blocks from replies into block mailbox

newFetchBlockClient
    :: forall m block.
       MonadSTM m
    => Mailbox m (Tip, Header block) (Tip, Point)
    -> Mailbox m (Tip, block) (Tip, Point)
    -> m (BlockFetchClientPipelined block Point m ())
newFetchBlockClient headerMailbox blockMailbox = do
    ranges <- nextRanges
    pure $ BlockFetchClientPipelined $ fetchBlocks ranges Zero
        where
            nextRanges = do
                messages <- atomically (flushHighFrequency headerMailbox)
                pure $ rangesFromHeaders (NE.toList messages)

            fetchBlocks [] _ = error "Should not happen"

            fetchBlocks ((tip,r):rs) n =
                SendMsgRequestRangePipelined
                    r
                    rs
                    (handleBlock tip)
                    (collectBlocks (Succ n))

            collectBlocks (Succ n) =
                CollectBlocksPipelined
                    Nothing
                    (\rs -> fetchBlocks rs n)

            handleBlock tip (Just b) rs = do
                atomically $ putHighFrequencyMessage blockMailbox (tip, b)
                pure rs

            handleBlock _ Nothing rs = do
                ranges <- nextRanges
                pure (ranges <> rs)
