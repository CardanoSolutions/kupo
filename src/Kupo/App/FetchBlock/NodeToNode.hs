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
    -> NE.NonEmpty (Tip, ChainRange Point)
ranges hs = (tip, ChainRange pt pt) :| []
  where
    (tip,hdr) = NE.head hs
    pt = headerPoint hdr

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
            nextRanges = ranges <$> atomically (flushHighFrequency headerMailbox)

            fetchBlocks n rs =
                SendMsgRequestRangePipelined
                    r
                    rest
                    (handleBlock tip)
                    (collectBlocks (Succ n))
                where
                    (tip,r) = NE.head rs
                    rest = NE.fromList (NE.tail rs)

            collectBlocks (Succ n) =
                CollectBlocksPipelined
                    Nothing
                    (\rs -> fetchBlocks n rs)

            handleBlock tip (Just b) rs = do
                atomically $ putHighFrequencyMessage blockMailbox (tip, b)
                pure rs

            handleBlock _ Nothing rs = do
                nextRs <- nextRanges
                pure (rs <> nextRs)
