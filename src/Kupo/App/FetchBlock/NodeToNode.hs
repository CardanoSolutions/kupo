--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-} -- TODO not necessary for using?

module Kupo.App.FetchBlock.NodeToNode
    ( FetchBlockClient
    , newFetchBlockClient
    ) where

import Kupo.Prelude

import Kupo.App.Mailbox
    ( Mailbox
    , putHighFrequencyMessage
    )
import Kupo.Control.MonadSTM
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
import Network.TypedProtocol
    ( N (..)
    )
import Ouroboros.Network.Protocol.BlockFetch.Client
    ( BlockFetchClientPipelined (..)
    )

-- | Pipeline node-to-node block fetch client
newFetchBlockClient
    :: forall m block.
        ( MonadSTM m
        )
    => Mailbox m (Tip, (Header (block))) (Tip, Point)
    -> Mailbox m (Tip, block) (Tip, Point)
    -> BlockFetchClientPipelined block Point m ()
newFetchBlockClient headerMailbox blockMailbox = _ -- TODO continue from here
