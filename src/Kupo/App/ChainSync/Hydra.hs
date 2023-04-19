--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}
module Kupo.App.ChainSync.Hydra
    ( connect
    , runChainSyncClient
    ) where

import Kupo.Prelude

import Control.Exception.Safe
    ( MonadThrow
    )
import Kupo.App.Mailbox
    ( Mailbox
    , putHighFrequencyMessage
    , putIntermittentMessage
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Data.Cardano
    ( Point
    , SlotNo
    , Tip
    , WithOrigin
    , pointSlot
    )
import Kupo.Data.ChainSync
    ( IntersectionNotFoundException (..)
    )
import Kupo.Data.Ogmios
    ( PartialBlock
    )

import qualified Data.ByteString as BS
import Kupo.Data.Cardano.HeaderHash
    ( unsafeHeaderHashFromBytes
    )
import Kupo.Data.Cardano.Point
    ( pattern BlockPoint
    )
import Kupo.Data.Cardano.Tip
    ( pattern Tip
    )
import Kupo.Data.Hydra
    ( HydraMessage (..)
    , decodeHydraMessage
    )
import Kupo.Data.PartialBlock
    ( PartialBlock (..)
    )
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Json as WS

runChainSyncClient
    :: forall m.
        ( MonadIO m
        , MonadSTM m
        , MonadThrow m
        )
    => Mailbox m (Tip, PartialBlock) (Tip, Point)
    -> m () -- An action to run before the main loop starts.
    -> [Point]
    -> WS.Connection
    -> m IntersectionNotFoundException
runChainSyncClient mailbox beforeMainLoop pts ws = do
    beforeMainLoop
    forever $ do
        WS.receiveJson ws decodeHydraMessage >>= \case
            SnapshotConfirmed{} -> do
                let headerHash = unsafeHeaderHashFromBytes $ BS.replicate 32 0
                let slotNo = 1 -- TODO: decode snapshot number as slot
                let blockNo = 1
                let tip = Tip slotNo headerHash blockNo
                let block =
                        PartialBlock
                            { blockPoint = BlockPoint slotNo headerHash
                            , blockBody  = []
                            }
                atomically (putHighFrequencyMessage mailbox (tip, block))
            SomethingElse -> pure ()

connect
    :: ConnectionStatusToggle IO
    -> String
    -> Int
    -> (WS.Connection -> IO a)
    -> IO a
connect ConnectionStatusToggle{toggleConnected} host port action =
    WS.runClientWith host port "/"
        WS.defaultConnectionOptions [] (\ws -> toggleConnected >> action ws)
