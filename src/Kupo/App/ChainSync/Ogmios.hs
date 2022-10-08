--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.ChainSync.Ogmios
    ( connect
    , runChainSyncClient

      -- * Internal
    , intersectionNotFound
    ) where

import Kupo.Prelude

import Kupo.App.Mailbox
    ( Mailbox
    , putHighFrequencyMessage
    , putIntermittentMessage
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Control.MonadThrow
    ( MonadThrow (..)
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
    , RequestNextResponse (..)
    , decodeFindIntersectResponse
    , decodeRequestNextResponse
    , encodeFindIntersect
    , encodeRequestNext
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
    WS.sendJson ws (encodeFindIntersect pts)
    WS.receiveJson ws (decodeFindIntersectResponse (intersectionNotFound pts)) >>= \case
        Left notFound -> do
            return notFound
        Right{} -> do
            beforeMainLoop
            replicateM_ 100 (WS.sendJson ws encodeRequestNext)
            forever $ do
                WS.receiveJson ws decodeRequestNextResponse >>= \case
                    RollBackward tip point -> do
                        atomically (putIntermittentMessage mailbox (tip, point))
                    RollForward tip block -> do
                        atomically (putHighFrequencyMessage mailbox (tip, block))
                WS.sendJson ws encodeRequestNext

--
-- Exceptions
--

intersectionNotFound
    :: [Point]
    -> WithOrigin SlotNo
    -> IntersectionNotFoundException
intersectionNotFound (fmap pointSlot -> requestedPoints) tip =
    IntersectionNotFound { requestedPoints, tip }

-- Connection

connect
    :: ConnectionStatusToggle IO
    -> String
    -> Int
    -> (WS.Connection -> IO a)
    -> IO a
connect ConnectionStatusToggle{toggleConnected} host port action =
    WS.runClientWith host port "/"
        -- TODO: Try to negotiate compact mode v2 once available.
        --
        -- See [ogmios#237](https://github.com/CardanoSolutions/ogmios/issues/237)
        --
        -- [("Sec-WebSocket-Protocol", "ogmios.v1:compact")]
        WS.defaultConnectionOptions [] (\ws -> toggleConnected >> action ws)
