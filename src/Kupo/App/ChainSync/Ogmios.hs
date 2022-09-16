--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.ChainSync.Ogmios
    ( connect
    , runChainSyncClient

      -- * Internal
    , intersectionNotFound
    , forcedIntersectionNotFound
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
import Kupo.Data.Configuration
    ( maxInFlight
    )
import Kupo.Data.Ogmios
    ( PartialBlock
    , RequestNextResponse (..)
    , decodeFindIntersectResponse
    , decodeRequestNextResponse
    , encodeFindIntersect
    , encodeRequestNext
    )

import Kupo.Data.ChainSync
    ( ForcedRollbackHandler (..)
    , IntersectionNotFoundException (..)
    )
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Json as WS

runChainSyncClient
    :: forall m.
        ( MonadIO m
        , MonadSTM m
        , MonadThrow m
        )
    => TMVar m (Point, ForcedRollbackHandler m)
    -> Mailbox m (Tip, PartialBlock) (Tip, Point)
    -> [Point]
    -> WS.Connection
    -> m ()
runChainSyncClient forcedRollbackVar mailbox pts ws = do
    WS.sendJson ws (encodeFindIntersect pts)
    WS.receiveJson ws (decodeFindIntersectResponse (intersectionNotFound pts)) >>= \case
        Left notFound -> throwIO notFound
        Right () -> burst
    forever $ atomically (tryTakeTMVar forcedRollbackVar) >>= \case
        Nothing -> do
            WS.receiveJson ws decodeRequestNextResponse >>= \case
                RollBackward tip point -> do
                    atomically (putIntermittentMessage mailbox (tip, point))
                RollForward tip block -> do
                    atomically (putHighFrequencyMessage mailbox (tip, block))
            WS.sendJson ws encodeRequestNext

        Just (pt, handler) -> do
            replicateM_ maxInFlight (WS.receiveJson ws decodeRequestNextResponse)
            WS.sendJson ws (encodeFindIntersect [pt])
            WS.receiveJson ws (decodeFindIntersectResponse (forcedIntersectionNotFound pt)) >>= \case
                Left notFound -> do
                    onFailure handler
                    throwIO notFound
                Right () -> do
                    onSuccess handler
                    burst
  where
    -- Burst the server with some initial requests, to leverage pipelining.
    burst :: m ()
    burst = replicateM_ maxInFlight (WS.sendJson ws encodeRequestNext)

--
-- Exceptions
--

intersectionNotFound
    :: [Point]
    -> WithOrigin SlotNo
    -> IntersectionNotFoundException
intersectionNotFound (fmap pointSlot -> requestedPoints) tip =
    IntersectionNotFound { requestedPoints, tip }

forcedIntersectionNotFound
    :: Point
    -> WithOrigin SlotNo
    -> IntersectionNotFoundException
forcedIntersectionNotFound (pointSlot -> point) _tip =
    ForcedIntersectionNotFound { point }

-- Connection

connect
    :: ConnectionStatusToggle IO
    -> String
    -> Int
    -> (WS.Connection -> IO ())
    -> IO ()
connect ConnectionStatusToggle{toggleConnected} host port action =
    WS.runClientWith host port "/"
        -- TODO: Try to negotiate compact mode v2 once available.
        --
        -- See [ogmios#237](https://github.com/CardanoSolutions/ogmios/issues/237)
        --
        -- [("Sec-WebSocket-Protocol", "ogmios.v1:compact")]
        WS.defaultConnectionOptions [] (\ws -> toggleConnected >> action ws)

data CannotResolveAddressException = CannotResolveAddress
    { host :: !String
    , port :: !Int
    } deriving (Show)

instance Exception CannotResolveAddressException
