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
    ( IsBlock (..)
    , Point
    , SlotNo
    , Tip
    , WithOrigin
    , pointSlot
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
    , maxInFlight
    , mkDistanceFromTip
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
    inFlight <- WS.receiveJson ws (decodeFindIntersectResponse (intersectionNotFound pts)) >>= \case
        Left notFound -> throwIO notFound
        Right (point, tip) -> pure (maxInFlight (mkDistanceFromTip tip point))
    burst inFlight
    loop inFlight
  where
    -- Burst the server with some initial requests, to leverage pipelining.
    burst :: Integer -> m ()
    burst n = replicateM_ (fromInteger n) (WS.sendJson ws encodeRequestNext)

    -- Forever request and collect next blocks; the process can be interrupted by a forced rollback,
    -- in which case it simply look for a new intersection and start over.
    --
    -- We keep track of how many messages are in-flight to keep the synchronization 'elastic'; that
    -- is, the closer we get to the tip, the least messages we end up pipelining. When the tip is
    -- reached, we only fetch blocks one-by-one and enqueue a single message.
    loop :: Integer -> m ()
    loop !inFlight = do
        d <- WS.receiveJson ws decodeRequestNextResponse >>= \case
            RollBackward tip point -> do
                atomically (putIntermittentMessage mailbox (tip, point))
                pure $ mkDistanceFromTip tip point
            RollForward tip block -> do
                atomically (putHighFrequencyMessage mailbox (tip, block))
                pure $ mkDistanceFromTip tip (getPoint block)

        atomically (tryTakeTMVar forcedRollbackVar) >>= \case
            Nothing -> do
                let delta = max 0 (maxInFlight d - inFlight + 1)
                burst delta
                loop (inFlight + delta - 1)

            Just (pt, handler) -> do
                replicateM_ (fromInteger inFlight - 1) (WS.receiveJson ws decodeRequestNextResponse)
                WS.sendJson ws (encodeFindIntersect [pt])
                WS.receiveJson ws (decodeFindIntersectResponse (forcedIntersectionNotFound pt)) >>= \case
                    Left notFound -> do
                        onFailure handler
                        throwIO notFound
                    Right (point, tip) -> do
                        onSuccess handler
                        let inFlight' = maxInFlight (mkDistanceFromTip tip point)
                        burst inFlight'
                        loop inFlight'
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
