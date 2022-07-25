--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.ChainSync.Ogmios
    ( connect
    , runChainSyncClient
    ) where

import Kupo.Prelude

import Kupo.App.Mailbox
    ( Mailbox, putHighFrequencyMessage, putIntermittentMessage )
import Kupo.Control.MonadSTM
    ( MonadSTM (..) )
import Kupo.Control.MonadThrow
    ( MonadThrow (..) )
import Kupo.Data.Cardano
    ( Point, Tip )
import Kupo.Data.Configuration
    ( maxInFlight )
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
    -> [Point]
    -> WS.Connection
    -> m ()
runChainSyncClient mailbox pts ws = do
    WS.sendJson ws (encodeFindIntersect pts)
    WS.receiveJson ws (decodeFindIntersectResponse pts) >>= \case
        Left notFound -> throwIO notFound
        Right () -> pure ()
    -- NOTE: burst the server with some initial requests, to leverage pipelining.
    replicateM_ maxInFlight (WS.sendJson ws encodeRequestNext)
    forever $ do
        WS.receiveJson ws decodeRequestNextResponse >>= \case
            RollBackward tip point -> do
                atomically (putIntermittentMessage mailbox (tip, point))
            RollForward tip block -> do
                atomically (putHighFrequencyMessage mailbox (tip, block))
        WS.sendJson ws encodeRequestNext

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
    { host :: String
    , port :: Int
    } deriving (Show)

instance Exception CannotResolveAddressException
