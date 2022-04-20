--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.ChainSync.Ogmios
    ( connect
    , runChainSyncClient
    ) where

import Kupo.Prelude

import Kupo.Control.MonadThrow
    ( MonadThrow (..) )
import Kupo.Data.Cardano
    ( Block, Point, Tip )
import Kupo.Data.ChainSync
    ( ChainSyncHandler (..) )
import Kupo.Data.Ogmios
    ( PartialBlock
    , RequestNextResponse (..)
    , decodeFindIntersectResponse
    , decodeRequestNextResponse
    , encodeFindIntersect
    , encodeRequestNext
    )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Internal as Json
import qualified Data.Aeson.Parser.Internal as Json
import qualified Data.Aeson.Types as Json
import qualified Network.WebSockets as WS

runChainSyncClient
    :: forall m.
        ( MonadIO m
        , MonadThrow m
        )
    => ChainSyncHandler m (Tip Block) (Point Block) PartialBlock
    -> [Point Block]
    -> WS.Connection
    -> m ()
runChainSyncClient ChainSyncHandler{onRollBackward, onRollForward} pts ws = do
    sendJson ws (encodeFindIntersect pts)
    receiveJson ws (decodeFindIntersectResponse pts) >>= \case
        Left notFound -> throwIO notFound
        Right () -> pure ()
    -- NOTE: burst the server with some initial requests, to leverage pipelining.
    replicateM_ 100 (sendJson ws encodeRequestNext)
    forever $ do
        receiveJson ws decodeRequestNextResponse >>= \case
            RollBackward tip point -> do
                onRollBackward tip point
            RollForward tip block -> do
                onRollForward tip block
        sendJson ws encodeRequestNext

-- Connection

connect
    :: ConnectionStatusToggle IO
    -> String
    -> Int
    -> (WS.Connection -> IO ())
    -> IO ()
connect ConnectionStatusToggle{toggleConnected} host port action =
    WS.runClientWith host port "/"
        WS.defaultConnectionOptions
        [("Sec-WebSocket-Protocol", "ogmios.v1:compact")]
        (\ws -> toggleConnected >> action ws)

data CannotResolveAddressException = CannotResolveAddress
    { host :: String
    , port :: Int
    } deriving (Show)

instance Exception CannotResolveAddressException

-- WebSockets Extras

data MalformedOrUnexpectedResponseException =
    MalformedOrUnexpectedResponse LByteString Json.JSONPath String
    deriving (Show)
instance Exception MalformedOrUnexpectedResponseException

sendJson
    :: forall m.
        ( MonadIO m
        )
    => WS.Connection
    -> Json.Encoding
    -> m ()
sendJson ws =
    liftIO . WS.sendTextData ws . Json.encodingToLazyByteString

receiveJson
    :: forall m a.
        ( MonadThrow m
        , MonadIO m
        )
    => WS.Connection
    -> (Json.Value -> Json.Parser a)
    -> m a
receiveJson ws decoder =  do
    bytes <- liftIO (WS.receiveData ws)
    either
        (\(path, err) -> throwIO $ MalformedOrUnexpectedResponse bytes path err)
        pure
        (Json.eitherDecodeWith Json.jsonEOF (Json.iparse decoder) bytes)
