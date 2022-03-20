--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.ChainSync.Ogmios
    ( runChainSyncClient
    ) where

import Kupo.Prelude

import Data.Attoparsec.ByteString
    ( Parser )
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
import Network.WebSockets.Stream
    ( Stream )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Types as Json
import qualified Network.WebSockets.Stream as WS

runChainSyncClient
    :: forall m.
        ( MonadIO m
        , MonadThrow m
        )
    => ChainSyncHandler m (Tip Block) (Point Block) PartialBlock
    -> [Point Block]
    -> Stream
    -> m ()
runChainSyncClient ChainSyncHandler{onRollBackward, onRollForward} pts stream = do
    writeJson stream (encodeFindIntersect pts)
    readJson stream (decodeFindIntersectResponse pts) >>= \case
        Left notFound -> throwIO notFound
        Right () -> pure ()
    -- NOTE: burst the server with some initial requests, to leverage pipelining.
    replicateM_ 100 (writeJson stream encodeRequestNext)
    forever $ do
        readJson stream decodeRequestNextResponse >>= \case
            RollBackward tip point -> do
                onRollBackward tip point
            RollForward tip block -> do
                onRollForward tip block
        writeJson stream encodeRequestNext


-- WebSockets Extras

data MalformedOrUnexpectedResponseException =
    MalformedOrUnexpectedResponseException
    deriving (Show)
instance Exception MalformedOrUnexpectedResponseException

writeJson
    :: forall m.
        ( MonadIO m
        )
    => Stream
    -> Json.Encoding
    -> m ()
writeJson stream encoding =
    liftIO (WS.write stream (Json.encodingToLazyByteString encoding))

readJson
    :: forall m a.
        ( MonadThrow m
        , MonadIO m
        )
    => Stream
    -> (Json.Value -> Json.Parser a)
    -> m a
readJson stream decode =  do
    liftIO (WS.parse stream parser)
    >>=
    maybe (throwIO MalformedOrUnexpectedResponseException) pure
  where
    parser :: Parser a
    parser = do
        value <- Json.json
        case Json.parse decode value of
            Json.Error{}   -> empty
            Json.Success a -> pure a
