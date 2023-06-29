--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- | Extras for working with websockets & JSON.
--
-- This module allows to write simple websocket clients following a request/response pattern over some
-- JSON encoded data.
module Network.WebSockets.Json where

import Prelude

import Control.Exception
    ( Exception
    )
import Control.Monad.Catch
    ( MonadThrow (..)
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Data.ByteString.Lazy
    ( ByteString
    )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Parser.Internal as Json
import qualified Data.Aeson.Types as Json
import qualified Network.WebSockets as WS

-- | Send some JSON encoding through the given connection.
sendJson
    :: forall m.
        ( MonadIO m
        )
    => WS.Connection
    -> Json.Encoding
    -> m ()
sendJson ws =
    liftIO . WS.sendTextData ws . Json.encodingToLazyByteString

-- | Synchronously receive some JSON-encoded bytes through the given connection.
--
-- Throws 'MalformedOrUnexpectedResponseException' upon failure.
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
        (\(path, err) -> throwM $ MalformedOrUnexpectedResponse bytes path err)
        pure
        (Json.eitherDecodeWith Json.jsonEOF (Json.iparse decoder) bytes)

-- | An exception thrown when failing to decode a JSON payload.
data MalformedOrUnexpectedResponseException =
    MalformedOrUnexpectedResponse
        { bytesReceived :: !ByteString
            -- ^ Actual bytes received from the websocket
        , errorPath :: !Json.JSONPath
            -- ^ JSON path at which the decoding error occured
        , hint :: !String
            -- ^ A explanation of what's going on.
        }
    deriving (Show)

instance Exception MalformedOrUnexpectedResponseException
