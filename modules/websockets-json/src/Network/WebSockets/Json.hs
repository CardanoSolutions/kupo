--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Network.WebSockets.Json where

import Relude

import Control.Monad.Class.MonadThrow
    ( MonadThrow (..) )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Internal as Json
import qualified Data.Aeson.Parser.Internal as Json
import qualified Data.Aeson.Types as Json
import qualified Network.WebSockets as WS

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
