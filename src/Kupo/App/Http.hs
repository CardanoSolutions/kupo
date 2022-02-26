--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Kupo.App.Http
    ( runServer
    ) where

import Kupo.Prelude

import Kupo.Configuration
    ( StandardCrypto )
import Kupo.Control.MonadDatabase
    ( Database (..) )
import Kupo.Data.Pattern
    ( patternFromText
    , patternToQueryLike
    , resultToJson
    , unsafeMkResult
    , wildcard
    )
import Network.HTTP.Types.Header
    ( Header, hContentLength, hContentType )
import Network.HTTP.Types.Status
    ( Status, status200, status400, status404, status406 )
import Network.Wai
    ( Application
    , Response
    , pathInfo
    , requestMethod
    , responseLBS
    , responseStream
    )

import qualified Data.Aeson as Json
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wai.Handler.Warp as Warp

--
-- Server
--

runServer :: (forall a. (Database IO -> IO a) -> IO a) -> String -> Int -> IO ()
runServer withDatabase host port =
    Warp.runSettings settings (app withDatabase)
  where
    settings = Warp.defaultSettings
        & Warp.setPort port
        & Warp.setHost (fromString host)
        & Warp.setServerName "kupo"
        & Warp.setBeforeMainLoop (do
            putStrLn $ "Server listening on " <> host <> ":" <> show port
        )

--
-- Router
--

app :: (forall a. (Database IO -> IO a) -> IO a) -> Application
app withDatabase req send = withDatabase $ \db ->
    case (requestMethod req, pathInfo req) of
        ("GET", [ "v1", "matches" ]) ->
            send $ handleGetMatches db Nothing

        ("GET", [ "v1", "matches", arg0 ]) ->
            send $ handleGetMatches db (Just (arg0, Nothing))

        ("GET", [ "v1", "matches", arg0, arg1 ]) ->
            send $ handleGetMatches db (Just (arg0, Just arg1))

        ("GET", _) ->
            send handleNotFound

        (_, _) ->
            send handleMethodNotAllowed

--
-- Handlers
--

handleGetMatches
    :: Database IO
    -> Maybe (Text, Maybe Text)
    -> Response
handleGetMatches Database{..} query = do
    let txt = maybe wildcard (\(a0, a1) -> a0 <> "/" <> fromMaybe wildcard a1) query
    case patternFromText @StandardCrypto txt of
        Nothing ->
            handleInvalidPattern
        Just p -> do
            responseStream status200 defaultHeaders $ \write flush -> do
                let streamResult = write . Json.fromEncoding . resultToJson
                runTransaction $ do
                    foldInputsByAddress
                        (patternToQueryLike p)
                        (\a0 a1 a2 a3 -> streamResult . unsafeMkResult @StandardCrypto a0 a1 a2 a3)
                flush

handleInvalidPattern :: Response
handleInvalidPattern = do
    responseJson status400 defaultHeaders $ HttpError
        { hint = "Invalid pattern! To fetch matches, you may provide any valid \
                 \pattern, including wildcards ('*') or full addresses. Make \
                 \sure to double-check the documentation at: \
                 \<https://cardanosolutions.github.io/kupo>!"
        }

handleNotFound :: Response
handleNotFound =
    responseJson status404 defaultHeaders $ HttpError
        { hint = "Endpoint not found. Make sure to double-check the \
                 \documentation at: <https://cardanosolutions.github.io/kupo>!"
        }

handleMethodNotAllowed :: Response
handleMethodNotAllowed =
    responseJson status406 defaultHeaders $ HttpError
        { hint = "Unsupported method called on known endpoint. Make sure to \
                 \double-check the documentation at: \
                 \<https://cardanosolutions.github.io/kupo>!"
        }

--
-- Helpers
--

data HttpError = HttpError
    { hint :: Text }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

defaultHeaders :: [Header]
defaultHeaders =
    [ ( hContentType, "application/json; charset=utf-8" )
    ]

responseJson
    :: ToJSON a
    => Status
    -> [Header]
    -> a
    -> Response
responseJson status headers a =
    let
        bytes = B.toLazyByteString $ Json.fromEncoding (Json.toEncoding a)
        len = BL.length bytes
        contentLength = ( hContentLength, encodeUtf8 (show @Text len) )
     in
        responseLBS status (contentLength : headers) bytes
