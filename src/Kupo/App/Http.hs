--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.Http
    ( runServer
    ) where

import Kupo.Prelude

import Network.HTTP.Types.Header
    ( Header, hContentType )
import Network.HTTP.Types.Status
    ( status200, status404, status406 )
import Network.Wai
    ( Application, Response, pathInfo, requestMethod, responseLBS )

import qualified Network.Wai.Handler.Warp as Warp

runServer :: String -> Int -> IO ()
runServer host port =
    Warp.runSettings settings app
  where
    settings = Warp.defaultSettings
        & Warp.setPort port
        & Warp.setHost (fromString host)
        & Warp.setServerName "kupo"
        & Warp.setBeforeMainLoop (do
            putStrLn $ "Server listening on " <> host <> ":" <> show port
        )

app :: Application
app req send = do
    case (requestMethod req, pathInfo req) of
        ("GET", [ "v1", "matches" ]) ->
            send =<< handleGetMatches Nothing Nothing

        ("GET", [ "v1", "matches", arg0 ]) ->
            send =<< handleGetMatches (Just arg0) Nothing

        ("GET", [ "v1", "matches", arg0, arg1 ]) ->
            send =<< handleGetMatches (Just arg0) (Just arg1)

        ("GET", _) ->
            send =<< handleNotFound

        (_, _) ->
            send =<< handleMethodNotAllowed

handleGetMatches
    :: Maybe Text
    -> Maybe Text
    -> IO Response
handleGetMatches arg0 arg1 = do
    print arg0
    print arg1
    pure $ responseLBS status200 defaultHeaders "OK"

handleNotFound :: IO Response
handleNotFound =
    pure $ responseLBS status404 defaultHeaders "Not found."

handleMethodNotAllowed :: IO Response
handleMethodNotAllowed =
    pure $ responseLBS status406 defaultHeaders "Method not allowed."

defaultHeaders :: [Header]
defaultHeaders =
    [ ( hContentType, "application/json; charset=utf-8" )
    ]
