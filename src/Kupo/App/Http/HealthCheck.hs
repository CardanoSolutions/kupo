--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.Http.HealthCheck
    ( healthCheck
    ) where

import Kupo.Prelude

import Kupo.Control.MonadCatch
    ( handle
    )
import Kupo.Data.Health
    ( ConnectionStatus (..)
    )
import Network.HTTP.Client
    ( defaultManagerSettings
    , httpLbs
    , newManager
    , parseRequest
    , responseBody
    )
import Relude.Extra
    ( lookup
    )
import System.Exit
    ( ExitCode (..)
    )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json

-- | Performs a health check against a running server, this is a standalone
-- program which exits immediately, either with a success or an error code.
healthCheck :: String -> Int -> IO ()
healthCheck host port = do
    response <- handle onAnyException $ join $ httpLbs
        <$> parseRequest ("http://" <> host <> ":" <> show port <> "/health")
        <*> newManager defaultManagerSettings
    case Json.decode (responseBody response) >>= getConnectionStatus of
        Just st | Json.value st == Json.toEncoding Connected ->
            return ()
        _ -> do
            exitWith (ExitFailure 1)
  where
    onAnyException (_ :: SomeException) =
        exitWith (ExitFailure 1)
    getConnectionStatus =
        lookup @(Map String Json.Value) "connection_status"
