--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
module Network.WebSockets.Tls where

import Prelude

import Data.List
    ( stripPrefix
    )
import Data.Maybe
    ( fromMaybe
    )
import Network.Connection
    ( connectionGetLine
    )

import qualified Network.WebSockets as WS
import qualified Wuss as WSS

-- | A drop-in replacement for 'WS.runClient' but that also handles TLS connections.
runClient
    :: String
        -- ^ Protocol + host
    -> Int
        -- ^ Port
    -> WS.ClientApp a
        -- ^ Client application to run
    -> IO a
runClient url port =
    case stripPrefix "wss://" url of
        Just host ->
            let
                options = WS.defaultConnectionOptions
                config  = WSS.defaultConfig { WSS.connectionGet = connectionGetLine (128 * 1024) }
             in
               WSS.runSecureClientWithConfig host (fromIntegral port) "/" config options []
        _ ->
            let
                host = fromMaybe url (stripPrefix "ws://" url)
             in
                WS.runClient host port "/"
