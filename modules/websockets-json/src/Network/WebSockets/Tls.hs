--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
module Network.WebSockets.Tls where

import Prelude

import Control.Monad
    ( join
    )
import Data.ByteString
    ( ByteString
    )
import Data.List
    ( stripPrefix
    )
import Data.Maybe
    ( fromMaybe
    )

import qualified Data.ByteString as BS
import qualified Network.Connection as Network
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
                config  = WSS.defaultConfig { WSS.connectionGet = connectionGet }
             in
               WSS.runSecureClientWithConfig host (fromIntegral port) "/" config options []
        _ ->
            let
                host = fromMaybe url (stripPrefix "ws://" url)
             in
                WS.runClient host port "/"
  where
    connectionGet
        :: Network.Connection
        -> IO ByteString
    connectionGet conn =
        more id
      where
        more !dl = getChunkRepeatedly
            (\s -> more (dl . (s:)))
            (\s -> done (dl . (s:)))
            (done dl)

        done dl = return $! BS.concat $ dl []

        getChunkRepeatedly
            :: (ByteString -> IO r) -- moreK: need more input
            -> (ByteString -> IO r) -- doneK: end of line (line terminator found)
            -> IO r                 -- eofK:  end of file
            -> IO r
        getChunkRepeatedly moreK doneK _eofK =
            join $ Network.connectionGetChunk' conn $ \s ->
                if BS.null s then (moreK s, BS.empty)
                else (doneK s, BS.empty)
