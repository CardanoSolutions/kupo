--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.FetchBlock.Ogmios
    ( withFetchBlockClient
    ) where

import Kupo.Prelude

import Kupo.Data.FetchBlock
    ( FetchBlockClient
    )
import Kupo.Data.Ogmios
    ( NextBlockResponse (..)
    , PartialBlock
    , decodeFindIntersectionResponse
    , decodeNextBlockResponse
    , encodeFindIntersectionRequest
    , encodeNextBlockRequest
    )

import qualified Network.WebSockets.Json as WS
import qualified Network.WebSockets.Tls as WSS

withFetchBlockClient
    :: String
    -> Int
    -> (FetchBlockClient IO PartialBlock -> IO ())
    -> IO ()
withFetchBlockClient host port action =
    action $ \point reply -> WSS.runClient host port $ \ws -> do
        WS.sendJson ws (encodeFindIntersectionRequest [point])
        WS.receiveJson ws (decodeFindIntersectionResponse identity) >>= \case
            Left _notFound -> reply Nothing
            Right{} -> do
                replicateM_ 2 (WS.sendJson ws encodeNextBlockRequest)
                -- NOTE: The first reply is always a 'Roll-Backward' to the requested point. Ignore.
                void (WS.receiveJson ws decodeNextBlockResponse)
                WS.receiveJson ws decodeNextBlockResponse >>= \case
                    RollBackward _tip _point -> do
                        reply Nothing
                    RollForward _tip block -> do
                        reply (Just block)
