--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.FetchTip.Ogmios
    ( newFetchTipClient
    ) where

import Kupo.Prelude

import Kupo.Data.FetchTip
    ( FetchTipClient
    )
import Kupo.Data.Ogmios
    ( NextBlockResponse (..)
    , decodeNextBlockResponse
    , encodeNextBlockRequest
    )

import qualified Network.WebSockets.Json as WS
import qualified Network.WebSockets.Tls as WSS

newFetchTipClient
    :: String
    -> Int
    -> FetchTipClient IO
newFetchTipClient host port = WSS.runClient host port $ \ws -> do
    WS.sendJson ws encodeNextBlockRequest
    WS.receiveJson ws decodeNextBlockResponse >>= \case
        RollBackward tip _point -> do
            return tip
        RollForward tip _block -> do
            return tip
