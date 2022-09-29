-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.Data.Http.Helpers where

import Kupo.Prelude

import qualified Data.ByteString as BS
import qualified Network.HTTP.Types as Http

--
-- Helpers
--

infixr 8 .=
(.=) :: ByteString -> Text -> (ByteString, Maybe ByteString)
(.=) key val = (key, Just (encodeUtf8 val))

renderQueryParams :: Http.Query -> Maybe expectation -> String
renderQueryParams queryParams expectation =
    let
        symbol = maybe "x" (const "✓") expectation
        queryStr = "?" <> BS.intercalate "&" (renderParam <$> queryParams)
     in
        symbol <> "   " <> decodeUtf8 queryStr

renderParam :: (ByteString, Maybe ByteString) -> ByteString
renderParam = \case
    (key, Nothing) -> key
    (key, Just val) -> key <> "=" <> val
