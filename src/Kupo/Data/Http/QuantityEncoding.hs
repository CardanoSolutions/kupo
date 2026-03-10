-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.Http.QuantityEncoding
    ( QuantityEncoding(..)
    , adjustMediaType
    , mediaTypeParam
    ) where

import Kupo.Prelude

import qualified Prelude as P
    ( id
    )

import Network.HTTP.Media
    ( MediaType
    , mapContentMedia
    , renderHeader
    , (//)
    , (/:)
    )
import Network.HTTP.Types
    ( Header
    , ResponseHeaders
    , hContentType
    )

data QuantityEncoding = EncodeAsInteger | EncodeAsString
    deriving Show

adjustMediaType :: QuantityEncoding -> ResponseHeaders -> ResponseHeaders
adjustMediaType EncodeAsInteger = P.id
adjustMediaType EncodeAsString  = map insertParam

mediaTypeParam :: (ByteString, ByteString)
mediaTypeParam = ("asset-quantity", "string")

insertParam :: Header -> Header
insertParam (n,v)
    | n == hContentType = (n, maybe v P.id (mapContentMedia mmap v))
    | otherwise         = (n, v)

mmap :: [(MediaType,ByteString)]
mmap =
    [("application"//"json"/:("charset","utf-8"),
      renderHeader $ "application"//"json"/:("charset","utf-8")/:mediaTypeParam)
    ,("application"//"json",
      renderHeader $ "application"//"json"/:mediaTypeParam)
    ]
