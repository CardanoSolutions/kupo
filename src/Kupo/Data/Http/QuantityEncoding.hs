-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.Http.QuantityEncoding
    ( QuantityEncoding(..)
    , adjustMediaType
    , mediaTypeParam
    , matchAcceptHeader
    ) where

import Kupo.Prelude

import qualified Prelude as P
    ( id
    )

import Network.HTTP.Media
    ( MediaType
    , mapAccept
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
    deriving (Show, Eq)

adjustMediaType :: QuantityEncoding -> ResponseHeaders -> ResponseHeaders
adjustMediaType = \case
    EncodeAsInteger -> P.id
    EncodeAsString -> map insertParam

mediaTypeParam :: (ByteString, ByteString)
mediaTypeParam = ("asset-quantity", "string")

insertParam :: Header -> Header
insertParam (n, v)
    | n == hContentType = (n, fromMaybe v (mapContentMedia mmap v))
    | otherwise         = (n, v)
  where
    mmap :: [(MediaType,ByteString)]
    mmap =
        [ ( "application" // "json" /: ("charset", "utf-8")
          , renderHeader $ "application" // "json" /: ("charset", "utf-8") /: mediaTypeParam
          )
        , ( "application"//"json"
          , renderHeader $ "application" // "json" /: mediaTypeParam
          )
        ]

matchAcceptHeader :: Maybe ByteString -> QuantityEncoding
matchAcceptHeader header = fromMaybe EncodeAsInteger $ header >>= mapAccept qualities
  where
    qualities =
        [ ("application" // "json"
          , EncodeAsInteger
          )
        , ("application" // "json" /: ("charset", "utf-8")
          , EncodeAsInteger
          )
        , ("application" // "json" /: mediaTypeParam
          , EncodeAsString
          )
        , ("application" // "json" /: mediaTypeParam /: ("charset", "utf-8")
          , EncodeAsString
          )
        , ("application" // "json" /: ("charset", "utf-8") /: mediaTypeParam
          , EncodeAsString
          )
        ]
