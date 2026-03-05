module Kupo.Data.Http.QuantityEncoding where

import Kupo.Prelude

data QuantityEncoding = EncodeAsInteger | EncodeAsString
    deriving Show

mediaTypeParam :: (ByteString, ByteString)
mediaTypeParam = ("asset-quantity", "string")
