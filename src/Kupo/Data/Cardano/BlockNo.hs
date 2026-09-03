module Kupo.Data.Cardano.BlockNo
    ( BlockNo (..)
    , blockNoToJson
    ) where
import qualified Data.Aeson.Encoding as Json
import Cardano.Slotting.Block
    ( BlockNo (..)
    )
import Kupo.Prelude

blockNoToJson :: BlockNo -> Json.Encoding
blockNoToJson =
    Json.integer . toInteger . unBlockNo
{-# INLINABLE blockNoToJson #-}
