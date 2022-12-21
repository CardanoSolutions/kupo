{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kupo.Data.Cardano.SlotNo
    ( SlotNo(..)
    , WithOrigin(..)
    , module Kupo.Data.Cardano.SlotNo
    ) where
import Kupo.Prelude

import Cardano.Ledger.Slot
    ( SlotNo (..)
    )
import Ouroboros.Network.Point
    ( WithOrigin (..)
    )

import qualified Data.Aeson.Encoding as Json
import qualified Data.Text as T
import qualified Data.Text.Read as T

slotNoToJson :: SlotNo -> Json.Encoding
slotNoToJson =
    Json.integer . toInteger . unSlotNo
{-# INLINABLE slotNoToJson #-}

-- | Parse a slot number from a text string.
slotNoFromText :: Text -> Maybe SlotNo
slotNoFromText txt = do
    (slotNo, remSlotNo) <- either (const Nothing) Just (T.decimal txt)
    guard (T.null remSlotNo)
    guard (slotNo < maxBound `div` 2 - 1)
    pure (SlotNo slotNo)

slotNoToText :: SlotNo -> Text
slotNoToText =
    show . unSlotNo
{-# INLINABLE slotNoToText #-}

distanceToSlot :: SlotNo -> SlotNo -> Word64
distanceToSlot (SlotNo a) (SlotNo b)
    | a > b = a - b
    | otherwise = b - a

instance ToJSON (WithOrigin SlotNo) where
    toEncoding = \case
        Origin -> toEncoding ("origin" :: Text)
        At sl -> toEncoding sl
