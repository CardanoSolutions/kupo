{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kupo.Data.Cardano.Point
    ( pattern BlockPoint
    , pattern GenesisPoint
    , module Kupo.Data.Cardano.Point
    , pointSlot
    ) where

import Kupo.Prelude

import Cardano.Slotting.Slot
    ( WithOrigin (..)
    )
import Kupo.Data.Cardano.Block
    ( Block
    )
import Kupo.Data.Cardano.HeaderHash
    ( headerHashFromText
    , headerHashToJson
    , headerHashToText
    )
import Kupo.Data.Cardano.SlotNo
    ( SlotNo (..)
    , slotNoFromText
    , slotNoToJson
    , slotNoToText
    )
import Ouroboros.Network.Block
    ( HeaderHash
    , pattern BlockPoint
    , pattern GenesisPoint
    , pointSlot
    )

import qualified Data.Aeson.Encoding as Json
import qualified Data.Text as T
import qualified Ouroboros.Network.Block as Ouroboros
import Kupo.Data.Cardano.Tip
    ( Tip
    , pattern GenesisTip
    , pattern Tip
    )

type Point = Ouroboros.Point Block

instance ToJSON Point where
    toJSON = error "ToJSON Point called instead of 'toEncoding'."
    toEncoding = pointToJson

-- | Parse a 'Point' from a text string. This alternatively tries two patterns:
--
-- - "origin"        → for a points that refers to the beginning of the blockchain
--
-- - "N.hhhh...hhhh" → A dot-separated integer and base16-encoded digest, which
--                     refers to a specific point on chain identified by this
--                     slot number and header hash.
--
pointFromText :: Text -> Maybe (Point)
pointFromText txt =
    genesisPointFromText <|> blockPointFromText
  where
    genesisPointFromText = GenesisPoint
        <$ guard (T.toLower txt == "origin")

    blockPointFromText = BlockPoint
        <$> slotNoFromText slotNo
        <*> headerHashFromText (T.drop 1 headerHash)
      where
        (slotNo, headerHash) = T.breakOn "." (T.strip txt)

pointToText :: Point -> Text
pointToText = \case
    GenesisPoint ->
        "origin"
    BlockPoint sl h ->
        mconcat
            [ slotNoToText sl
            , "."
            , headerHashToText h
            ]

pointFromTip :: Tip -> Point
pointFromTip = \case
    GenesisTip -> GenesisPoint
    Tip slotNo headerHash _blockNo -> BlockPoint slotNo headerHash

getPointSlotNo :: Point -> SlotNo
getPointSlotNo pt =
    case pointSlot pt of
        Origin -> SlotNo 0
        At sl  -> sl

getPointHeaderHash :: Point -> Maybe (HeaderHash Block)
getPointHeaderHash = \case
    GenesisPoint -> Nothing
    BlockPoint _ h -> Just h

unsafeGetPointHeaderHash :: HasCallStack => Point -> HeaderHash Block
unsafeGetPointHeaderHash =
    fromMaybe (error "Point is 'Origin'") . getPointHeaderHash
{-# INLINABLE unsafeGetPointHeaderHash #-}

pointToJson
    :: Point
    -> Json.Encoding
pointToJson = \case
    GenesisPoint ->
        Json.text "origin"
    BlockPoint slotNo headerHash ->
        Json.pairs $ mconcat
            [ Json.pair "slot_no" (slotNoToJson slotNo)
            , Json.pair "header_hash" (headerHashToJson headerHash)
            ]
