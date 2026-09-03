module Kupo.Data.Cardano.Checkpoint
    ( Checkpoint (..)
    , checkpointToJson
    , getCheckpointHeaderHash
    , getCheckpointSlotNo
    , unsafeGetCheckpointHeaderHash
    ) where

import Kupo.Prelude

import Kupo.Data.Cardano.Block
    ( Block
    )
import Kupo.Data.Cardano.BlockNo
    ( BlockNo
    , blockNoToJson
    )
import Kupo.Data.Cardano.HeaderHash
    ( headerHashToJson
    )
import Ouroboros.Network.Block
    ( HeaderHash
    )
import Kupo.Data.Cardano.Point
    ( Point
    , getPointSlotNo
    , getPointHeaderHash
    , unsafeGetPointHeaderHash
    )
import Kupo.Data.Cardano.SlotNo
    ( slotNoToJson, SlotNo
    )

import qualified Data.Aeson.Encoding as Json

data Checkpoint = Checkpoint
    { checkpointPoint :: !Point
    , checkpointBlockNo :: !BlockNo
    } deriving (Show, Eq)

checkpointToJson :: Checkpoint -> Json.Encoding
checkpointToJson pt = Json.pairs $ mconcat
    [ Json.pair "slot_no"
        (slotNoToJson (getCheckpointSlotNo pt))
    , Json.pair "header_hash"
        (headerHashToJson (unsafeGetCheckpointHeaderHash pt))
    , Json.pair "block_no"
        (blockNoToJson (checkpointBlockNo pt))
    ]
{-# INLINABLE checkpointToJson #-}

getCheckpointSlotNo :: Checkpoint -> SlotNo
getCheckpointSlotNo checkpoint = getPointSlotNo (checkpointPoint checkpoint)

getCheckpointHeaderHash :: Checkpoint -> Maybe (HeaderHash Block)
getCheckpointHeaderHash checkpoint = getPointHeaderHash (checkpointPoint checkpoint)

unsafeGetCheckpointHeaderHash :: Checkpoint -> HeaderHash Block
unsafeGetCheckpointHeaderHash =
    fromMaybe (error "Point is 'Origin'") . getCheckpointHeaderHash
{-# INLINABLE unsafeGetCheckpointHeaderHash #-}
