-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}

module Test.Kupo.Fixture where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( BinaryData
    , DatumHash
    , Point
    , PolicyId
    , Script
    , ScriptHash
    , TransactionId
    , pattern GenesisPoint
    , unsafeBinaryDataFromBytes
    , unsafeDatumHashFromBytes
    , unsafePolicyIdFromBytes
    , unsafeScriptFromBytes
    , unsafeScriptHashFromBytes
    , unsafeTransactionIdFromBytes
    )
import Kupo.Data.Database
    ( Checkpoint (..)
    , pointFromRow
    )

someNonExistingPoint :: Point
someNonExistingPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        14141414
    , checkpointHeaderHash = unsafeDecodeBase16
        "0000000000000000000000000000000000000000000000000000000000000000"
    }

somePoint :: Point
somePoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        51292637
    , checkpointHeaderHash = unsafeDecodeBase16
        "2e7ee124eccbc648789008f8669695486f5727cada41b2d86d1c36355c76b771"
    }

somePointAncestor :: Point
somePointAncestor = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        51292617
    , checkpointHeaderHash = unsafeDecodeBase16
        "e63797c3982e9af8ab1f9ea1a60bfa47df9ead2e745adcca95ad30c6f49982a0"
    }

somePointSuccessor :: Point
somePointSuccessor = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        51292686
    , checkpointHeaderHash = unsafeDecodeBase16
        "0cdb5efde9aba0bbd2f0606d4ab64e4762cf13fe4ee78e0c5d956666fe6c95d3"
    }

someOtherPoint :: Point
someOtherPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        53392903
    , checkpointHeaderHash = unsafeDecodeBase16
        "56ed3689f5a1dce99345c5ec85de8ff307fe0a31dff443e0170b9c21edbeaba5"
    }

somePointNearScripts :: Point
somePointNearScripts = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        62637303
    , checkpointHeaderHash = unsafeDecodeBase16
        "1d29ed9e913c88593f0827b7579909e164541498e302da349047496d7360c74a"
    }

lastByronPoint :: Point
lastByronPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        1598399
    , checkpointHeaderHash = unsafeDecodeBase16
        "7e16781b40ebf8b6da18f7b5e8ade855d6738095ef2f1c58c77e88b6e45997a4"
    }

lastShelleyPoint :: Point
lastShelleyPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        13694363
    , checkpointHeaderHash = unsafeDecodeBase16
        "b596f9739b647ab5af901c8fc6f75791e262b0aeba81994a1d622543459734f2"
    }

lastAllegraPoint :: Point
lastAllegraPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        18014387
    , checkpointHeaderHash = unsafeDecodeBase16
        "9914c8da22a833a777d8fc1f735d2dbba70b99f15d765b6c6ee45fe322d92d93"
    }

lastMaryPoint :: Point
lastMaryPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        36158304
    , checkpointHeaderHash = unsafeDecodeBase16
        "2b95ce628d36c3f8f37a32c2942b48e4f9295ccfe8190bcbc1f012e1e97c79eb"
    }

lastAlonzoPoint :: Point
lastAlonzoPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        62510369
    , checkpointHeaderHash = unsafeDecodeBase16
        "d931221f9bc4cae34de422d9f4281a2b0344e86aac6b31eb54e2ee90f44a09b9"
    }

eraBoundaries :: [(String, Point)]
eraBoundaries =
    [ ("Byron", GenesisPoint)
    , ("Shelley", lastByronPoint)
    , ("Allegra", lastShelleyPoint)
    , ("Mary", lastAllegraPoint)
    , ("Alonzo", lastMaryPoint)
    , ("Babbage", lastAlonzoPoint)
    ]

-- A policy id from tokens present in a transaction output in early Alonzo
somePolicyId :: PolicyId
somePolicyId = unsafePolicyIdFromBytes $ unsafeDecodeBase16
    "469F9A74824555709042FB95AA2D11E3C7FA4EADA2FE97BC287687DD"

-- A transaction present in the first Alonzo block.
someTransactionId :: TransactionId
someTransactionId = unsafeTransactionIdFromBytes $ unsafeDecodeBase16
    "836EA71A49D79B222C3D525EB980439F13656241D9A2D6AC4F8CEBE159FCBB88"

someDatumHashInWitness :: DatumHash
someDatumHashInWitness = unsafeDatumHashFromBytes $ unsafeDecodeBase16
    "0118AD9F6A79B8DFAB690DCB66EA6244A382891525789B405F7AF7DC61635578"

someDatumHashInOutput :: DatumHash
someDatumHashInOutput = unsafeDatumHashFromBytes $ unsafeDecodeBase16
    "13B17DA8E9EE49F07B99D887F0F11251115F927769A688F50A34A75D82816B3B"

-- pre-image of 'someDatumHashInWitness'
someDatumInWitness :: BinaryData
someDatumInWitness = unsafeBinaryDataFromBytes $ unsafeDecodeBase16
    "D8799F581C1C0BFECF6C69E1D0D540897ECC0B66844B5FB5030FB488672A47F6\
    \FB8040401A001E8480D87980FF"

-- pre-image of 'someDatumHashInOutput'
someDatumInOutput :: BinaryData
someDatumInOutput = unsafeBinaryDataFromBytes $ unsafeDecodeBase16
    "51616E6F746865722063686F636F6C617465"

someScriptHashInMetadata :: ScriptHash
someScriptHashInMetadata = unsafeScriptHashFromBytes $ unsafeDecodeBase16
    "1EBA0CF9DE6B645BA679BEBCD414FB3CCE6A1F3B3DD565BF60489D2D"

-- pre-image of 'someScriptHashInMetadata'
someScriptInMetadata :: Script
someScriptInMetadata = unsafeScriptFromBytes $ unsafeDecodeBase16
    "0082018282051A0A7E63608200581C0FCDF16CBF1AEB749C10A1AEDEEDBDB642\
    \E03C6F51CA92F4514B9070"

someScriptHashInOutput :: ScriptHash
someScriptHashInOutput = unsafeScriptHashFromBytes $ unsafeDecodeBase16
    "945886F2DF73D41D1387D421ACFA5399DE9788BD491AA715B3679867"

-- pre-image of 'someScriptHashInOutput'
someScriptInOutput :: Script
someScriptInOutput = unsafeScriptFromBytes $ unsafeDecodeBase16
    "015840010000332233322222253353004333573466EBC00C00801801440204C9\
    \8D4C01CCD5CE2481094E6F7420457175616C0000849848800848800480044800\
    \480041"

someScriptHashInWitness :: ScriptHash
someScriptHashInWitness = unsafeScriptHashFromBytes $ unsafeDecodeBase16
    "B5F64911ECF7F2E99F52B7F8519DCC4CAC23A0711597068894538337"

-- pre-image of 'someScriptHashInWitness'
someScriptInWitness :: Script
someScriptInWitness = unsafeScriptFromBytes $ unsafeDecodeBase16
    "00830304868200581C1D240D05167CC83F78CC57609607A552BE117E7737B8BD\
    \F20817C7B28200581C9DF98CBE67E25AAD5D1AA7FF895998FC44E6ED6DB2B1D4\
    \03A21C4C358200581C61729ADA10CB50F189CEE642C4EE84D35384EFCF6C4338\
    \57D56769298200581C282E27CA11BD3E4BE1A65AAE24C7BE5BF4B33E9DE338C8\
    \5ED76DBB6D8200581C1905A56D43DF143519B1B89CE9AF7188343410EE9F41C3\
    \3D97F50BFC8200581CB481306EE8B782E044726452C8E68FC2D649577A2A4972\
    \BFD61A8FB9"

-- Some stake key in Shelley, present in addresses from early blocks of Shelley.
someStakeKey :: ByteString
someStakeKey = unsafeDecodeBase16
    "06e2ae44dff6770dc0f4ada3cf4cf2605008e27aecdb332ad349fda7"

-- Another stake key that can be seen in early Shelley blocks.
someOtherStakeKey :: ByteString
someOtherStakeKey = unsafeDecodeBase16
    "8c19e0c753136fd37f7bdd0ad6e24b6faa8d4ab0cb1cf8b20439b63c"
