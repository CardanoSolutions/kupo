-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}

-- | This module contains on-chain points and data objects issued from the **PreProd** network.
-- They're used to test specific features of Kupo from a real running network.
module Test.Kupo.Fixture where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( BinaryData
    , DatumHash
    , MetadataHash
    , Point
    , PolicyId
    , Script
    , ScriptHash
    , SlotNo
    , TransactionId
    , pattern GenesisPoint
    , unsafeBinaryDataFromBytes
    , unsafeDatumHashFromBytes
    , unsafeMetadataHashFromBytes
    , unsafePolicyIdFromBytes
    , unsafeScriptFromBytes
    , unsafeScriptHashFromBytes
    , unsafeTransactionIdFromBytes
    )
import Kupo.Data.Database
    ( Checkpoint (..)
    , pointFromRow
    )

eraBoundaries :: [(String, Point)]
eraBoundaries =
    [ ("Byron", GenesisPoint)
    , ("Shelley", lastByronPoint)
    , ("Allegra", lastShelleyPoint)
    , ("Mary", lastAllegraPoint)
    , ("Alonzo", lastMaryPoint)
    , ("Babbage", lastAlonzoPoint)
    ]

-- | Last point of the Byron era
lastByronPoint :: Point
lastByronPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        84242
    , checkpointHeaderHash = unsafeDecodeBase16
        "45899e8002b27df291e09188bfe3aeb5397ac03546a7d0ead93aa2500860f1af"
    }

-- | Last point of the Shelley era
lastShelleyPoint :: Point
lastShelleyPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        518360
    , checkpointHeaderHash = unsafeDecodeBase16
        "f9d8b6c77fedd60c3caf5de0ce63a0aeb9d1753269c9c07503d9aa09d5144481"
    }

-- | Last point of the Allegra era
lastAllegraPoint :: Point
lastAllegraPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        950340
    , checkpointHeaderHash = unsafeDecodeBase16
        "74c03af754bcde9cd242c5a168689edcab1756a3f7ae4d5dca1a31d86839c7b1"
    }

-- | Last point of the Mary era
lastMaryPoint :: Point
lastMaryPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        1382348
    , checkpointHeaderHash = unsafeDecodeBase16
        "af5fddc7d16a349e1a2af8ba89f4f5d3273955a13095b3709ef6e3db576a0b33"
    }

-- | Last point of the Alonzo era
lastAlonzoPoint :: Point
lastAlonzoPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        3542390
    , checkpointHeaderHash = unsafeDecodeBase16
        "f93e682d5b91a94d8660e748aef229c19cb285bfb9830db48941d6a78183d81f"
    }

-- | Some point that actually doesn't exist on chain. Useful to test failures to find
-- intersection.
someNonExistingPoint :: Point
someNonExistingPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        14141414
    , checkpointHeaderHash = unsafeDecodeBase16
        "0000000000000000000000000000000000000000000000000000000000000000"
    }

-- | A point that exists on-chain. There's nothing special about it other than, it exists.
somePoint :: Point
somePoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        11017324
    , checkpointHeaderHash = unsafeDecodeBase16
        "195908564a66d713bd2b71a9b1f290be6853cb31085fe7371276a35a2f8f7e62"
    }

-- | The direct ancestor of 'somePoint'.
somePointAncestor :: Point
somePointAncestor = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        11017254
    , checkpointHeaderHash = unsafeDecodeBase16
        "97349562d7dc20b95e81e3bc5d4c5aab6cb8fdfd6fc846fcea4bcb364dca7045"
    }

-- | The direct successor of 'somePoint'.
somePointSuccessor :: Point
somePointSuccessor = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        11017333
    , checkpointHeaderHash = unsafeDecodeBase16
        "28f9e744c21c5ebdcfe53e2f1dc2e996066fbc93faf8203c404b2bbd87a28306"
    }

-- | Another point that exists on-chain that is different from 'somePoint' and is located MANY blocks
-- after 'somePoint'.
someOtherPoint :: Point
someOtherPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        36492716
    , checkpointHeaderHash = unsafeDecodeBase16
        "d51095ef5405d83e7a1c82b98d12b357ba6b95f070f684bb38ab47ef90b21688"
    }

somePointNearScripts :: Point
somePointNearScripts = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        62637303
    , checkpointHeaderHash = unsafeDecodeBase16
        "1d29ed9e913c88593f0827b7579909e164541498e302da349047496d7360c74a"
    }

-- | An ancestor point near 'somePhase2FailedTransactionIdWithReturn'.
somePointNearPhase2Failure :: Point
somePointNearPhase2Failure = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        63806106
    , checkpointHeaderHash = unsafeDecodeBase16
        "154C7E0DB314D23D9C091B64F71AF4632EB4A642D273E5B06B051315C57D6EEA"
    }

-- A policy id from tokens present in a transaction output in early Alonzo
somePolicyId :: PolicyId
somePolicyId = unsafePolicyIdFromBytes $ unsafeDecodeBase16
    "469F9A74824555709042FB95AA2D11E3C7FA4EADA2FE97BC287687DD"

-- A transaction present in the first Alonzo block.
someTransactionId :: TransactionId
someTransactionId = unsafeTransactionIdFromBytes $ unsafeDecodeBase16
    "836EA71A49D79B222C3D525EB980439F13656241D9A2D6AC4F8CEBE159FCBB88"

-- A transaction id present in a block at the 3rd position
someThirdTransactionId :: TransactionId
someThirdTransactionId = unsafeTransactionIdFromBytes $ unsafeDecodeBase16
    "CA828E5361E72DB12131BDBDDAFDABDE785D82968EE7E9169EE69750E792F780"

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

-- | Some slot shortly after the start of Allegra which contains transactions with metadata.
someSlotWithMetadata :: SlotNo
someSlotWithMetadata = 18_014_527

-- | The metadata associated with 'someSlotWithMetadata'
someMetadata :: [MetadataHash]
someMetadata = unsafeMetadataHashFromBytes . unsafeDecodeBase16 <$>
    [ "A1AE526C1A228CE730E223C8AB56543F865BA8C2C991CA87D509783D4B002C35"
    , "0C7C650D6992E292A8EAFE206CE87469AC27F0B929DE414370DDDC38993A5C14"
    ]

-- | The transaction associated with the first metadata of 'someMetadata'
someTransactionIdWithMetadata :: TransactionId
someTransactionIdWithMetadata = unsafeTransactionIdFromBytes $ unsafeDecodeBase16
    "4DC5093D26FDD8D82BA30615EDA72D96B80ED28D3750D916AB1D3AB6FEE4B4B3"


-- | A transaction id of a failed (phase-2) transaction, with collateral return (in Babbage era)
somePhase2FailedTransactionIdWithReturn :: TransactionId
somePhase2FailedTransactionIdWithReturn = unsafeTransactionIdFromBytes $ unsafeDecodeBase16
    "26D6C82E535507470DD0DEFE3A10597241D13C43D6B5A88068D9515C38C86969"
