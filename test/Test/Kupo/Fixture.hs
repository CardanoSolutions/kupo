-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}

module Test.Kupo.Fixture where

import Kupo.Prelude

import Kupo.Control.MonadDatabase
    ( Checkpoint (..) )
import Kupo.Data.Cardano
    ( Block, pattern GenesisPoint, Point )
import Kupo.Data.Database
    ( pointFromRow )

someNonExistingPoint :: Point Block
someNonExistingPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        14141414
    , checkpointHeaderHash = unsafeDecodeBase16
        "0000000000000000000000000000000000000000000000000000000000000000"
    }

somePoint :: Point Block
somePoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        51292637
    , checkpointHeaderHash = unsafeDecodeBase16
        "2e7ee124eccbc648789008f8669695486f5727cada41b2d86d1c36355c76b771"
    }

somePointAncestor :: Point Block
somePointAncestor = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        51292617
    , checkpointHeaderHash = unsafeDecodeBase16
        "e63797c3982e9af8ab1f9ea1a60bfa47df9ead2e745adcca95ad30c6f49982a0"
    }

somePointSuccessor :: Point Block
somePointSuccessor = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        51292686
    , checkpointHeaderHash = unsafeDecodeBase16
        "0cdb5efde9aba0bbd2f0606d4ab64e4762cf13fe4ee78e0c5d956666fe6c95d3"
    }

someOtherPoint :: Point Block
someOtherPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        53392903
    , checkpointHeaderHash = unsafeDecodeBase16
        "56ed3689f5a1dce99345c5ec85de8ff307fe0a31dff443e0170b9c21edbeaba5"
    }

lastByronPoint :: Point Block
lastByronPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        1598399
    , checkpointHeaderHash = unsafeDecodeBase16
        "7e16781b40ebf8b6da18f7b5e8ade855d6738095ef2f1c58c77e88b6e45997a4"
    }

lastShelleyPoint :: Point Block
lastShelleyPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        13694363
    , checkpointHeaderHash = unsafeDecodeBase16
        "b596f9739b647ab5af901c8fc6f75791e262b0aeba81994a1d622543459734f2"
    }

lastAllegraPoint :: Point Block
lastAllegraPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        18014387
    , checkpointHeaderHash = unsafeDecodeBase16
        "9914c8da22a833a777d8fc1f735d2dbba70b99f15d765b6c6ee45fe322d92d93"
    }

lastMaryPoint :: Point Block
lastMaryPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        36158304
    , checkpointHeaderHash = unsafeDecodeBase16
        "2b95ce628d36c3f8f37a32c2942b48e4f9295ccfe8190bcbc1f012e1e97c79eb"
    }

lastAlonzoPoint :: Point Block
lastAlonzoPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        62510369
    , checkpointHeaderHash = unsafeDecodeBase16
        "d931221f9bc4cae34de422d9f4281a2b0344e86aac6b31eb54e2ee90f44a09b9"
    }

eraBoundaries :: [(String, Point Block)]
eraBoundaries =
    [ ("Byron", GenesisPoint)
    , ("Shelley", lastByronPoint)
    , ("Allegra", lastShelleyPoint)
    , ("Mary", lastAllegraPoint)
    , ("Alonzo", lastMaryPoint)
    , ("Babbage", lastAlonzoPoint)
    ]
