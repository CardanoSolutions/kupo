-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.Fixture where

import Kupo.Prelude

import Kupo.Control.MonadDatabase
    ( Checkpoint (..) )
import Kupo.Data.Cardano
    ( Block, Point )
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
