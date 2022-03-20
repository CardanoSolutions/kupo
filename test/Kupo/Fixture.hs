-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Fixture where

import Kupo.Prelude

import Kupo.Configuration
    ( StandardCrypto )
import Kupo.Control.MonadDatabase
    ( Checkpoint (..) )
import Kupo.Data.ChainSync
    ( Block, Point )
import Kupo.Data.Database
    ( pointFromRow )

somePoint :: Point (Block StandardCrypto)
somePoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        51292637
    , checkpointHeaderHash = unsafeDecodeBase16
        "2e7ee124eccbc648789008f8669695486f5727cada41b2d86d1c36355c76b771"
    }

someOtherPoint :: Point (Block StandardCrypto)
someOtherPoint = pointFromRow $ Checkpoint
    { checkpointSlotNo =
        53392903
    , checkpointHeaderHash = unsafeDecodeBase16
        "56ed3689f5a1dce99345c5ec85de8ff307fe0a31dff443e0170b9c21edbeaba5"
    }
