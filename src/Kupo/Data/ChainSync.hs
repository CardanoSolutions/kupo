--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.ChainSync
    ( ChainSyncHandler (..)
    , IntersectionNotFoundException (..)
    ) where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( SlotNo, WithOrigin (..) )

-- | A message handler for the chain-sync client. Messages are guaranteed (by
-- the protocol) to arrive in order.
data ChainSyncHandler m tip point block = ChainSyncHandler
    { onRollBackward :: tip -> point -> m ()
    , onRollForward :: tip -> block -> m ()
    }

-- | Exception thrown when creating a chain-sync client from an invalid list of
-- points.
data IntersectionNotFoundException = IntersectionNotFound
    { requestedPoints :: [WithOrigin SlotNo]
        -- ^ Provided points for intersection.
    , tip :: WithOrigin SlotNo
        -- ^ Current known tip of the chain.
    } deriving (Show)
instance Exception IntersectionNotFoundException

