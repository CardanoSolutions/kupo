--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.ChainSync
    ( ForcedRollbackHandler (..)
    , IntersectionNotFoundException (..)
    , HandshakeException (..)

    -- * Pipelining Decision
    , DistanceFromTip
    , mkDistanceFromTip
    , maxInFlight
    ) where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( Point
    , SlotNo
    , Tip
    , WithOrigin (..)
    , distanceToTip
    , getPointSlotNo
    )

data ForcedRollbackHandler (m :: Type -> Type) = ForcedRollbackHandler
    { onSuccess :: !(m ())
    , onFailure :: !(m ())
    }

-- | Exception thrown when first establishing a connection with a remote cardano-node.
data HandshakeException = HandshakeException Text deriving (Show)
instance Exception HandshakeException

-- | Exception thrown when creating a chain-sync client from an invalid list of
-- points.
data IntersectionNotFoundException
    = IntersectionNotFound
        { requestedPoints :: ![WithOrigin SlotNo]
            -- ^ Provided points for intersection.
        , tip :: !(WithOrigin SlotNo)
            -- ^ Current known tip of the chain.
        }
    | ForcedIntersectionNotFound
        { point :: !(WithOrigin SlotNo)
            -- ^ Forced intersection point
        }
    deriving (Show)
instance Exception IntersectionNotFoundException

-- | A number of slots between the current cursor and the node's tip.
newtype DistanceFromTip = DistanceFromTip Integer
    deriving (Generic, Show)

-- | A smart constructor for constructing 'DistanceFromTip'
mkDistanceFromTip :: Tip -> Point -> DistanceFromTip
mkDistanceFromTip tip =
    DistanceFromTip . toInteger . distanceToTip tip . getPointSlotNo

-- | Decision has to whether pipeline an extra message. This is meant to create an 'elastic'
-- pipelining where we do pipeline many messages when catching up from a point far in the past, but
-- do not needlessly pipeline too many messages when close to the tip.
--
-- This may sound like a needless "optimization" but it is actually crucial in order to support
-- forced-rollback from clients. Indeed, if we've pipelined too many messages, we can't simply ask for
-- a new intersection before all responses to these messages have been collected! When the client is
-- already synchronized, this means it can take a long time as responses come one-by-one every ~20s or
-- so.
maxInFlight :: DistanceFromTip -> Integer
maxInFlight (DistanceFromTip d)
    | d > 6000  = 100
    | d > 1000  = 5
    | otherwise = 1
