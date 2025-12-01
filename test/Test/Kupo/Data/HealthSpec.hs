-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- Used to partially pattern match result of parsing default arguments. Okay-ish
-- because it's test code and, having it fail would be instantly caught.
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Kupo.Data.HealthSpec
    ( spec
    ) where

import Kupo.Prelude

import Data.Time
    ( UTCTime
    )
import Kupo.Data.Cardano
    ( SlotNo (..)
    )
import Kupo.Data.Configuration
    ( EpochSlots (..)
    , NetworkMagic (..)
    , NetworkParameters (..)
    , mkSystemStart
    )
import Kupo.Data.Health
    ( NetworkSynchronization (..)
    , mkNetworkSynchronization
    )
import Test.Hspec
    ( Spec
    , context
    , parallel
    , shouldBe
    , specify
    )

import qualified Relude.Unsafe as Unsafe

spec :: Spec
spec = parallel $ do
    let currentTime =
            Unsafe.read @UTCTime "2025-12-01 15:42:45.149803 UTC"

    context "mkNetworkSynchronization" $ do
        context "mainnet" $ do
            let networkParams = NetworkParameters
                    { networkMagic = NetworkMagic 764824073
                    , systemStart = mkSystemStart 1506203091
                    , slotsPerEpoch = EpochSlots 21600
                    }

            specify "pre-Byron" $ do
                shouldBe
                    (mkNetworkSynchronization currentTime networkParams $ SlotNo 1272240)
                    (NetworkSynchronization $ Unsafe.read "0.09847")

            specify "post-Byron (near tip)" $ do
                shouldBe
                    (mkNetworkSynchronization currentTime networkParams $ SlotNo 173022057)
                    (NetworkSynchronization $ Unsafe.read "0.99994")

            specify "post-Byron (future)" $ do
                shouldBe
                    (mkNetworkSynchronization currentTime networkParams $ SlotNo 174000000)
                    (NetworkSynchronization $ Unsafe.read "1.0")

        context "preprod" $ do
            let networkParams = NetworkParameters
                    { networkMagic = NetworkMagic 1
                    , systemStart = mkSystemStart 1654041600
                    , slotsPerEpoch = EpochSlots 21600
                    }

            specify "pre-Byron" $ do
                shouldBe
                    (mkNetworkSynchronization currentTime networkParams $ SlotNo 14000)
                    (NetworkSynchronization $ Unsafe.read "0.00253")

            specify "post-Byron (somewhat in the middle)" $ do
                shouldBe
                    (mkNetworkSynchronization currentTime networkParams $ SlotNo 58686495)
                    (NetworkSynchronization $ Unsafe.read "0.54564")

            specify "post-Byron (near tip)" $ do
                shouldBe
                    (mkNetworkSynchronization currentTime networkParams $ SlotNo 108899768)
                    (NetworkSynchronization $ Unsafe.read "0.99981")
