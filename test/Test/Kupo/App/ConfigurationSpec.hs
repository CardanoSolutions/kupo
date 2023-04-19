-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- Used to partially pattern match result of parsing default arguments. Okay-ish
-- because it's test code and, having it fail would be instantly caught.
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Kupo.App.ConfigurationSpec
    ( spec
    ) where

import Kupo.Prelude

import Kupo.App.Configuration
    ( parseNetworkParameters
    )
import Kupo.Data.Configuration
    ( EpochSlots (..)
    , NetworkMagic (..)
    , NetworkParameters (..)
    , mkSystemStart
    )
import Test.Hspec
    ( Spec
    , context
    , parallel
    , shouldBe
    , specify
    )

spec :: Spec
spec = parallel $ do
    context "parseNetworkParameters" $ do
        specify "mainnet" $ do
            params <- parseNetworkParameters "./config/network/mainnet/cardano-node/config.json"
            networkMagic  params `shouldBe` NetworkMagic 764824073
            systemStart   params `shouldBe` mkSystemStart 1506203091
            slotsPerEpoch params `shouldBe` EpochSlots 21600
