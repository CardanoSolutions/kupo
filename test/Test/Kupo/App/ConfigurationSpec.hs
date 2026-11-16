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

import Data.Aeson
    ( encodeFile
    , object
    , (.=)
    )
import Kupo.App.Configuration
    ( parseNetworkParameters
    )
import Kupo.Data.Configuration
    ( EpochSlots (..)
    , NetworkMagic (..)
    , NetworkParameters (..)
    , mkSystemStart
    )
import System.Directory
    ( copyFile
    , createDirectoryIfMissing
    )
import System.FilePath
    ( (</>)
    )
import System.IO.Temp
    ( withSystemTempDirectory
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

        specify "mainnet, nested under Configuration and Protocol" $ do
            let genesis = "./config/network/mainnet/genesis"
            withSystemTempDirectory "kupo-configuration" $ \dir -> do
                createDirectoryIfMissing True (dir </> "genesis")
                createDirectoryIfMissing True (dir </> "node")
                copyFile (genesis </> "byron.json") (dir </> "genesis" </> "byron.json")
                copyFile (genesis </> "shelley.json") (dir </> "genesis" </> "shelley.json")
                let configFile = dir </> "node" </> "configuration.yaml"
                encodeFile configFile $ object
                    [ "Version" .= (2 :: Int)
                    , "Configuration" .= object
                        [ "Protocol" .= object
                            [ "ByronGenesisFile" .= ("../genesis/byron.json" :: Text)
                            , "ShelleyGenesisFile" .= ("../genesis/shelley.json" :: Text)
                            ]
                        ]
                    ]
                params <- parseNetworkParameters configFile
                networkMagic  params `shouldBe` NetworkMagic 764824073
                systemStart   params `shouldBe` mkSystemStart 1506203091
                slotsPerEpoch params `shouldBe` EpochSlots 21600
