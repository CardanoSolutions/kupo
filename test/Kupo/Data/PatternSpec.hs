-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.PatternSpec
    ( spec
    ) where

import Kupo.Prelude

import Data.List
    ( delete, (!!) )
import Kupo.Configuration
    ( StandardCrypto )
import Kupo.Data.ChainSync
    ( Address, addressFromBytes )
import Kupo.Data.Pattern
    ( Pattern (..)
    , includingBootstrap
    , matching
    , onlyShelley
    , patternFromText
    )
import Test.Hspec
    ( Spec, context, parallel, shouldBe, specify )

spec :: Spec
spec = parallel $ do
    context "patternFromText" $ forM_ patterns $ \(str, expectation, _) -> do
        specify (toString str) $ do
            patternFromText str `shouldBe` Just expectation

    context "matching" $ forM_ patterns $ \(str, p, sort -> matches) -> do
        specify (toString str) $ do
            (p `matchAll` addresses) `shouldBe` matches

--
-- Helper
--

matchAll :: Pattern crypto -> [Address crypto] -> [Address crypto]
matchAll p xs = do
    sort [ x | Just x <- (\x -> x <$ (x `matching` p)) <$> xs ]

--
-- Test Vectors
--

patterns :: [(Text, Pattern StandardCrypto, [Address StandardCrypto])]
patterns =
    [ ( "*"
      , MatchAny includingBootstrap
      , addresses
      )

    , ( "*/*"
      , MatchAny onlyShelley
      , delete (addresses !! 4) addresses
      )

    , ( "addr1vxk7c97z0pxe0m2q8sq34de65v4a7a8wzr8ygfvt7ukz26c8636cs"
      , MatchExact (addresses !! 0)
      , [ addresses !! 0
        ]
      )

    , ( "Ae2tdPwUPEZChsngv7kdy8pPWJrKSaZwYGMbAtVsSCVppWKfuHwxw3R7fd5"
      , MatchExact (addresses !! 4)
      , [ addresses !! 4
        ]
      )

    , ( "379bd7fd5493ebb21e199526b1a1b389ddb85cd70fccd4ca169bfdc4f59042ec/*"
      , MatchPayment (credentials !! 0)
      , [ addresses !! 0
        , addresses !! 2
        ]
      )

    , ( "adec17c2784d97ed403c011ab73aa32bdf74ee10ce44258bf72c256b/*"
      , MatchPayment (credentials !! 0)
      , [ addresses !! 0
        , addresses !! 2
        ]
      )

    , ( "*/379bd7fd5493ebb21e199526b1a1b389ddb85cd70fccd4ca169bfdc4f59042ec"
      , MatchDelegation (credentials !! 0)
      , [ addresses !! 3
        ]
      )

    , ( "*/stake_vkh14hkp0sncfkt76spuqydtww4r900hfmsseezztzlh9sjkkjx6d7q"
      , MatchDelegation (credentials !! 0)
      , [ addresses !! 3
        ]
      )

    , ( "addr_vkh14hkp0sncfkt76spuqydtww4r900hfmsseezztzlh9sjkkvjxtmr/\
        \script1cda3khwqv60360rp5m7akt50m6ttapacs8rqhn5w342z7r35m37"
      , MatchPaymentAndDelegation (credentials !! 0) (credentials !! 1)
      , [ addresses !! 2
        ]
      )

    , ( "stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw"
      , MatchDelegation (credentials !! 2)
      , []
      )
    , ( "script1cda3khwqv60360rp5m7akt50m6ttapacs8rqhn5w342z7r35m37/*"
      , MatchPayment (credentials !! 1)
      , [ addresses !! 1
        ]
      )
    ]

addresses :: [Address StandardCrypto]
addresses =
    [ addr
    | Just addr <- either (const Nothing) addressFromBytes . decodeBase16 <$>
        -- Payment address, from credentials#0
        [ "61adec17c2784d97ed403c011ab73aa32bdf74ee10ce44258bf72c256b"

        -- Payment address, from credentials#1
        , "71c37b1b5dc0669f1d3c61a6fddb2e8fde96be87b881c60bce8e8d542f"

        -- Delegated address, from credentials#0 & credentials#1
        , "21adec17c2784d97ed403c011ab73aa32bdf74ee10ce44258bf72c256bc37b1b5dc0669f1d3c61a6fddb2e8fde96be87b881c60bce8e8d542f"

        -- Delegated address, from credentials#2 & credentials#0
        , "01337b62cfff6403a06a3acbc34f8c46003c69fe79a3628cefa9c47251adec17c2784d97ed403c011ab73aa32bdf74ee10ce44258bf72c256b"

        -- Bootstrap, from credential#0
        , "82d818582183581c9f55ecc68118931f27652f39f44c1d583cc9b002405b84465221800ba0001a71a18634"
        ]
    ]

credentials :: [ByteString]
credentials =
    [ credential
    | Right credential <- decodeBase16 <$>
        -- Verification Key Hash
        [ "adec17c2784d97ed403c011ab73aa32bdf74ee10ce44258bf72c256b"

        -- Script Hash
        , "c37b1b5dc0669f1d3c61a6fddb2e8fde96be87b881c60bce8e8d542f"

        -- Verification Key Hash
        , "337b62cfff6403a06a3acbc34f8c46003c69fe79a3628cefa9c47251"
        ]
    ]
