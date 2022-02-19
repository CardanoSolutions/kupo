-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.PatternSpec
    ( spec
    ) where

import Kupo.Prelude

import Data.List
    ( (!!) )
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
    context "patternFromText" $ forM_ patterns $ \(str, expectation) -> do
        specify (toString str) $ do
            patternFromText str `shouldBe` Just expectation

--
-- Test Vectors
--

patterns :: [(Text, Pattern StandardCrypto)]
patterns =
    [ ( "*"
      , MatchAny includingBootstrap
      )
    , ( "*/*"
      , MatchAny onlyShelley
      )
    , ( "addr1vyc29pvl2uyzqt8nwxrcxnf558ffm27u3d9calxn8tdudjgz4xq9p"
      , MatchExact (addresses !! 0)
      )
    , ( "379bd7fd5493ebb21e199526b1a1b389ddb85cd70fccd4ca169bfdc4f59042ec/*"
      , MatchPayment (credentials !! 0)
      )
    , ( "adec17c2784d97ed403c011ab73aa32bdf74ee10ce44258bf72c256b/*"
      , MatchPayment (credentials !! 0)
      )
    , ( "*/379bd7fd5493ebb21e199526b1a1b389ddb85cd70fccd4ca169bfdc4f59042ec"
      , MatchDelegation (credentials !! 0)
      )
    , ( "*/stake_vkh14hkp0sncfkt76spuqydtww4r900hfmsseezztzlh9sjkkjx6d7q"
      , MatchDelegation (credentials !! 0)
      )
    , ( "addr_vkh14hkp0sncfkt76spuqydtww4r900hfmsseezztzlh9sjkkvjxtmr/\
        \script1cda3khwqv60360rp5m7akt50m6ttapacs8rqhn5w342z7r35m37"
      , MatchPaymentAndDelegation (credentials !! 0) (credentials !! 1)
      )
    , ( "stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw"
      , MatchDelegation (credentials !! 2)
      )
    , ( "script1cda3khwqv60360rp5m7akt50m6ttapacs8rqhn5w342z7r35m37/*"
      , MatchPayment (credentials !! 1)
      )
    ]

addresses :: [Address StandardCrypto]
addresses =
    [ addr
    | Just addr <- either (const Nothing) addressFromBytes . decodeBase16 <$>
        [ "6130a2859f5708202cf37187834d34a1d29dabdc8b4b8efcd33adbc6c9"
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
