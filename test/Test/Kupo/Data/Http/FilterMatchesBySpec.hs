-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.Data.Http.FilterMatchesBySpec
    ( spec
    ) where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( AssetName
    , PolicyId
    , assetNameToText
    , policyIdToText
    , unsafeAssetNameFromBytes
    )
import Kupo.Data.Http.FilterMatchesBy
    ( FilterMatchesBy (..), filterMatchesBy )
import Test.Hspec
    ( Spec, context, parallel, shouldBe, specify )
import Test.Kupo.Data.Generators
    ( genNonEmptyAssetName, genPolicyId, generateWith )
import Test.QuickCheck
    ( suchThat )

import qualified Data.ByteString as BS
import qualified Network.HTTP.Types as Http

spec :: Spec
spec = parallel $ do
    context "filterMatchesBy" $ do
        forM_ matrix $ \(queryParams, expectation) -> do
            specify (renderQueryParams queryParams expectation) $ do
                filterMatchesBy queryParams `shouldBe` expectation
          where
            matrix =
                [ ( []
                  , Just NoFilter
                  )
                , ( [ "foo" .= "bar", ("spent", Nothing) ]
                  , Just NoFilter
                  )
                , ( [ "policy_id" .= policyIdToText somePolicyId ]
                  , Just (FilterByPolicyId somePolicyId)
                  )
                , ( [ "foo" .= "bar"
                    , "policy_id" .= policyIdToText somePolicyId
                    ]
                  , Just (FilterByPolicyId somePolicyId)
                  )
                , ( [ "policy_id" .= policyIdToText somePolicyId
                    , "asset_name" .= assetNameToText someAssetName
                    ]
                  , Just (FilterByAssetId (somePolicyId, someAssetName))
                  )
                , ( [ "asset_name" .= assetNameToText someAssetName
                    , "policy_id" .= policyIdToText somePolicyId
                    ]
                  , Just (FilterByAssetId (somePolicyId, someAssetName))
                  )
                , ( [ "asset_name" .= assetNameToText someAssetName
                    ]
                  , Nothing
                  )
                , ( [ "policy_id" .= policyIdToText someOtherPolicyId
                    , "policy_id" .= policyIdToText somePolicyId
                    ]
                  , Nothing
                  )
                , ( [ "policy_id" .= policyIdToText someOtherPolicyId
                    , "asset_name" .= assetNameToText someAssetName
                    , "asset_name" .= assetNameToText someOtherAssetName
                    ]
                  , Nothing
                  )
                ]

--
-- Fixtures
--

somePolicyId :: PolicyId
somePolicyId =
    generateWith 42 genPolicyId

someOtherPolicyId :: PolicyId
someOtherPolicyId =
    generateWith 42 $ genPolicyId `suchThat` (/= somePolicyId)

someAssetName :: AssetName
someAssetName =
    generateWith 42 $
        unsafeAssetNameFromBytes <$> genNonEmptyAssetName

someOtherAssetName :: AssetName
someOtherAssetName =
    generateWith 42 $
        (unsafeAssetNameFromBytes <$> genNonEmptyAssetName) `suchThat` (/= someAssetName)

--
-- Helpers
--

(.=) :: ByteString -> Text -> (ByteString, Maybe ByteString)
(.=) key val = (key, Just (encodeUtf8 val))

renderQueryParams :: Http.Query -> Maybe expectation -> String
renderQueryParams queryParams expectation =
    let
        symbol = maybe "x" (const "✓") expectation
        queryStr = "?" <> BS.intercalate "&" (renderParam <$> queryParams)
     in
        symbol <> "   " <> decodeUtf8 queryStr

renderParam :: (ByteString, Maybe ByteString) -> ByteString
renderParam = \case
    (key, Nothing) -> key
    (key, Just val) -> key <> "=" <> val
