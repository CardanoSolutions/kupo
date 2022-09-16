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
    , TransactionId
    , assetNameToText
    , mkOutputReference
    , policyIdToText
    , transactionIdToText
    , unsafeAssetNameFromBytes
    )
import Kupo.Data.Http.FilterMatchesBy
    ( FilterMatchesBy (..)
    , filterMatchesBy
    )
import Test.Hspec
    ( Spec
    , context
    , parallel
    , shouldBe
    , specify
    )
import Test.Kupo.Data.Generators
    ( genNonEmptyAssetName
    , genPolicyId
    , genTransactionId
    , generateWith
    )
import Test.QuickCheck
    ( suchThat
    )

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
                , ( [ "transaction_id" .= transactionIdToText someTransactionId
                    ]
                  , Just (FilterByTransactionId someTransactionId)
                  )
                , ( [ "foo" .= "bar"
                    , "transaction_id" .= transactionIdToText someTransactionId
                    ]
                  , Just (FilterByTransactionId someTransactionId)
                  )
                , ( [ "transaction_id" .= transactionIdToText someTransactionId
                    , "output_index" .= "14"
                    ]
                  , Just (FilterByOutputReference (mkOutputReference someTransactionId 14))
                  )
                , ( [ "transaction_id" .= transactionIdToText someTransactionId
                    , "transaction_id" .= transactionIdToText someOtherTransactionId
                    ]
                  , Nothing
                  )
                , ( [ "transaction_id" .= transactionIdToText someTransactionId
                    , "output_index" .= "14"
                    , "output_index" .= "42"
                    ]
                  , Nothing
                  )
                , ( [ "output_index" .= "14"
                    ]
                  , Nothing
                  )
                , ( [ "policy_id" .= policyIdToText somePolicyId
                    , "transaction_id" .= transactionIdToText someTransactionId
                    ]
                  , Nothing
                  )
                ]

--
-- Fixtures
--

someTransactionId :: TransactionId
someTransactionId =
    generateWith 42 genTransactionId

someOtherTransactionId :: TransactionId
someOtherTransactionId =
    generateWith 42 $ genTransactionId `suchThat` (/= someTransactionId)

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

infixr 8 .=
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
