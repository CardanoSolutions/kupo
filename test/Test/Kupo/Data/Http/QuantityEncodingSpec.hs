-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.Data.Http.QuantityEncodingSpec
    ( spec
    ) where

import Kupo.Prelude

import Kupo.Data.Http.QuantityEncoding
    ( QuantityEncoding(..)
    , adjustMediaType
    , mediaTypeParam
    )
import Network.HTTP.Media.MediaType
    ( (//)
    , (/:)
    )
import Network.HTTP.Media.RenderHeader
    ( renderHeader
    )
import Network.HTTP.Types
    ( ResponseHeaders
    , hContentType
    )
import Test.Hspec
    ( Spec
    , describe
    , shouldBe
    , specify
    )

spec :: Spec
spec =
    describe "Adjusting media type" $ do

        specify "Media type is unchanged for EncodeAsInteger" $ do
            adjustMediaType EncodeAsInteger headers `shouldBe` headers

        specify "Adds 'asset-quantity' param for EncodeAsString" $ do
            adjustMediaType EncodeAsString headers `shouldBe` headers'

headers :: ResponseHeaders
headers = [(hContentType , renderHeader ("application"//"json"))]

headers' :: ResponseHeaders
headers' = [(hContentType , renderHeader ("application"//"json"/:mediaTypeParam))]
