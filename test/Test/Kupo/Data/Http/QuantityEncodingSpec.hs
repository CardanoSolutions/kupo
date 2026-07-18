-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.Data.Http.QuantityEncodingSpec
    ( spec
    ) where

import Kupo.Prelude

import Kupo.Data.Http.QuantityEncoding
    ( QuantityEncoding (..)
    , adjustMediaType
    , matchAcceptHeader
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
    ( hContentType
    )
import Test.Hspec
    ( Spec
    , describe
    , shouldBe
    , specify
    )

spec :: Spec
spec = do
    describe "Adjusting media type" $ do
        let headers = [(hContentType, renderHeader ("application" // "json"))]
        specify "Media type is unchanged for EncodeAsInteger" $ do
            adjustMediaType EncodeAsInteger headers `shouldBe` headers

        let headers' = [(hContentType , renderHeader ("application" // "json" /: mediaTypeParam))]
        specify "Adds 'asset-quantity' param for EncodeAsString" $ do
            adjustMediaType EncodeAsString headers `shouldBe` headers'

    describe "matchAcceptHeader" $ do
        specify "default to Integer" $ do
            matchAcceptHeader Nothing `shouldBe` EncodeAsInteger

        forM_
            [ ( "application/json", EncodeAsInteger )
            , ( "application/json;charset=utf-8", EncodeAsInteger )
            , ( "application/json;charset=UTF-8", EncodeAsInteger )
            , ( "application/json;asset-quantity=string", EncodeAsString )
            , ( "application/json;charset=utf-8;asset-quantity=string", EncodeAsString )
            , ( "application/json;charset=UTF-8;asset-quantity=string", EncodeAsString )
            , ( "application/json;asset-quantity=string;charset=utf-8", EncodeAsString )
            , ( "application/json;asset-quantity=string;charset=UTF-8", EncodeAsString )
            ] $ \(accept, expected) ->
                specify (decodeUtf8 accept <> " -> " <> show expected) $
                    matchAcceptHeader (Just accept) `shouldBe` expected
