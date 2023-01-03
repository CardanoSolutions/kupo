-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.Data.Http.SlotRangeSpec
    ( spec
    ) where

import Kupo.Prelude

import Kupo.Data.Http.SlotRange
    ( slotRangeFromQueryParams
    )
import Test.Hspec
    ( Spec
    , context
    , parallel
    , shouldBe
    , specify
    )
import Test.Kupo.Data.Http.Helpers
    ( renderQueryParams
    , (.=)
    )

spec :: Spec
spec = parallel $ do
    context "slotRangeFromQueryParams" $ do
        forM_ matrix $ \(queryParams, expectation) -> do
            specify (renderQueryParams queryParams expectation) $ do
                slotRangeFromQueryParams queryParams `shouldBe` expectation
          where
            matrix =
                [ ( []
                  , Just (Nothing, Nothing)
                  )
                , ( [ "created_after" .= "14" ]
                  , Just (Just 14, Nothing)
                  )
                , ( [ "created_before" .= "42" ]
                  , Just (Nothing, Just 42)
                  )
                , ( [ "created_after" .= "14", "created_before" .= "42" ]
                  , Just (Just 14, Just 42)
                  )
                , ( [ "created_before" .= "42", "created_after" .= "14" ]
                  , Just (Just 14, Just 42)
                  )
                , ( [ "created_before" .= "42", "foo" .= "bar" ]
                  , Just (Nothing, Just 42)
                  )
                , ( [ "foo" .= "bar", "created_after" .= "14" ]
                  , Just (Just 14, Nothing)
                  )
                , ( [ "created_after" .= "14", "created_after" .= "42" ]
                  , Nothing
                  )
                , ( [ "created_after" .= "foo" ]
                  , Nothing
                  )
                , ( [ "created_before" .= "-57" ]
                  , Nothing
                  )
                ]
