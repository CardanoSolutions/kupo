-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.Data.Http.OrderMatchesBySpec
    ( spec
    ) where

import Kupo.Prelude

import Kupo.Data.Http.OrderMatchesBy
    ( OrderMatchesBy (..)
    , orderMatchesBy
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
    context "orderMatchesBy" $ do
        forM_ matrix $ \(queryParams, expectation) -> do
            specify (renderQueryParams queryParams expectation) $ do
                orderMatchesBy queryParams `shouldBe` expectation
          where
            matrix =
                [ ( []
                  , Just NoExplicitOrder
                  )
                , ( [ "foo" .= "bar" ]
                  , Just NoExplicitOrder
                  )
                , ( [ "order" .= "most_recent_first" ]
                  , Just MostRecentFirst
                  )
                , ( [ "order" .= "oldest_first" ]
                  , Just OldestFirst
                  )
                , ( [ "order" .= "foo" ]
                  , Nothing
                  )
                , ( [ "order" .= "most_recent_first", "order" .= "oldest_first" ]
                  , Nothing
                  )
                ]
