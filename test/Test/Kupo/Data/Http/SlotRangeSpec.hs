-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.Data.Http.SlotRangeSpec
    ( spec
    ) where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( pointFromText
    )
import Kupo.Data.Http.SlotRange
    ( Range (..)
    , RangeField (..)
    , slotRangeFromQueryParams
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
                  , Just Whole
                  )
                , ( [ "foo" .= "bar" ]
                  , Just Whole
                  )
                , ( [ "created_after" .= "14" ]
                  , Just (After CreatedAt (Left 14))
                  )
                , ( [ "created_before" .= "42" ]
                  , Just (Before CreatedAt (Left 42))
                  )
                , ( [ "created_after" .= "14", "created_before" .= "42" ]
                  , Just (Between (CreatedAt, Left 14) (CreatedAt, Left 42))
                  )
                , ( [ "created_before" .= "42", "created_after" .= "14" ]
                  , Just (Between (CreatedAt, Left 14) (CreatedAt, Left 42))
                  )
                , ( [ "created_before" .= "42", "foo" .= "bar" ]
                  , Just (Before CreatedAt (Left 42))
                  )
                , ( [ "foo" .= "bar", "created_after" .= "14" ]
                  , Just (After CreatedAt (Left 14))
                  )
                , ( [ "created_after" .=
                        "14.0000000000000000000000000000000000000000000000000000000000000000"
                    ]
                  ,  (After CreatedAt . Right) <$> pointFromText
                        "14.0000000000000000000000000000000000000000000000000000000000000000"
                  )
                , ( [ "spent_after" .= "14" ]
                  , Just (After SpentAt (Left 14))
                  )
                , ( [ "spent_before" .= "42" ]
                  , Just (Before SpentAt (Left 42))
                  )
                , ( [ "spent_after" .= "14", "spent_before" .= "42" ]
                  , Just (Between (SpentAt, Left 14) (SpentAt, Left 42))
                  )
                , ( [ "spent_before" .= "42", "spent_after" .= "14" ]
                  , Just (Between (SpentAt, Left 14) (SpentAt, Left 42))
                  )
                , ( [ "spent_before" .= "42", "foo" .= "bar" ]
                  , Just (Before SpentAt (Left 42))
                  )
                , ( [ "foo" .= "bar", "spent_after" .= "14" ]
                  , Just (After SpentAt (Left 14))
                  )
                , ( [ "spent_after" .=
                        "14.0000000000000000000000000000000000000000000000000000000000000000"
                    ]
                  ,  (After SpentAt . Right) <$> pointFromText
                        "14.0000000000000000000000000000000000000000000000000000000000000000"
                  )
                , ( [ "created_after" .= "14", "spent_before" .= "42" ]
                  , Just (Between (CreatedAt, Left 14) (SpentAt, Left 42))
                  )
                , ( [ "spent_after" .= "14", "created_before" .= "42" ]
                  , Just (Between (SpentAt, Left 14) (CreatedAt, Left 42))
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
                , ( [ "spent_after" .= "14", "spent_after" .= "42" ]
                  , Nothing
                  )
                , ( [ "spent_after" .= "foo" ]
                  , Nothing
                  )
                , ( [ "spent_before" .= "-57" ]
                  , Nothing
                  )
                , ( [ "created_after" .= "14", "spent_after" .= "42" ]
                  , Nothing
                  )
                , ( [ "created_before" .= "14", "spent_before" .= "42" ]
                  , Nothing
                  )
                ]
