-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.PatternSpec
    ( spec
    ) where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( Address )
import Kupo.Data.Pattern
    ( Pattern (..), matching, patternFromText )
import Kupo.Data.Pattern.Fixture
    ( addresses, patterns )
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
