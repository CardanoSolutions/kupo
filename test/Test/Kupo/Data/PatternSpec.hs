-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.Data.PatternSpec
    ( spec
    ) where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( Address )
import Kupo.Data.Pattern
    ( Pattern (..), includes, matching, overlaps, patternFromText )
import Test.Hspec
    ( Spec, context, parallel, shouldBe, specify )
import Test.Hspec.QuickCheck
    ( prop )
import Test.Kupo.Data.Generators
    ( genPattern )
import Test.Kupo.Data.Pattern.Fixture
    ( addresses, patterns )
import Test.QuickCheck
    ( counterexample, forAll, (==>) )

import qualified Data.Set as Set

spec :: Spec
spec = parallel $ do
    context "patternFromText" $ forM_ patterns $ \(str, expectation, _) -> do
        specify (toString str) $ do
            patternFromText str `shouldBe` Just expectation

    context "matching" $ forM_ patterns $ \(str, p, sort -> matches) -> do
        specify (toString str) $ do
            (p `matchAll` addresses) `shouldBe` matches

    prop "p1 includes p2 => matches(p2) ⊆ matches(p1)" $
        forAll genPattern $ \p1 ->
            forAll genPattern $ \p2 ->
                p1 `includes` p2 ==>
                    let
                        m1 = Set.fromList (p1 `matchAll` addresses)
                        m2 = Set.fromList (p2 `matchAll` addresses)
                    in
                        (m2 `Set.isSubsetOf` m1)
                            & counterexample ("matches(p2): " <> show m2)
                            & counterexample ("matches(p1): " <> show m1)

    prop "includes is reflexive" $
        forAll genPattern $ \p1 ->
            p1 `includes` p1

    prop "includes is antisymmetric" $
        forAll genPattern $ \p1 ->
            forAll genPattern $ \p2 ->
                p1 `includes` p2 ==>
                    if p2 `includes` p1 then
                        p1 == p2
                    else
                        p1 /= p2

    prop "p1 includes p2 => p1 overlaps [p2]" $
        forAll genPattern $ \p1 ->
            forAll genPattern $ \p2 ->
                p1 `includes` p2 ==>
                    p1 `overlaps` [p2]

--
-- Helper
--

matchAll :: Pattern -> [Address] -> [Address]
matchAll p xs = do
    sort [ x | Just x <- (\x -> x <$ (x `matching` p)) <$> xs ]
