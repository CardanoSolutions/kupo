-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.Data.PatternSpec
    ( spec
    ) where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( ComparableOutput
    , ExtendedOutputReference
    , Output
    , OutputReference
    , emptyMetadata
    , getPointSlotNo
    , toComparableOutput
    , unsafeBinaryDataFromBytes
    )
import Kupo.Data.PartialBlock
    ( PartialBlock (..)
    , PartialTransaction (..)
    )
import Kupo.Data.Pattern
    ( Codecs (..)
    , Match (consumed)
    , MatchBootstrap (..)
    , Pattern (..)
    , includes
    , matchBlock
    , matching
    , overlaps
    , patternFromText
    )
import Test.Hspec
    ( Spec
    , context
    , parallel
    , shouldBe
    , specify
    )
import Test.Hspec.QuickCheck
    ( prop
    )
import Test.Kupo.Data.Generators
    ( genNonGenesisPoint
    , genOutputReference
    , genTransactionId
    )
import Test.Kupo.Data.Pattern.Fixture
    ( matches
    , patterns
    )
import Test.QuickCheck
    ( Gen
    , choose
    , counterexample
    , elements
    , forAll
    , shuffle
    , vectorOf
    , (===)
    , (==>)
    )

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set

spec :: Spec
spec = parallel $ do
    context "patternFromText" $ forM_ patterns $ \(str, expectation, _) -> do
        specify (toString str) $ do
            patternFromText str `shouldBe` Just expectation

    context "matching" $ forM_ patterns $ \(str, p, matches') -> do
        specify (toString str) $ do
            let
                got = sort (p `matchAll` matches)
                expected = sort (map (second toComparableOutput) matches')
             in
                got `shouldBe` expected

    prop "p1 includes p2 => matches(p2) ⊆ matches(p1)" $
        forAll genPattern $ \p1 ->
            forAll genPattern $ \p2 ->
                p1 `includes` p2 ==>
                    let
                        m1 = Set.fromList (p1 `matchAll` matches)
                        m2 = Set.fromList (p2 `matchAll` matches)
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
                    p1 `overlaps` (Set.singleton p2)

    context "matchBlock" $ do
        prop "numbers spent inputs and their redeemers in ascending input order" $
            forAll ((,,) <$> genNonGenesisPoint <*> genTransactionId <*> genSpentInputs) $
                \(pt, txId, spent) -> do
                    let ascending = zip [0 ..] (Set.toAscList (Set.fromList spent))
                    let redeemer ix = unsafeBinaryDataFromBytes $ BS.pack [fromIntegral ix]
                    let tx = PartialTransaction
                            { id = txId
                            , inputs = spent
                            , outputs = []
                            , datums = mempty
                            , spendRedeemers = Map.fromList [(ix, redeemer ix) | (ix, _) <- ascending]
                            , scripts = mempty
                            , metadata = Nothing
                            }
                    let blk = PartialBlock { blockPoint = pt, blockBody = [tx], isDijkstraBlock = False }
                    let codecs = Codecs
                            { toResult = identity
                            , toBinaryData = \_ _ -> ()
                            , toScript = \_ _ -> ()
                            , toPolicy = \_ _ -> ()
                            }
                    let patterns' = Set.singleton (MatchAny IncludingBootstrap)
                    let got = Map.lookup (txId, Nothing, getPointSlotNo pt) $ consumed $ matchBlock codecs patterns' blk
                    got === Just
                        [ (MatchOutputReference ref, ix, Just (redeemer ix))
                        | (ix, ref) <- ascending
                        ]

--
-- Helper
--

matchAll
    :: Pattern
    -> [(ExtendedOutputReference, Output)]
    -> [(ExtendedOutputReference, ComparableOutput)]
matchAll p xs =
    [ (outRef, toComparableOutput out)
    | (outRef, out) <- xs
    , isJust (matching (fst outRef) out emptyMetadata p)
    ]

genPattern :: Gen Pattern
genPattern =
    elements [ p | (_, p, _) <- patterns ]

genSpentInputs :: Gen [OutputReference]
genSpentInputs = do
    -- At most 20 inputs, so each redeemer byte stays below 24 (one CBOR byte).
    n <- choose (1, 20)
    refs <- vectorOf n genOutputReference
    shuffle (Set.toList (Set.fromList refs))
