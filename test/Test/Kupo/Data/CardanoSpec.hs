-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}

module Test.Kupo.Data.CardanoSpec
    ( spec
    ) where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( pattern GenesisPoint
    , SlotNo (..)
    , datumHashFromText
    , headerHashFromText
    , pointFromText
    , slotNoFromText
    , slotNoToText
    )
import Kupo.Data.Cardano
    ( datumHashToText )
import Test.Hspec
    ( Spec, context, parallel, shouldBe, specify )
import Test.Hspec.QuickCheck
    ( prop )
import Test.Kupo.Data.Generators
    ( genDatumHash, genSlotNo )
import Test.QuickCheck
    ( Gen, arbitrary, forAll, property, vectorOf, (===) )

import qualified Data.ByteString as BS
spec :: Spec
spec = parallel $ do
    context "pointFromText" $ do
        specify "origin" $ do
            pointFromText "origin" `shouldBe` Just GenesisPoint

        prop "{slotNo}.{blockHeaderHash}" $ \(s :: Word64) -> forAll (genBytes 32) $ \bytes ->
            let txt = show s <> "." <> encodeBase16 bytes
             in case pointFromText txt of
                    Just{}  -> property True
                    Nothing -> property False

    context "slotNoFromText ↔ slotNoToText" $ do
        prop "forall (s :: Word64). slotNoFromText (show s) === Just{}" $ \(s :: Word64) ->
            slotNoFromText (show s) === Just (SlotNo s)
        prop "forall (s :: SlotNo). slotNoFromText (slotNoToText s) === Just{}" $
            forAll genSlotNo $ \s ->
                slotNoFromText (slotNoToText s) === Just s

    context "datumHashFromText ↔ datumHashToText" $ do
        prop "forall (x :: DatumHash). datumHashFromText (datumHashToText x) === x" $
            forAll genDatumHash $ \x ->
                datumHashFromText (datumHashToText x) === Just x

    context "headerHashFromText" $ do
        prop "forall (bs :: ByteString). bs .size 32 => headerHashFromText (base16 bs) === Just{}" $
            forAll (genBytes 32) $ \bytes ->
                case headerHashFromText (encodeBase16 bytes) of
                    Just{}  -> property True
                    Nothing -> property False

--
-- Generators
--

genBytes :: Int -> Gen ByteString
genBytes n =
    BS.pack <$> vectorOf n arbitrary
