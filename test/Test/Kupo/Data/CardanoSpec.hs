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
    , headerHashFromText
    , pointFromText
    , slotNoFromText
    )
import Test.Hspec
    ( Spec, context, parallel, shouldBe, specify )
import Test.Hspec.QuickCheck
    ( prop )
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

    context "slotNoFromText" $ do
        prop "forall (s :: Word64). slotNoFromText (show s) === Just{}" $ \(s :: Word64) ->
            slotNoFromText (show s) === Just (SlotNo s)

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
