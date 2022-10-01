-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}

module Test.Kupo.Data.CardanoSpec
    ( spec
    ) where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( Blake2b_224
    , assetNameFromText
    , assetNameToText
    , datumHashFromText
    , datumHashToText
    , digest
    , hashScript
    , headerHashFromText
    , metadataFromJson
    , metadataFromText
    , metadataHashFromText
    , metadataHashToText
    , metadataToJson
    , metadataToText
    , outputReferenceFromText
    , outputReferenceToText
    , pattern GenesisPoint
    , pointFromText
    , policyIdFromText
    , policyIdToText
    , scriptFromBytes
    , scriptHashFromBytes
    , scriptHashFromText
    , scriptHashToBytes
    , scriptHashToText
    , scriptToBytes
    , slotNoFromText
    , slotNoToText
    , unsafeAssetNameFromBytes
    , unsafeScriptHashFromBytes
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
    ( genAssetName
    , genDatumHash
    , genMetadata
    , genOutputReference
    , genPolicyId
    , genScript
    , genScriptHash
    , genSlotNo
    )
import Test.QuickCheck
    ( Gen
    , arbitrary
    , counterexample
    , forAll
    , property
    , vectorOf
    , (===)
    )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL8

spec :: Spec
spec = parallel $ do
    context "pointFromText" $ do
        specify "origin" $ do
            pointFromText "origin" `shouldBe` Just GenesisPoint

        prop "{slotNo}.{blockHeaderHash}" $
            forAll genSlotNo $ \s ->
                forAll (genBytes 32) $ \bytes ->
                    let txt = slotNoToText s <> "." <> encodeBase16 bytes
                     in case pointFromText txt of
                            Just{}  -> property True
                            Nothing -> property False

    context "SlotNo" $ do
        prop "∀(s :: SlotNo). slotNoFromText (slotNoToText s) === Just{}" $
            forAll genSlotNo $ \s ->
                slotNoFromText (slotNoToText s) === Just s

    context "DatumHash" $ do
        prop "∀(x :: DatumHash). datumHashFromText (datumHashToText x) === x" $
            forAll genDatumHash $ \x ->
                datumHashFromText (datumHashToText x) === Just x

    context "Script" $ do
        prop "∀(x :: Script). scriptFromBytes (scriptToBytes x) === x" $
            forAll genScript $ \x ->
                scriptFromBytes (scriptToBytes x) === Just x
        prop "∀(x :: Script). digest @Blake2b_224 x === hashScript x" $
            forAll genScript $ \x ->
                unsafeScriptHashFromBytes (digest (Proxy @Blake2b_224) (scriptToBytes x))
                    === hashScript x

    context "ScriptHash" $ do
        prop "∀(x :: ScriptHash). scriptHashFromBytes (scriptHashToBytes x) === x" $
            forAll genScriptHash $ \x ->
                scriptHashFromBytes (scriptHashToBytes x) === Just x
        prop "∀(x :: ScriptHash). scriptFromText (scriptToText x) === x" $
            forAll genScriptHash $ \x ->
                scriptHashFromText (scriptHashToText x) === Just x

    context "HeaderHash" $ do
        prop "∀(bs :: ByteString). bs .size 32 ⇒ headerHashFromText (base16 bs) === Just{}" $
            forAll (genBytes 32) $ \bytes ->
                case headerHashFromText (encodeBase16 bytes) of
                    Just{}  -> property True
                    Nothing -> property False

    context "Metadata" $ do
        prop "∀(x :: Metadata). metadataFromText (metadataToText x) === x" $
            forAll genMetadata $ \(_, x) ->
                metadataFromText (metadataToText x) === Just x
        prop "∀(x :: Metadata). metadataFromJson (metadataToJson x) === x" $
            forAll genMetadata $ \(_, x) ->
                let encoded = Json.encodingToLazyByteString (metadataToJson x) in
                let value = Json.decode' encoded in
                case Json.parse metadataFromJson <$> value of
                    Just Json.Success{} ->
                        property True
                    Just (Json.Error hint) ->
                        property False
                            & counterexample ("Encoding: " <> BL8.unpack encoded)
                            & counterexample hint
                    Nothing ->
                        property False
                            & counterexample ("Encoding: " <> BL8.unpack encoded)
                            & counterexample "Malformed JSON encoding."

    context "MetadataHash" $ do
        prop "∀(x :: MetadataHash). metadataHashFromText (metadataHashToText x) === x" $
            forAll genMetadata $ \(x, _) ->
                metadataHashFromText (metadataHashToText x) === Just x

    context "OutputReference" $ do
        prop "∀(x :: OutputReference). outputRefFromText (outputRefToText x) == Just x" $
            forAll genOutputReference $ \x ->
                case outputReferenceFromText (outputReferenceToText x) of
                    Just{} -> property True
                    Nothing -> property False

    context "PolicyId" $ do
        prop "∀(x :: PolicyId). policyIdFromText (policyIdToText x) == Just x" $
            forAll genPolicyId $ \x ->
                    case policyIdFromText (policyIdToText x) of
                        Just{} -> property True
                        Nothing -> property False

    context "AssetName" $ do
        prop "∀(x :: AssetName). assetNameFromText (assetNameToText x) == Just x" $
            forAll genAssetName $ \(unsafeAssetNameFromBytes -> x) ->
                    case assetNameFromText (assetNameToText x) of
                        Just{} -> property True
                        Nothing -> property False

--
-- Generators
--

genBytes :: Int -> Gen ByteString
genBytes n =
    BS.pack <$> vectorOf n arbitrary
