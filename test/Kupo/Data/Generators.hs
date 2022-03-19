-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}

module Kupo.Data.Generators where

import Kupo.Prelude

import Kupo.Configuration
    ( StandardCrypto )
import Kupo.Data.ChainSync
    ( Address
    , Blake2b_224
    , Blake2b_256
    , Block
    , pattern BlockPoint
    , DatumHash
    , HeaderHash
    , OutputIndex
    , OutputReference
    , Point
    , SlotNo (..)
    , TransactionId
    , Value
    , assetNameMaxLength
    , digestSize
    , mkOutputReference
    , unsafeDatumHashFromBytes
    , unsafeHeaderHashFromBytes
    , unsafeTransactionIdFromBytes
    , unsafeValueFromList
    )
import Kupo.Data.Pattern
    ( Result (..) )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , choose
    , elements
    , frequency
    , suchThat
    , vector
    , vectorOf
    )
import Test.QuickCheck.Gen
    ( Gen (..) )
import Test.QuickCheck.Random
    ( mkQCGen )

import qualified Data.ByteString as BS
import qualified Kupo.Data.Pattern.Fixture as Fixture

genAddress :: Gen (Address StandardCrypto)
genAddress =
    elements Fixture.addresses

genAssetName :: Gen ByteString
genAssetName = elements $
    mempty : [ generateWith seed (BS.pack <$> chooseVector (1, assetNameMaxLength))
             | seed <- [ 0 .. 10 ]
             ]

genDatumHash :: Gen (DatumHash StandardCrypto)
genDatumHash =
    unsafeDatumHashFromBytes . BS.pack <$> vector (digestSize @Blake2b_256)

genHeaderHash :: Gen (HeaderHash (Block StandardCrypto))
genHeaderHash = do
    unsafeHeaderHashFromBytes . BS.pack <$> vector (digestSize @Blake2b_256)

genNonGenesisPoint :: Gen (Point (Block StandardCrypto))
genNonGenesisPoint = do
    BlockPoint <$> genSlotNo <*> genHeaderHash

genOutputIndex :: Gen OutputIndex
genOutputIndex =
    fromIntegral <$> choose (0 :: Int, 255)

genOutputReference :: Gen (OutputReference StandardCrypto)
genOutputReference =
    mkOutputReference <$> genTransactionId <*> genOutputIndex

genPolicyId :: Gen ByteString
genPolicyId = elements
    [ generateWith seed (BS.pack <$> vector (digestSize @Blake2b_224))
    | seed <- [ 0 .. 10 ]
    ]

genResult :: Gen (Result StandardCrypto)
genResult = Result
    <$> genOutputReference
    <*> genAddress
    <*> genValue
    <*> frequency [(1, pure Nothing), (5, Just <$> genDatumHash)]
    <*> genNonGenesisPoint

genSlotNo :: Gen SlotNo
genSlotNo = do
    SlotNo <$> arbitrary

genTransactionId :: Gen (TransactionId StandardCrypto)
genTransactionId =
    unsafeTransactionIdFromBytes . BS.pack <$> vector (digestSize @Blake2b_256)

genValue :: Gen (Value StandardCrypto)
genValue = do
    ada <- arbitrary `suchThat` (> 0)
    nPolicy <- choose (0, 3)
    nAssets <- choose (nPolicy, 3 * nPolicy)
    fmap (unsafeValueFromList ada) $ zip3
        <$> fmap cycle (vectorOf nPolicy genPolicyId)
        <*> vectorOf nAssets genAssetName
        <*> arbitrary

--
-- Helpers
--

generateWith :: Int -> Gen a -> a
generateWith seed (MkGen run) = run (mkQCGen seed) 42

chooseVector :: Arbitrary a => (Int, Int) -> Gen [a]
chooseVector = choose >=> vector
