-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}

module Test.Kupo.Data.Generators where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( Address
    , BinaryData
    , Blake2b_224
    , Blake2b_256
    , Block
    , pattern BlockPoint
    , Datum
    , DatumHash
    , HeaderHash
    , Output
    , OutputIndex
    , OutputReference
    , Point
    , PolicyId
    , Script
    , ScriptHash
    , ScriptReference (..)
    , SlotNo (..)
    , TransactionId
    , Value
    , assetNameMaxLength
    , digestSize
    , fromBinaryData
    , fromDatumHash
    , mkOutput
    , mkOutputReference
    , noDatum
    , scriptHashToBytes
    , unsafeBinaryDataFromBytes
    , unsafeDatumHashFromBytes
    , unsafeHeaderHashFromBytes
    , unsafePolicyIdFromBytes
    , unsafeScriptFromBytes
    , unsafeScriptHashFromBytes
    , unsafeTransactionIdFromBytes
    , unsafeValueFromList
    )
import Kupo.Data.Configuration
    ( InputManagement (..) )
import Kupo.Data.Health
    ( ConnectionStatus (..), Health (..) )
import Kupo.Data.Http.ForcedRollback
    ( ForcedRollback (..), ForcedRollbackLimit (..) )
import Kupo.Data.Pattern
    ( Pattern, Result (..) )
import System.IO.Unsafe
    ( unsafePerformIO )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , arbitraryBoundedEnum
    , choose
    , elements
    , frequency
    , listOf
    , oneof
    , suchThat
    , vector
    , vectorOf
    )
import Test.QuickCheck.Gen
    ( Gen (..) )
import Test.QuickCheck.Random
    ( mkQCGen )

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Test.Kupo.Data.Pattern.Fixture as Fixture

genAddress :: Gen Address
genAddress =
    elements Fixture.addresses

genAssetName :: Gen ByteString
genAssetName = elements $
    mempty : [ generateWith seed (BS.pack <$> chooseVector (1, nMax) arbitrary)
             | seed <- [ 0 .. 10 ]
             ]
  where
    nMax = assetNameMaxLength

genNonEmptyAssetName :: Gen ByteString
genNonEmptyAssetName =
    genAssetName `suchThat` (not . BS.null)

genConnectionStatus :: Gen ConnectionStatus
genConnectionStatus =
    arbitraryBoundedEnum

genDatumHash :: Gen DatumHash
genDatumHash =
    unsafeDatumHashFromBytes . BS.pack <$> vector (digestSize @Blake2b_256)

genDatum :: Gen Datum
genDatum = frequency
    [ (1, pure noDatum)
    , (5, fromDatumHash <$> genDatumHash)
    , (5, fromBinaryData <$> genBinaryData)
    ]

genScript :: Gen Script
genScript = elements plutusScriptVectors

genScriptReference :: Gen ScriptReference
genScriptReference = frequency
    [ (1, pure NoScript)
    , (5, ReferencedScript <$> genScriptHash)
    , (3, InlineScript <$> genScript)
    ]

genScriptHash :: Gen ScriptHash
genScriptHash =
    unsafeScriptHashFromBytes . BS.pack <$> vector (digestSize @Blake2b_224)

genPolicyId :: Gen PolicyId
genPolicyId =
    unsafePolicyIdFromBytes . scriptHashToBytes <$> genScriptHash

genBinaryData :: Gen BinaryData
genBinaryData = elements plutusDataVectors

genHeaderHash :: Gen (HeaderHash Block)
genHeaderHash = do
    unsafeHeaderHashFromBytes . BS.pack <$> vector (digestSize @Blake2b_256)

genHealth :: Gen Health
genHealth = Health
    <$> genConnectionStatus
    <*> frequency [(1, pure Nothing), (5, Just <$> genSlotNo)]
    <*> frequency [(1, pure Nothing), (5, Just <$> genSlotNo)]

genNonGenesisPoint :: Gen Point
genNonGenesisPoint = do
    BlockPoint <$> genSlotNo <*> genHeaderHash

genPointsBetween :: (SlotNo, SlotNo) -> Gen [Point]
genPointsBetween (inf, sup)
    | inf >= sup = pure []
    | otherwise = do
        slotNo <- SlotNo <$> frequency
            [ (3, pure (unSlotNo inf + 1))
            , (2, choose (unSlotNo inf + 1, min (unSlotNo inf + 5) (unSlotNo sup)))
            , (1, choose (unSlotNo inf + 1, unSlotNo sup))
            ]
        pt <- BlockPoint slotNo <$> genHeaderHash
        (pt :) <$> genPointsBetween (slotNo, sup)

genOutputIndex :: Gen OutputIndex
genOutputIndex =
    fromIntegral <$> choose (0 :: Int, 255)

genOutputReference :: Gen OutputReference
genOutputReference =
    mkOutputReference <$> genTransactionId <*> genOutputIndex

genPattern :: Gen Pattern
genPattern = elements
    [ p | (_, p, _) <- Fixture.patterns ]

genResult :: Gen Result
genResult = Result
    <$> genOutputReference
    <*> genAddress
    <*> genOutputValue
    <*> genDatum
    <*> genScriptReference
    <*> genNonGenesisPoint
    <*> frequency [(1, pure Nothing), (5, Just <$> genNonGenesisPoint)]

genOutput :: Gen Output
genOutput = mkOutput
    <$> genAddress
    <*> genOutputValue
    <*> genDatum
    <*> frequency [(5, pure Nothing), (1, Just <$> genScript)]

genSlotNo :: Gen SlotNo
genSlotNo = do
    SlotNo <$> choose (1, maxBound `div` 2  - 1)

genTransactionId :: Gen TransactionId
genTransactionId =
    unsafeTransactionIdFromBytes . BS.pack <$> vector (digestSize @Blake2b_256)

-- | Generate values with non-negative quantities. When used for
-- minting/burning, values' quantities can be negative. When used in outputs,
-- they can't.
genOutputValue :: Gen Value
genOutputValue = do
    ada <- arbitrary `suchThat` (> 0)
    nPolicy <- choose (0, 3)
    nAssets <- choose (nPolicy, 3 * nPolicy)
    fmap (unsafeValueFromList ada) $ zip3
        <$> fmap cycle (vectorOf nPolicy genPolicyIdWithSmallEntropy)
        <*> vectorOf nAssets genAssetName
        <*> listOf (arbitrary `suchThat` (> 0))
  where
    genPolicyIdWithSmallEntropy :: Gen ByteString
    genPolicyIdWithSmallEntropy = elements
        [ generateWith seed (BS.pack <$> vector (digestSize @Blake2b_224))
        | seed <- [ 0 .. 10 ]
        ]

genInputManagement :: Gen InputManagement
genInputManagement =
    elements [MarkSpentInputs, RemoveSpentInputs]

genForcedRollbackLimit :: Gen ForcedRollbackLimit
genForcedRollbackLimit =
    elements [UnsafeAllowRollbackBeyondSafeZone, OnlyAllowRollbackWithinSafeZone]

genForcedRollback :: Gen ForcedRollback
genForcedRollback =
    ForcedRollback
        <$> oneof [Left <$> genSlotNo, Right <$> genNonGenesisPoint]
        <*> genForcedRollbackLimit

--
-- Helpers
--

generateWith :: Int -> Gen a -> a
generateWith seed (MkGen run) = run (mkQCGen seed) 42

chooseVector :: (Int, Int) -> Gen a -> Gen [a]
chooseVector range genA = choose range >>= (`vectorOf` genA)

plutusDataVectors :: [BinaryData]
plutusDataVectors = unsafePerformIO $ do
    let filename = "./test/vectors/binary_data.csv"
    rows <- T.splitOn "\n" . decodeUtf8 <$> BS.readFile filename
    pure
        [ unsafeBinaryDataFromBytes (unsafeDecodeBase16 bytes)
        | bytes <- rows
        , bytes /= ""
        ]
{-# NOINLINE plutusDataVectors #-}

plutusScriptVectors :: [Script]
plutusScriptVectors = unsafePerformIO $ do
    let filename = "./test/vectors/scripts.csv"
    rows <- T.splitOn "\n" . decodeUtf8 <$> BS.readFile filename
    pure
        [ unsafeScriptFromBytes (unsafeDecodeBase16 bytes)
        | bytes <- rows
        , bytes /= ""
        ]
{-# NOINLINE plutusScriptVectors #-}
