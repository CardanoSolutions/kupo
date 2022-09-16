-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}

module Test.Kupo.Data.Generators where

import Kupo.Prelude

import Data.Bits
    ( shiftL
    )
import Kupo.Data.Cardano
    ( Address
    , BinaryData
    , Blake2b_224
    , Blake2b_256
    , Block
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
    , pattern BlockPoint
    , scriptHashToBytes
    , unsafeAddressFromBytes
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
    ( InputManagement (..)
    )
import Kupo.Data.Health
    ( ConnectionStatus (..)
    , Health (..)
    )
import Kupo.Data.Http.ForcedRollback
    ( ForcedRollback (..)
    , ForcedRollbackLimit (..)
    )
import Kupo.Data.Pattern
    ( MatchBootstrap (..)
    , Pattern (..)
    , Result (..)
    )
import System.IO.Unsafe
    ( unsafePerformIO
    )
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
    ( Gen (..)
    )
import Test.QuickCheck.Random
    ( mkQCGen
    )

import qualified Data.ByteString as BS
import qualified Data.Text as T

genAddress :: Gen Address
genAddress =
    frequency
        [ (1, genBootstrapAddress)
        , (10, genNonBootstrapAddress)
        ]

genBytes :: Int -> Gen ByteString
genBytes n =
    BS.pack <$> vector n

genBootstrapAddress :: Gen Address
genBootstrapAddress = elements
    [ unsafeAddressFromBytes $ unsafeDecodeBase16
        "82d818584283581c6b968ea45ead037c0f3788e578a79e3cb8840523f3ae230257\
        \19c4d2a101581e581cca3e553c9c63c58c04a8a143c9988c3f0e15c550c69a48aa\
        \3155c182001a88c8f168"
    , unsafeAddressFromBytes $ unsafeDecodeBase16
        "82d818582183581c20e9d7a0aeb05aa9d0ea4c4b97a2214cd31e7f855abab30e60\
        \8afe19a0001a96a202b9"
    ]

genNonBootstrapAddress :: Gen Address
genNonBootstrapAddress = oneof
    [ unsafeAddressFromBytes . (addrType 0 <>) <$> genBytes (2*sz)
    , unsafeAddressFromBytes . (addrType 1 <>) <$> genBytes (2*sz)
    , unsafeAddressFromBytes . (addrType 2 <>) <$> genBytes (2*sz)
    , unsafeAddressFromBytes . (addrType 3 <>) <$> genBytes (2*sz)
    , pure $ unsafeAddressFromBytes $ unsafeDecodeBase16
        "414e67b7e16ae36d61d349493315cd92eb5d8c430502694657811a2c8e8198bd431b03"
    , pure $ unsafeAddressFromBytes $ unsafeDecodeBase16
        "519f1d3c61a6fddb2e8fdc37b1b5dc066e96be87b881c60bce8e8d542f8198bd431b03"
    , unsafeAddressFromBytes . (addrType 6 <>) <$> genBytes sz
    , unsafeAddressFromBytes . (addrType 7 <>) <$> genBytes sz
    ]
  where
    sz = digestSize @Blake2b_224
    addrType n = BS.singleton (n `shiftL` 4)

genAssetName :: Gen ByteString
genAssetName = elements $
    mempty : [ generateWith seed (choose (1, nMax) >>= genBytes)
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
    unsafeDatumHashFromBytes <$> genBytes (digestSize @Blake2b_256)

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
    unsafeScriptHashFromBytes <$> genBytes (digestSize @Blake2b_224)

genPolicyId :: Gen PolicyId
genPolicyId =
    unsafePolicyIdFromBytes . scriptHashToBytes <$> genScriptHash

genBinaryData :: Gen BinaryData
genBinaryData = elements plutusDataVectors

genHeaderHash :: Gen (HeaderHash Block)
genHeaderHash = do
    unsafeHeaderHashFromBytes <$> genBytes (digestSize @Blake2b_256)

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
genPattern = oneof
    [ MatchAny <$> elements [ IncludingBootstrap, OnlyShelley ]
    , MatchExact <$> genAddress
    , MatchPayment <$> genBytes sz
    , MatchDelegation <$> genBytes sz
    , MatchPaymentAndDelegation <$> genBytes sz <*> genBytes sz
    , MatchTransactionId <$> genTransactionId
    , MatchOutputReference <$> genOutputReference
    ]
  where
    sz = digestSize @Blake2b_224

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
    unsafeTransactionIdFromBytes <$> genBytes (digestSize @Blake2b_256)

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
        [ generateWith seed (genBytes (digestSize @Blake2b_224))
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

-- | Generate a arbitrary value with the given seed and generator.
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
