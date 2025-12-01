module Kupo.Data.Cardano.BinaryData where

import Kupo.Prelude

import Kupo.Data.Cardano.DatumHash
    ( DatumHash
    )

import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Plutus.Data as Ledger
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json

type BinaryData =
    Ledger.BinaryData ConwayEra

type BinaryDataHash =
    DatumHash

hashBinaryData
    :: BinaryData
    -> BinaryDataHash
hashBinaryData  =
    Ledger.hashBinaryData
{-# INLINEABLE hashBinaryData #-}

binaryDataToJson
    :: BinaryData
    -> Json.Encoding
binaryDataToJson =
    Json.text . encodeBase16 . Ledger.originalBytes
{-# INLINABLE binaryDataToJson #-}

binaryDataToBytes
    :: BinaryData
    -> ByteString
binaryDataToBytes =
    Ledger.originalBytes . Ledger.binaryDataToData
{-# INLINABLE binaryDataToBytes #-}

binaryDataFromBytes
    :: ByteString
    -> Maybe BinaryData
binaryDataFromBytes =
    either (const Nothing) Just . Ledger.makeBinaryData . toShort
{-# INLINABLE binaryDataFromBytes #-}

unsafeBinaryDataFromBytes
    :: HasCallStack
    => ByteString
    -> BinaryData
unsafeBinaryDataFromBytes =
    either (error . toText) identity . Ledger.makeBinaryData . toShort
{-# INLINABLE unsafeBinaryDataFromBytes #-}

fromAlonzoData
    :: HasCallStack
    => Ledger.Data AlonzoEra
    -> BinaryData
fromAlonzoData =
    unsafeBinaryDataFromBytes . Ledger.originalBytes
{-# INLINEABLE fromAlonzoData #-}

fromBabbageData
    :: Ledger.Data BabbageEra
    -> BinaryData
fromBabbageData =
      Ledger.dataToBinaryData
    . Ledger.upgradeData
{-# INLINEABLE fromBabbageData #-}

fromConwayData
    :: Ledger.Data ConwayEra
    -> BinaryData
fromConwayData =
    Ledger.dataToBinaryData
{-# INLINEABLE fromConwayData #-}
