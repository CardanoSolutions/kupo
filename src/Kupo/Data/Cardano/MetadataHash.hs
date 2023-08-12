module Kupo.Data.Cardano.MetadataHash where

import Kupo.Prelude

import Cardano.Crypto.Hash
    ( hashFromTextAsHex
    )

import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Data.Aeson as Json

type MetadataHash =
    Ledger.SafeHash StandardCrypto Ledger.EraIndependentTxAuxData

unsafeMetadataHashFromBytes :: HasCallStack => ByteString -> MetadataHash
unsafeMetadataHashFromBytes =
    Ledger.unsafeMakeSafeHash
    . fromMaybe (error "unsafeMetadataHashFromBytes")
    . hashFromBytes @Blake2b_256
{-# INLINABLE unsafeMetadataHashFromBytes #-}

metadataHashToText :: MetadataHash -> Text
metadataHashToText =
    encodeBase16 . Ledger.originalBytes . Ledger.extractHash
{-# INLINABLE metadataHashToText #-}

metadataHashFromText :: Text -> Maybe MetadataHash
metadataHashFromText =
    fmap Ledger.unsafeMakeSafeHash . hashFromTextAsHex @Blake2b_256
{-# INLINABLE metadataHashFromText #-}

metadataHashToJson :: MetadataHash -> Json.Encoding
metadataHashToJson =
    hashToJson . Ledger.extractHash
{-# INLINABLE metadataHashToJson #-}
