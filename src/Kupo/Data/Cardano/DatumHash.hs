module Kupo.Data.Cardano.DatumHash where

import Kupo.Prelude

import qualified Cardano.Ledger.Hashes as Ledger
import qualified Data.Aeson.Encoding as Json
import qualified Data.ByteString as BS

type DatumHash =
    Ledger.DataHash

datumHashToBytes
    :: DatumHash
    -> ByteString
datumHashToBytes =
    Ledger.originalBytes
{-# INLINABLE datumHashToBytes #-}

datumHashFromBytes
    :: ByteString
    -> Maybe DatumHash
datumHashFromBytes bytes
    | BS.length bytes == (digestSize @Blake2b_256) =
        Just (unsafeDatumHashFromBytes bytes)
    | otherwise =
        Nothing

datumHashToText
    :: DatumHash
    -> Text
datumHashToText =
    encodeBase16 . Ledger.originalBytes
{-# INLINABLE datumHashToText #-}

datumHashFromText
    :: Text
    -> Maybe DatumHash
datumHashFromText str =
    case datumHashFromBytes <$> decodeBase16 (encodeUtf8 str) of
        Right (Just hash) -> Just hash
        Right Nothing -> Nothing
        Left{} -> Nothing

unsafeDatumHashFromBytes
    :: HasCallStack
    => ByteString
    -> DatumHash
unsafeDatumHashFromBytes =
    Ledger.unsafeMakeSafeHash . unsafeHashFromBytes @Blake2b_256
{-# INLINABLE unsafeDatumHashFromBytes #-}

datumHashToJson
    :: DatumHash
    -> Json.Encoding
datumHashToJson =
    hashToJson . Ledger.extractHash
{-# INLINABLE datumHashToJson #-}
