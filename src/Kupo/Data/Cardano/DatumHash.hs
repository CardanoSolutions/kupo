{-# LANGUAGE TypeOperators #-}

module Kupo.Data.Cardano.DatumHash where

import Kupo.Prelude

import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Data.Aeson.Encoding as Json
import qualified Data.ByteString as BS

type DatumHash =
    DatumHash' StandardCrypto

type DatumHash' crypto =
    Ledger.DataHash crypto

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
    :: forall crypto.
        ( HasCallStack
        , HASH crypto ~ Blake2b_256
        )
    => ByteString
    -> DatumHash' crypto
unsafeDatumHashFromBytes =
    Ledger.unsafeMakeSafeHash . unsafeHashFromBytes @Blake2b_256
{-# INLINABLE unsafeDatumHashFromBytes #-}

datumHashToJson
    :: DatumHash
    -> Json.Encoding
datumHashToJson =
    hashToJson . Ledger.extractHash
{-# INLINABLE datumHashToJson #-}
