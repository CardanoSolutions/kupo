module Kupo.Data.Cardano.ScriptHash where

import Kupo.Prelude

import qualified Cardano.Ledger.Hashes as Ledger
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.ByteString as BS

type ScriptHash =
    Ledger.ScriptHash

unsafeScriptHashFromBytes
    :: HasCallStack
    => ByteString
    -> ScriptHash
unsafeScriptHashFromBytes =
    fromMaybe (error "unsafeScriptFromBytes") . scriptHashFromBytes
{-# INLINABLE unsafeScriptHashFromBytes #-}

scriptHashFromBytes
    :: ByteString
    -> Maybe ScriptHash
scriptHashFromBytes bytes
    | BS.length bytes == digestSize @Blake2b_224 =
        Just $ Ledger.ScriptHash (UnsafeHash (toShort bytes))
    | otherwise =
        Nothing

scriptHashToBytes
    :: ScriptHash
    -> ByteString
scriptHashToBytes (Ledger.ScriptHash (UnsafeHash scriptHash)) =
    fromShort scriptHash
{-# INLINABLE scriptHashToBytes #-}

scriptHashToText
    :: ScriptHash
    -> Text
scriptHashToText (Ledger.ScriptHash (UnsafeHash scriptHash)) =
    encodeBase16 (fromShort scriptHash)
{-# INLINABLE scriptHashToText #-}

scriptHashFromText
    :: Text
    -> Maybe ScriptHash
scriptHashFromText txt =
    case decodeBase16 (encodeUtf8 txt) of
        Right bytes ->
            scriptHashFromBytes bytes
        Left{} ->
            Nothing

scriptHashToJson
    :: ScriptHash
    -> Json.Encoding
scriptHashToJson =
    Json.text . scriptHashToText
{-# INLINABLE scriptHashToJson #-}
