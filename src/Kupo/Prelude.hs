{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Prelude
    ( -- * relude
      module Relude

      -- * JSON
    , FromJSON (..)
    , ToJSON (..)
    , encodingToValue
    , eitherDecodeJson
    , defaultGenericToEncoding
    , encodeBytes
    , encodeObject
    , encodeMap

      -- * CBOR
    , serializeCbor
    , serializeCborLatest
    , decodeCbor
    , decodeCborLatest
    , unsafeDecodeCbor
    , decodeCborAnn
    , unsafeDecodeCborAnn
    , Binary.decCBOR
    , Binary.encCBOR

      -- * Lenses
    , Lens'
    , view
    , (^.)
    , (^?)
    , (^?!)
    , at

      -- * Extras
    , foldrWithIndex
    , next
    , nubOn
    , prev
    , safeToEnum

      -- * Encoding / Decoding
    , encodeBase16
    , decodeBase16
    , unsafeDecodeBase16
    , encodeBase58
    , decodeBase58
    , encodeBase64
    , decodeBase64

      -- * Crypto
    , Blake2b_224
    , Blake2b_256
    , Crypto
    , Era
    , HASH
    , Hash (..)
    , HashAlgorithm (..)
    , StandardCrypto
    , digestSize
    , hashFromBytes
    , hashToJson
    , unsafeHashFromBytes

      -- * Ledger Eras
    , ByronEra
    , ShelleyEra
    , AllegraEra
    , MaryEra
    , AlonzoEra
    , BabbageEra
    , ConwayEra
    , MostRecentEra

      -- * System
    , ConnectionStatusToggle (..)
    , noConnectionStatusToggle
    , hijackSigTerm
    , hSupportsANSI
    , hSetCursorColumn
    , hClearLine
    ) where

import Cardano.Crypto.Hash
    ( Blake2b_224
    , Blake2b_256
    , Hash (..)
    , HashAlgorithm (..)
    , hashFromBytes
    , sizeHash
    )
import Cardano.Ledger.Allegra
    ( AllegraEra
    )
import Cardano.Ledger.Alonzo
    ( AlonzoEra
    )
import Cardano.Ledger.Babbage
    ( BabbageEra
    )
import Cardano.Ledger.Conway
    ( ConwayEra
    )
import Cardano.Ledger.Core
    ( ByronEra
    , Era
    , eraProtVerLow
    )
import Cardano.Ledger.Crypto
    ( Crypto (HASH)
    , StandardCrypto
    )
import Cardano.Ledger.Mary
    ( MaryEra
    )
import Cardano.Ledger.Shelley
    ( ShelleyEra
    )
import Control.Arrow
    ( left
    )
import Control.Lens
    ( Lens'
    , at
    , (^?!)
    , (^?)
    )
import Data.Aeson
    ( Encoding
    , FromJSON (..)
    , GToJSON'
    , ToJSON (..)
    , Zero
    , genericToEncoding
    )
import Data.Base16.Types
    ( extractBase16
    )
import Data.ByteString.Base16
    ( decodeBase16Untyped
    )
import Data.ByteString.Base64
    ( decodeBase64
    , encodeBase64
    )
import Data.ByteString.Builder.Extra
    ( safeStrategy
    , toLazyByteStringWith
    )
import Data.Generics.Internal.VL.Lens
    ( view
    , (^.)
    )
import Data.List
    ( nubBy
    )
import Data.Sequence.Strict
    ( StrictSeq
    )
import GHC.Generics
    ( Rep
    )
import GHC.TypeLits
    ( ErrorMessage (..)
    , TypeError
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoEras
    )
import Ouroboros.Consensus.Shelley.Ledger
    ( ShelleyBlock
    )
import Relude hiding
    ( MVar
    , Nat
    , STM
    , TMVar
    , TVar
    , atomically
    , catchSTM
    , id
    , isEmptyTMVar
    , mkWeakTMVar
    , modifyTVar'
    , newEmptyMVar
    , newEmptyTMVar
    , newEmptyTMVarIO
    , newMVar
    , newTMVar
    , newTMVarIO
    , newTVar
    , newTVarIO
    , putMVar
    , putTMVar
    , readMVar
    , readTMVar
    , readTVar
    , readTVarIO
    , swapMVar
    , swapTMVar
    , takeMVar
    , takeTMVar
    , throwSTM
    , traceM
    , tryPutMVar
    , tryPutTMVar
    , tryReadMVar
    , tryReadTMVar
    , tryTakeMVar
    , tryTakeTMVar
    , writeTVar
    )
import Relude.Extra
    ( next
    , prev
    , safeToEnum
    )
import System.IO
    ( hIsTerminalDevice
    , hPutStr
    )
import System.Posix.Signals
    ( Handler (..)
    , installHandler
    , keyboardSignal
    , raiseSignal
    , softwareTermination
    )

import qualified Cardano.Ledger.Binary as Binary
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Key as Json
import qualified Data.Aeson.Parser as Json
import qualified Data.Aeson.Parser.Internal as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base58 as Base58
import qualified Data.Map as Map

--
-- JSON
--

-- | Generic JSON encoding, with pre-defined default options.
defaultGenericToEncoding :: (Generic a, GToJSON' Encoding Zero (Rep a)) => a -> Json.Encoding
defaultGenericToEncoding =
    genericToEncoding Json.defaultOptions

encodingToValue :: Json.Encoding -> Json.Value
encodingToValue =
    maybe (error "encodingToValue: failed?") identity
    . Json.decode
    . Json.encodingToLazyByteString

eitherDecodeJson
    :: (Json.Value -> Json.Parser a)
    -> LByteString
    -> Either String a
eitherDecodeJson decoder =
    left snd . Json.eitherDecodeWith Json.jsonEOF (Json.iparse decoder)

encodeMap :: (k -> Text) -> (v -> Json.Encoding) -> Map k v -> Json.Encoding
encodeMap encodeKey encodeValue =
    encodeObject . Map.foldrWithKey (\k v -> (:) (encodeKey k, encodeValue v)) []

encodeObject :: [(Text, Json.Encoding)] -> Json.Encoding
encodeObject =
    Json.pairs . foldr (\(Json.fromText -> k, v) -> (<>) (Json.pair k v)) mempty

encodeBytes :: ByteString -> Json.Encoding
encodeBytes =
    Json.text . encodeBase16

--
-- CBOR
--

serializeCbor
    :: forall (era :: Type -> Type) a. (Era (era StandardCrypto))
    => (a -> Binary.Encoding)
    -> a
    -> ByteString
serializeCbor encode =
    toStrict . toLazyByteStringWith strategy mempty . Binary.toBuilder version . encode
  where
    -- 1024 is the size of the first buffer, 4096 is the size of subsequent
    -- buffers. Chosen because they seem to give good performance. They are not
    -- sacred.
    strategy = safeStrategy 1024 4096
    version = eraProtVerLow @(era StandardCrypto)

serializeCborLatest
    :: (a -> Binary.Encoding)
    -> a
    -> ByteString
serializeCborLatest encode =
    toStrict . toLazyByteStringWith strategy mempty . Binary.toBuilder version . encode
  where
    -- 1024 is the size of the first buffer, 4096 is the size of subsequent
    -- buffers. Chosen because they seem to give good performance. They are not
    -- sacred.
    strategy = safeStrategy 1024 4096
    version = eraProtVerLow @(MostRecentEra StandardCrypto)

decodeCborAnn
    :: forall (era :: Type -> Type) a. (Era (era StandardCrypto))
    => Text
    -> (forall s. Binary.Decoder s (Binary.Annotator a))
    -> LByteString
    -> Either Binary.DecoderError a
decodeCborAnn =
    Binary.decodeFullAnnotator (eraProtVerLow @(era StandardCrypto))

unsafeDecodeCborAnn
    :: forall (era :: Type -> Type) a. (Era (era StandardCrypto), HasCallStack)
    => Text
    -> (forall s. Binary.Decoder s (Binary.Annotator a))
    -> LByteString
    -> a
unsafeDecodeCborAnn lbl decoder =
    either (error . show) identity .
        Binary.decodeFullAnnotator (eraProtVerLow @(era StandardCrypto)) lbl decoder

decodeCbor
    :: forall (era :: Type -> Type) a. (Era (era StandardCrypto))
    => Text
    -> (forall s. Binary.Decoder s a)
    -> LByteString
    -> Either Binary.DecoderError a
decodeCbor =
    Binary.decodeFullDecoder (eraProtVerLow @(era StandardCrypto))

decodeCborLatest
    :: Text
    -> (forall s. Binary.Decoder s a)
    -> LByteString
    -> Either Binary.DecoderError a
decodeCborLatest =
    Binary.decodeFullDecoder (eraProtVerLow @(MostRecentEra StandardCrypto))

unsafeDecodeCbor
    :: forall (era :: Type -> Type) a. (Era (era StandardCrypto), HasCallStack)
    => Text
    -> (forall s. Binary.Decoder s a)
    -> LByteString
    -> a
unsafeDecodeCbor lbl decoder =
    either (error . show) identity .
        Binary.decodeFullDecoder (eraProtVerLow @(era StandardCrypto)) lbl decoder

--
-- Extras
--

-- | Remove duplicates from a list based on information extracted from the
-- elements.
nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn b = nubBy (on (==) b)

-- | Like 'foldr' but provides access to the index of each element.
foldrWithIndex
        :: (Integral ix, Foldable f)
        => (ix -> a -> result -> result)
        -> result
        -> f a
        -> result
foldrWithIndex outer result xs =
    foldr (\x inner !i -> outer i x (inner (i+1))) (const result) xs 0
{-# SPECIALIZE foldrWithIndex :: (Word16 -> a -> result -> result) -> result -> [a] -> result #-}
{-# SPECIALIZE foldrWithIndex :: (Word16 -> a -> result -> result) -> result -> StrictSeq a -> result #-}

--
-- Encoding / Decoding
--

encodeBase16 :: ByteString -> Text
encodeBase16 = extractBase16 . Base16.encodeBase16
{-# INLINEABLE encodeBase16 #-}

decodeBase16 :: ByteString -> Either Text ByteString
decodeBase16 = decodeBase16Untyped
{-# INLINABLE decodeBase16 #-}

-- | An unsafe version of 'decodeBase16'. Use with caution.
unsafeDecodeBase16 :: HasCallStack => Text -> ByteString
unsafeDecodeBase16 =
    either error identity . decodeBase16 . encodeUtf8

-- | Decode a byte string from 'Base58', re-defining here to align interfaces
-- with base16 & base64.
decodeBase58 :: ByteString -> Either Text ByteString
decodeBase58 =
    maybe (Left msg) Right . Base58.decodeBase58 Base58.bitcoinAlphabet
  where
    msg = "failed to decode Base58-encoded string."

-- | Encode some byte string to 'Text' as Base58. See note on 'decodeBase58'.
encodeBase58 :: ByteString -> Text
encodeBase58 =
    decodeUtf8 . Base58.encodeBase58 Base58.bitcoinAlphabet

--
-- Crypto
--

unsafeHashFromBytes :: forall alg a. (HasCallStack, HashAlgorithm alg) => ByteString -> Hash alg a
unsafeHashFromBytes bytes
    | BS.length bytes /= digestSize @alg =
        error $ "failed to create " <> show (digestSize @alg) <> "-byte hash digest\
              \ for from bytestring: " <> encodeBase16 bytes
    | otherwise =
        UnsafeHash (toShort bytes)

digestSize :: forall alg. HashAlgorithm alg => Int
digestSize =
    fromIntegral (sizeHash (Proxy @alg))
{-# INLINABLE digestSize #-}

hashToJson :: HashAlgorithm alg => Hash alg a -> Json.Encoding
hashToJson (UnsafeHash h) =
    encodeBytes (fromShort h)

--
-- System
--

data ConnectionStatusToggle m = ConnectionStatusToggle
    { toggleConnected :: !(m ())
    , toggleDisconnected :: !(m ())
    }

-- | A 'ConnectionStatusToggle' which has no effect.
noConnectionStatusToggle :: Applicative m => ConnectionStatusToggle m
noConnectionStatusToggle =
    ConnectionStatusToggle
    { toggleConnected = pure ()
    , toggleDisconnected = pure ()
    }

-- | The runtime does not let the application terminate gracefully when a
-- SIGTERM is received. It does however for SIGINT which allows the application
-- to cleanup sub-processes.
--
-- This function install handlers for SIGTERM and turn them into SIGINT.
hijackSigTerm :: MonadIO m => m ()
hijackSigTerm =
    liftIO $ void (installHandler softwareTermination handler empty)
  where
    handler = CatchOnce (raiseSignal keyboardSignal)

-- | Check whether a target filehandle is an ANSI-capable terminal
hSupportsANSI :: Handle -> IO Bool
hSupportsANSI h = (&&) <$> hIsTerminalDevice h <*> isNotDumb
  where
    isNotDumb = (/= Just "dumb") <$> lookupEnv "TERM"

-- | Set the cursor to the given (0-based) column. Useful to override the terminal output.
hSetCursorColumn :: Handle -> Int -> IO ()
hSetCursorColumn h col = hPutStr h (csi [col + 1] "G")

-- | Clear the current line on screen.
hClearLine :: Handle -> IO ()
hClearLine h = hPutStr h (csi [2] "K")

csi :: [Int] -> [Char] -> [Char]
csi args code = "\ESC[" ++ intercalate ";" (map show args) ++ code

--
-- Type-level
--

-- | Access the last element of a type-level list.
type family LastElem xs where
    LastElem '[]       = TypeError ('Text "LastElem: empty list.")
    LastElem (x : '[]) = x
    LastElem (x : xs)  = LastElem xs

type MostRecentEra crypto = BlockEra (LastElem (CardanoEras crypto))

type family BlockEra block :: Type where
   BlockEra (ShelleyBlock protocol era) = era
