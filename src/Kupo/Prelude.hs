{-# LANGUAGE AllowAmbiguousTypes #-}

--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Prelude
    ( -- * relude
      module Relude

      -- * JSON
    , FromJSON (..)
    , ToJSON (..)
    , eitherDecodeJson
    , defaultGenericToEncoding
    , encodeBytes
    , encodeObject
    , encodeMap

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

      -- * Serialization
    , serialize'
    , decodeFull'
    , unsafeDeserialize'

      -- * Crypto
    , Blake2b_224
    , Blake2b_256
    , Crypto
    , HASH
    , Hash (..)
    , HashAlgorithm (..)
    , StandardCrypto
    , digestSize
    , hashFromBytes
    , hashToJson
    , unsafeHashFromBytes

      -- * Ledger Eras
    , AllegraEra
    , AlonzoEra
    , BabbageEra
    , MaryEra
    , ShelleyEra

      -- * System
    , ConnectionStatusToggle (..)
    , noConnectionStatusToggle
    , hijackSigTerm
    , hSupportsANSI
    ) where

import Cardano.Binary
    ( decodeFull'
    , serialize'
    , unsafeDeserialize'
    )
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
import Data.ByteString.Base16
    ( decodeBase16
    , encodeBase16
    )
import Data.ByteString.Base64
    ( decodeBase64
    , encodeBase64
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
import Relude hiding
    ( MVar
    , Nat
    , Option
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
import System.Environment
    ( lookupEnv
    )
import System.IO
    ( hIsTerminalDevice
    )
import System.Posix.Signals
    ( Handler (..)
    , installHandler
    , keyboardSignal
    , raiseSignal
    , softwareTermination
    )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Internal as Json
import qualified Data.Aeson.Key as Json
import qualified Data.Aeson.Parser as Json
import qualified Data.Aeson.Parser.Internal as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base58 as Base58
import qualified Data.Map as Map

--
-- JSON
--

-- | Generic JSON encoding, with pre-defined default options.
defaultGenericToEncoding :: (Generic a, GToJSON' Encoding Zero (Rep a)) => a -> Json.Encoding
defaultGenericToEncoding =
    genericToEncoding Json.defaultOptions

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
