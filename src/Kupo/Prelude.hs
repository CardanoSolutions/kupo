--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Prelude
    ( -- * relude
      module Relude
    , FromJSON (..)
    , ToJSON (..)
    , hijackSigTerm
    , view
    , (^.)
    , (^?)
    , encodeBase16
    , decodeBase16
    , encodeBase58
    , decodeBase58
    , encodeBase64
    , decodeBase64
    , serialize'
    , defaultGenericToEncoding
    ) where

import Cardano.Binary
    ( serialize' )
import Data.Aeson
    ( Encoding, FromJSON (..), GToJSON', ToJSON (..), Zero, genericToEncoding )
import Data.ByteString.Base16
    ( decodeBase16, encodeBase16 )
import Data.ByteString.Base64
    ( decodeBase64, encodeBase64 )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Profunctor.Unsafe
    ( ( #. ) )
import GHC.Generics
    ( Rep )
import Relude hiding
    ( MVar
    , Nat
    , Option
    , STM
    , TMVar
    , TVar
    , atomically
    , catchSTM
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
import System.Posix.Signals
    ( Handler (..)
    , installHandler
    , keyboardSignal
    , raiseSignal
    , softwareTermination
    )

import qualified Data.Aeson as Json
import qualified Data.ByteString.Base58 as Base58

-- | Copied from: https://hackage.haskell.org/package/generic-lens-1.1.0.0/docs/src/Data.Generics.Internal.VL.Prism.html
infixl 8 ^?
(^?) :: s -> ((a -> Const (First a) a) -> s -> Const (First a) s) -> Maybe a
s ^? l = getFirst (fmof l (First #. Just) s)
  where fmof l' f = getConst #. l' (Const #. f)

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
    decodeUtf8. Base58.encodeBase58 Base58.bitcoinAlphabet

-- | Generic JSON encoding, with pre-defined default options.
defaultGenericToEncoding :: (Generic a, GToJSON' Encoding Zero (Rep a)) => a -> Encoding
defaultGenericToEncoding =
    genericToEncoding Json.defaultOptions
