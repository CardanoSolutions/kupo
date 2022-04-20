--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Prelude
    ( -- * relude
      module Relude

      -- * JSON
    , FromJSON (..)
    , ToJSON (..)
    , byteStringToJson
    , defaultGenericToEncoding

      -- * Lenses
    , Lens'
    , view
    , (^.)
    , (^?)
    , (^?!)
    , at

      -- Encoding
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

      -- * System
    , hijackSigTerm
    , ConnectionStatusToggle (..)
    ) where

import Cardano.Binary
    ( decodeFull', serialize', unsafeDeserialize' )
import Control.Lens
    ( Lens', at, (^?), (^?!) )
import Data.Aeson
    ( Encoding, FromJSON (..), GToJSON', ToJSON (..), Zero, genericToEncoding )
import Data.ByteString.Base16
    ( decodeBase16, encodeBase16 )
import Data.ByteString.Base64
    ( decodeBase64, encodeBase64 )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
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
import qualified Data.Aeson.Encoding as Json
import qualified Data.ByteString.Base58 as Base58

data ConnectionStatusToggle m = ConnectionStatusToggle
    { toggleConnected :: m ()
    , toggleDisconnected :: m ()
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
    decodeUtf8. Base58.encodeBase58 Base58.bitcoinAlphabet

-- | Encode a ByteString to JSON as a base16 text
byteStringToJson :: ByteString -> Json.Encoding
byteStringToJson =
    Json.text . encodeBase16

-- | Generic JSON encoding, with pre-defined default options.
defaultGenericToEncoding :: (Generic a, GToJSON' Encoding Zero (Rep a)) => a -> Json.Encoding
defaultGenericToEncoding =
    genericToEncoding Json.defaultOptions
