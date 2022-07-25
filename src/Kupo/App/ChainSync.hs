--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.ChainSync
    ( -- * Types
      IntersectionNotFoundException (..)

      -- * Exception Handlers
    , withChainSyncExceptionHandler

      -- * Tracer
    , TraceChainSync (..)
    ) where

import Kupo.Prelude

import Control.Exception.Safe
    ( IOException )
import Data.List
    ( isInfixOf )
import Data.Severity
    ( HasSeverityAnnotation (..), Severity (..) )
import Data.Time.Clock
    ( DiffTime )
import Kupo.Control.MonadCatch
    ( MonadCatch (..) )
import Kupo.Control.MonadDelay
    ( foreverCalmly )
import Kupo.Control.MonadLog
    ( Tracer, traceWith )
import Kupo.Control.MonadThrow
    ( MonadThrow (..) )
import Kupo.Data.Cardano
    ( SlotNo, WithOrigin (..) )
import Kupo.Data.ChainSync
    ( IntersectionNotFoundException (..) )
import Network.WebSockets
    ( ConnectionException (..) )
import System.IO.Error
    ( isDoesNotExistError )

withChainSyncExceptionHandler
    :: Tracer IO TraceChainSync
    -> ConnectionStatusToggle IO
    -> IO ()
    -> IO ()
withChainSyncExceptionHandler tr ConnectionStatusToggle{toggleDisconnected} io =
    foreverCalmly (handleExceptions io)
  where
    handleExceptions
        = handle (onRetryableException isRetryableIOException)
        . handle (onRetryableException isRetryableConnectionException)
        . (`onException` toggleDisconnected)

    onRetryableException :: Exception e => (e -> Bool) -> e -> IO ()
    onRetryableException isRetryable e
        | isRetryable e = do
            traceWith tr $ ChainSyncFailedToConnectOrConnectionLost 5
        | otherwise = do
            throwIO e

    isRetryableIOException :: IOException -> Bool
    isRetryableIOException e =
        isResourceVanishedError e || isDoesNotExistError e || isTryAgainError e
      where
        isTryAgainError :: IOException -> Bool
        isTryAgainError = isInfixOf "resource exhausted" . show

        isResourceVanishedError :: IOException -> Bool
        isResourceVanishedError = isInfixOf "resource vanished" . show

    isRetryableConnectionException :: ConnectionException -> Bool
    isRetryableConnectionException = \case
        CloseRequest{} -> True
        ConnectionClosed{} -> True
        ParseException{} -> False
        UnicodeException{} -> False

--
-- Tracer
--

data TraceChainSync where
    ChainSyncIntersectionNotFound
        :: { points :: [WithOrigin SlotNo] }
        -> TraceChainSync
    ChainSyncFailedToConnectOrConnectionLost
        :: { retryingIn :: DiffTime }
        -> TraceChainSync
    ChainSyncUnknownException
        :: { exception :: Text }
        -> TraceChainSync
    deriving stock (Generic, Show)

instance ToJSON TraceChainSync where
    toEncoding =
        defaultGenericToEncoding

instance HasSeverityAnnotation TraceChainSync where
    getSeverityAnnotation = \case
        ChainSyncFailedToConnectOrConnectionLost{} ->
            Warning
        ChainSyncIntersectionNotFound{} ->
            Error
        ChainSyncUnknownException{} ->
            Error
