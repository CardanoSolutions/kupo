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
    ( IOException
    )
import Data.List
    ( isInfixOf
    )
import Data.Severity
    ( HasSeverityAnnotation (..)
    , Severity (..)
    )
import Data.Time.Clock
    ( DiffTime
    )
import Kupo.Control.MonadCatch
    ( MonadCatch (..)
    )
import Kupo.Control.MonadDelay
    ( foreverCalmly
    )
import Kupo.Control.MonadLog
    ( Tracer
    , traceWith
    )
import Kupo.Control.MonadThrow
    ( MonadThrow (..)
    )
import Kupo.Data.Cardano
    ( SlotNo
    , WithOrigin (..)
    )
import Kupo.Data.ChainSync
    ( IntersectionNotFoundException (..)
    )
import Network.WebSockets
    ( ConnectionException (..)
    )
import System.IO.Error
    ( isDoesNotExistError
    )

withChainSyncExceptionHandler
    :: Tracer IO TraceChainSync
    -> ConnectionStatusToggle IO
    -> IO ()
    -> IO Void
withChainSyncExceptionHandler tr ConnectionStatusToggle{toggleDisconnected} io =
    foreverCalmly (handleExceptions (io $> 42))
  where
    handleExceptions :: IO DiffTime -> IO DiffTime
    handleExceptions
        = handle (onRetryableException 5 isRetryableIOException)
        . handle (onRetryableException 5 isRetryableConnectionException)
        . handle (onRetryableException 0 isRetryableIntersectionNotFoundException)
        . (`onException` toggleDisconnected)

    onRetryableException :: Exception e => DiffTime -> (e -> Bool) -> e -> IO DiffTime
    onRetryableException retryingIn isRetryable e
        | isRetryable e = do
            traceWith tr $ ChainSyncFailedToConnectOrConnectionLost{retryingIn}
            pure retryingIn
        | otherwise = do
            throwIO e

    isRetryableIOException :: IOException -> Bool
    isRetryableIOException e =
        isResourceVanishedError e || isDoesNotExistError e || isTryAgainError e || isInvalidArgumentOnSocket e
      where
        isTryAgainError :: IOException -> Bool
        isTryAgainError = isInfixOf "resource exhausted" . show

        isResourceVanishedError :: IOException -> Bool
        isResourceVanishedError = isInfixOf "resource vanished" . show

        -- NOTE: MacOS
        isInvalidArgumentOnSocket :: IOException -> Bool
        isInvalidArgumentOnSocket = isInfixOf "invalid argument (Socket operation on non-socket)" . show

    isRetryableConnectionException :: ConnectionException -> Bool
    isRetryableConnectionException = \case
        CloseRequest{} -> True
        ConnectionClosed{} -> True
        ParseException{} -> False
        UnicodeException{} -> False

    isRetryableIntersectionNotFoundException :: IntersectionNotFoundException -> Bool
    isRetryableIntersectionNotFoundException = \case
        ForcedIntersectionNotFound{} -> True
        IntersectionNotFound{} -> False

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
