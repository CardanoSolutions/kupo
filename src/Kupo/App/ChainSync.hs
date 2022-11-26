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
import Data.Char
    ( isDigit
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
import Kupo.Control.MonadOuroboros
    ( NodeToClientVersion
    )
import Kupo.Control.MonadThrow
    ( MonadThrow (..)
    )
import Kupo.Data.Cardano
    ( SlotNo
    , WithOrigin (..)
    )
import Kupo.Data.ChainSync
    ( HandshakeException (..)
    , IntersectionNotFoundException (..)
    )
import Network.WebSockets
    ( ConnectionException (..)
    )
import Ouroboros.Network.Protocol.Handshake
    ( HandshakeProtocolError (..)
    )
import System.IO.Error
    ( isDoesNotExistError
    )

import qualified Data.Text as T
import qualified Ouroboros.Network.Protocol.Handshake as Handshake

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
        = handle onHandshakeException
        . handle (onRetryableException 5 isRetryableIOException)
        . handle (onRetryableException 5 isRetryableConnectionException)
        . handle (onRetryableException 0 isRetryableIntersectionNotFoundException)
        . (`onException` toggleDisconnected)

    onHandshakeException :: HandshakeProtocolError NodeToClientVersion -> IO a
    onHandshakeException = \case
        HandshakeError (Handshake.Refused _version reason) -> do
            let hint = case T.splitOn "/=" reason of
                    [T.filter isDigit -> remoteConfig, T.filter isDigit -> localConfig] ->
                        unwords
                            [ "Kupo is configured for", prettyNetwork localConfig
                            , "but cardano-node is running on", prettyNetwork remoteConfig <> "."
                            , "You probably want to use a different configuration."
                            ]
                    _unexpectedReasonMessage ->
                        ""
            throwIO $ HandshakeException $ unwords
                [ "Unable to establish the connection with the cardano-node:"
                , "it runs on a different network!"
                , hint
                ]
        e ->
            throwIO e
      where
        prettyNetwork = \case
            "764824073" -> "'mainnet'"
            "1097911063" -> "'testnet'"
            "1" -> "'preview'"
            "2" -> "'preprod'"
            unknownMagic -> "'network-magic=" <> unknownMagic <> "'"

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
