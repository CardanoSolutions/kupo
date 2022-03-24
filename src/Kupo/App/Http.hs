--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Kupo.App.Http
    ( -- * Server
      httpServer
    , app

      -- * Client
    , healthCheck

      -- * Tracer
    , TraceHttpServer (..)
    ) where

import Kupo.Prelude

import Data.List
    ( nub, (\\) )
import Kupo.Control.MonadCatch
    ( handle )
import Kupo.Control.MonadDatabase
    ( Database (..) )
import Kupo.Control.MonadLog
    ( HasSeverityAnnotation (..), MonadLog (..), Severity (..), Tracer )
import Kupo.Control.MonadSTM
    ( MonadSTM (..) )
import Kupo.Data.Cardano
    ( pointToJson )
import Kupo.Data.Database
    ( patternToQueryLike, patternToRow, pointFromRow, resultFromRow )
import Kupo.Data.Health
    ( ConnectionStatus (..), Health )
import Kupo.Data.Pattern
    ( Pattern (..), patternFromText, patternToText, resultToJson, wildcard )
import Network.HTTP.Client
    ( defaultManagerSettings, httpLbs, newManager, parseRequest, responseBody )
import Network.HTTP.Types.Header
    ( Header, hContentLength, hContentType )
import Network.HTTP.Types.Status
    ( status200, status400, status404, status406 )
import Network.Wai
    ( Application
    , Middleware
    , Response
    , pathInfo
    , requestMethod
    , responseLBS
    , responseStatus
    , responseStream
    )
import Relude.Extra
    ( lookup )
import System.Exit
    ( ExitCode (..) )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Types.Status as Http
import qualified Network.Wai.Handler.Warp as Warp

--
-- Server
--

httpServer
    :: Tracer IO TraceHttpServer
    -> (forall a. (Database IO -> IO a) -> IO a)
    -> TVar IO [Pattern]
    -> IO Health
    -> String
    -> Int
    -> IO ()
httpServer tr withDatabase patternsVar readHealth host port =
    Warp.runSettings settings $ tracerMiddleware tr (app withDatabase patternsVar readHealth)
  where
    settings = Warp.defaultSettings
        & Warp.setPort port
        & Warp.setHost (fromString host)
        & Warp.setServerName "kupo"
        & Warp.setBeforeMainLoop (logWith tr TraceServerListening{host,port})

--
-- Router
--

app
    :: (forall a. (Database IO -> IO a) -> IO a)
    -> TVar IO [Pattern]
    -> IO Health
    -> Application
app withDatabase patternsVar readHealth req send =
    case (requestMethod req, pathInfo req) of
        ("GET", [ "v1", "health" ]) ->
            send . handleGetHealth =<< readHealth

        ("GET", [ "v1", "checkpoints" ]) ->
            withDatabase (send . handleGetCheckpoints)

        ("GET", "v1" : "matches" : args ) ->
            withDatabase (send . handleGetMatches (patternFromQuery args))

        ("GET", [ "v1", "patterns" ]) -> do
            readTVarIO patternsVar >>= send . handleGetPatterns
        ("PUT", "v1" : "patterns" : args ) ->
            withDatabase (send <=< handlePutPattern patternsVar (patternFromQuery args))
        ("DELETE", "v1" : "patterns" : args ) ->
            withDatabase (send <=< handleDeletePattern patternsVar (patternFromQuery args))
        (_, "v1" : "checkpoints" : _) ->
            send handleMethodNotAllowed
        (_, "v1" : "health" : _) ->
            send handleMethodNotAllowed
        (_, "v1" : "matches" : _) ->
            send handleMethodNotAllowed
        (_, "v1" : "patterns" : _) ->
            send handleMethodNotAllowed

        (_, _) ->
            send handleNotFound
--
-- Handlers
--

handleGetHealth
    :: Health
    -> Response
handleGetHealth =
    responseJson status200 defaultHeaders

handleGetCheckpoints
    :: Database IO
    -> Response
handleGetCheckpoints Database{..} = do
    responseStreamJson pointToJson $ \yield done -> do
        points <- runTransaction (listCheckpointsDesc pointFromRow)
        mapM_ yield points
        done

handleGetMatches
    :: Maybe Text
    -> Database IO
    -> Response
handleGetMatches query Database{..} = do
    case query >>= patternFromText of
        Nothing ->
            handleInvalidPattern
        Just p -> do
            responseStreamJson resultToJson $ \yield done -> do
                runTransaction $ foldInputsByAddress
                    (patternToQueryLike p)
                    (yield . resultFromRow)
                done

handleGetPatterns
    :: [Pattern]
    -> Response
handleGetPatterns patterns = do
    responseStreamJson Json.text $ \yield done -> do
        mapM_ (yield . patternToText) patterns
        done

handleDeletePattern
    :: TVar IO [Pattern]
    -> Maybe Text
    -> Database IO
    -> IO Response
handleDeletePattern patternsVar query Database{..} = do
    case query >>= patternFromText of
        Nothing ->
            pure handleInvalidPattern
        Just MatchAny{} -> do
            pure handleDangerousPattern
        Just p -> do
            runTransaction $ deletePattern (patternToRow p)
            n <- atomically $ do
                before <- readTVar patternsVar
                modifyTVar' patternsVar (\\ [p])
                after <- readTVar patternsVar
                pure (toInteger (length before - length after))
            pure $ responseLBS status200 defaultHeaders $
                B.toLazyByteString $ Json.fromEncoding $ Json.pairs $ mconcat
                    [ Json.pair "deleted" (Json.integer n)
                    ]

handlePutPattern
    :: TVar IO [Pattern]
    -> Maybe Text
    -> Database IO
    -> IO Response
handlePutPattern patternsVar query Database{..} = do
    case query >>= patternFromText of
        Nothing ->
            pure handleInvalidPattern
        Just p  -> do
            runTransaction $ insertPatterns [patternToRow p]
            patterns <- atomically $ do
                modifyTVar' patternsVar (nub . (p :))
                readTVar patternsVar
            pure $ responseLBS status200 defaultHeaders $
                B.toLazyByteString $ Json.fromEncoding $ Json.list
                    (Json.text . patternToText)
                    patterns

handleInvalidPattern :: Response
handleInvalidPattern = do
    responseJson status400 defaultHeaders $ HttpError
        { hint = "Invalid pattern! To fetch matches, you may provide any valid \
                 \pattern, including wildcards ('*') or full addresses. Make \
                 \sure to double-check the documentation at: \
                 \<https://cardanosolutions.github.io/kupo>!"
        }

handleDangerousPattern :: Response
handleDangerousPattern = do
    responseJson status400 defaultHeaders $ HttpError
        { hint = "Dangerous operation prevented! Wildcards patterns aren't allowed \
                 \alone when deleting pattern. If you need to delete multiple \
                 \patterns, do multiple DELETE requests."
        }

handleNotFound :: Response
handleNotFound =
    responseJson status404 defaultHeaders $ HttpError
        { hint = "Endpoint not found. Make sure to double-check the \
                 \documentation at: <https://cardanosolutions.github.io/kupo>!"
        }

handleMethodNotAllowed :: Response
handleMethodNotAllowed =
    responseJson status406 defaultHeaders $ HttpError
        { hint = "Unsupported method called on known endpoint. Make sure to \
                 \double-check the documentation at: \
                 \<https://cardanosolutions.github.io/kupo>!"
        }

--
-- Clients
--

-- | Performs a health check against a running server, this is a standalone
-- program which exits immediately, either with a success or an error code.
healthCheck :: String -> Int -> IO ()
healthCheck host port = do
    response <- handle onAnyException $ join $ httpLbs
        <$> parseRequest ("http://" <> host <> ":" <> show port <> "/v1/health")
        <*> newManager defaultManagerSettings
    case Json.decode (responseBody response) >>= getConnectionStatus of
        Just st | Json.value st == Json.toEncoding Connected ->
            return ()
        _ -> do
            exitWith (ExitFailure 1)
  where
    onAnyException (_ :: SomeException) =
        exitWith (ExitFailure 1)
    getConnectionStatus =
        lookup @(Map String Json.Value) "connection_status"

--
-- Helpers
--

data HttpError = HttpError
    { hint :: Text }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

defaultHeaders :: [Header]
defaultHeaders =
    [ ( hContentType, "application/json;charset=utf-8" )
    ]

patternFromQuery :: [Text] -> Maybe Text
patternFromQuery = \case
    [] ->
        Just wildcard
    [arg0] ->
        Just arg0
    [arg0, arg1] ->
        Just (arg0 <> "/" <> arg1)
    _ ->
        Nothing

responseJson
    :: ToJSON a
    => Http.Status
    -> [Header]
    -> a
    -> Response
responseJson status headers a =
    let
        bytes = B.toLazyByteString $ Json.fromEncoding (Json.toEncoding a)
        len = BL.length bytes
        contentLength = ( hContentLength, encodeUtf8 (show @Text len) )
     in
        responseLBS status (contentLength : headers) bytes

responseStreamJson
    :: (a -> Json.Encoding)
    -> ((a -> IO ()) -> IO () -> IO ())
    -> Response
responseStreamJson encode callback = do
    responseStream status200 defaultHeaders $ \write flush -> do
        ref <- newIORef True
        write openBracket
        callback
            (\a -> do
                isFirstResult <- readIORef ref
                write (separator isFirstResult <> Json.fromEncoding (encode a))
                writeIORef ref False
            )
            (write closeBracket >> flush)
  where
    openBracket = B.putCharUtf8 '['
    closeBracket = B.putCharUtf8 ']'
    separator isFirstResult
        | isFirstResult = mempty
        | otherwise     = B.putCharUtf8 ','

--
-- Tracer
--

tracerMiddleware :: Tracer IO TraceHttpServer -> Middleware
tracerMiddleware tr runApp req send = do
    runApp req $ \res -> do
        let status = mkStatus (responseStatus res)
        logWith tr $ TraceRequest {method, path, status}
        send res
  where
    method = decodeUtf8 (requestMethod req)
    path = pathInfo req

data TraceHttpServer where
    TraceServerListening
        :: { host :: String, port :: Int }
        -> TraceHttpServer
    TraceRequest
        :: { method :: Text, path :: [Text], status :: Status }
        -> TraceHttpServer
    deriving stock (Generic)

instance HasSeverityAnnotation TraceHttpServer where
    getSeverityAnnotation = \case
        TraceServerListening{} -> Notice
        TraceRequest{} -> Info

instance ToJSON TraceHttpServer where
    toEncoding =
        defaultGenericToEncoding

--
-- Status
--

data Status = Status
    { statusCode :: Int
    , statusMessage :: Text
    } deriving stock (Generic)
      deriving anyclass (ToJSON)

mkStatus :: Http.Status -> Status
mkStatus status = Status
    { statusCode = Http.statusCode status
    , statusMessage = decodeUtf8 (Http.statusMessage status)
    }
