--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Kupo.App.Http
    ( -- * Server
      httpServer
    , app

      -- * HealthCheck
    , healthCheck

      -- * Tracer
    , TraceHttpServer (..)
    ) where

import Kupo.Prelude

import Data.Aeson
    ( (.:)
    )
import Kupo.App.Database
    ( ConnectionType (..)
    , DBTransaction
    , Database (..)
    )
import Kupo.App.Http.HealthCheck
    ( healthCheck
    )
import Kupo.Control.MonadCatch
    ( MonadCatch (..)
    )
import Kupo.Control.MonadDelay
    ( threadDelay
    )
import Kupo.Control.MonadLog
    ( HasSeverityAnnotation (..)
    , MonadLog (..)
    , Severity (..)
    , Tracer
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Control.MonadThrow
    ( throwIO
    )
import Kupo.Control.MonadTime
    ( DiffTime
    )
import Kupo.Data.Cardano
    ( DatumHash
    , IsBlock (..)
    , Point
    , ScriptHash
    , SlotNo (..)
    , binaryDataToJson
    , datumHashFromText
    , distanceToSlot
    , foldBlock
    , getPoint
    , getPointHeaderHash
    , getPointSlotNo
    , getTransactionId
    , hasAssetId
    , hasPolicyId
    , headerHashToText
    , metadataToJson'
    , pattern GenesisPoint
    , pointToJson
    , scriptHashFromText
    , scriptToJson
    , slotNoFromText
    , slotNoToText
    , unsafeGetPointHeaderHash
    )
import Kupo.Data.ChainSync
    ( ForcedRollbackHandler (..)
    )
import Kupo.Data.Configuration
    ( LongestRollback (..)
    , maxReconnectionAttempts
    )
import Kupo.Data.Database
    ( mkSortDirection
    )
import Kupo.Data.FetchBlock
    ( FetchBlockClient
    )
import Kupo.Data.Health
    ( Health (..)
    , mkPrometheusMetrics
    )
import Kupo.Data.Http.FilterMatchesBy
    ( FilterMatchesBy (..)
    , filterMatchesBy
    )
import Kupo.Data.Http.ForcedRollback
    ( ForcedRollback (..)
    , ForcedRollbackLimit (..)
    , decodeForcedRollback
    )
import Kupo.Data.Http.GetCheckpointMode
    ( GetCheckpointMode (..)
    , getCheckpointModeFromQuery
    )
import Kupo.Data.Http.OrderMatchesBy
    ( orderMatchesBy
    )
import Kupo.Data.Http.Response
    ( responseJson
    , responseJsonEncoding
    , responseStreamJson
    )
import Kupo.Data.Http.SlotRange
    ( intoSlotRange
    , slotRangeFromQueryParams
    )
import Kupo.Data.Http.Status
    ( Status (..)
    , mkStatus
    )
import Kupo.Data.Http.StatusFlag
    ( statusFlagFromQueryParams
    )
import Kupo.Data.Pattern
    ( Pattern (..)
    , Result (..)
    , included
    , overlaps
    , patternFromText
    , patternToText
    , resultToJson
    , wildcard
    )
import Network.HTTP.Types
    ( hAccept
    , hContentType
    , status200
    , status304
    )
import Network.Wai
    ( Application
    , Middleware
    , Request
    , Response
    , ResponseReceived
    , pathInfo
    , queryString
    , requestHeaders
    , requestMethod
    , responseBuilder
    , responseLBS
    , responseStatus
    , strictRequestBody
    )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC.Clock
import qualified Kupo.Data.Http.Default as Default
import qualified Kupo.Data.Http.Error as Errors
import qualified Network.HTTP.Types.Header as Http
import qualified Network.HTTP.Types.URI as Http
import qualified Network.Wai.Handler.Warp as Warp

--
-- Server
--

httpServer
    :: forall block.
        ( IsBlock block
        )
    => Tracer IO TraceHttpServer
    -> (ConnectionType -> (Database IO -> IO ResponseReceived) -> IO (Maybe ResponseReceived))
    -> (Point -> ForcedRollbackHandler IO -> IO ())
    -> FetchBlockClient IO block
    -> TVar IO (Set Pattern)
    -> IO Health
    -> String
    -> Int
    -> IO ()
httpServer tr withDatabase forceRollback fetchBlock patternsVar readHealth host port =
    Warp.runSettings settings
        $ tracerMiddleware tr
        $ app withDatabaseWrapped forceRollback fetchBlock patternsVar readHealth
  where
    settings = Warp.defaultSettings
        & Warp.setPort port
        & Warp.setHost (fromString host)
        & Warp.setServerName "kupo"
        & Warp.setTimeout 120
        & Warp.setMaximumBodyFlush Nothing
        & Warp.setBeforeMainLoop (logWith tr HttpServerListening{host,port})

    withDatabaseWrapped send connectionType action = do
        retrying 1 0.025 $ handle onServerError (handle onAssertPointException (withDatabase connectionType action))
      where
        onAssertPointException = \case
            ErrPointNotFound{} ->
                Just <$> send Errors.nonExistingPoint
            ErrPointMismatch{requested, found} ->
                Just <$> send (Errors.pointMismatch requested found)

        onServerError (hint :: SomeException) = do
            logWith tr $ HttpUnexpectedError (toText $ displayException hint)
            Just <$> send Errors.serverError

        retrying attempts retryingIn io = io >>= \case
            Just r  ->
                return r
            Nothing | attempts > maxReconnectionAttempts ->
                send Errors.serviceUnavailable
            Nothing -> do
                logWith tr $ HttpFailedToOpenDatabaseConnection { attempts, retryingIn }
                threadDelay retryingIn >> retrying (next attempts) (2 * retryingIn) io

--
-- Router
--

app
    :: forall block res.
        ( IsBlock block
        , res ~ ResponseReceived
        )
    => ((Response -> IO res) -> ConnectionType -> (Database IO -> IO res) -> IO res)
    -> (Point -> ForcedRollbackHandler IO -> IO ())
    -> FetchBlockClient IO block
    -> TVar IO (Set Pattern)
    -> IO Health
    -> Application
app withDatabase forceRollback fetchBlock patternsVar readHealth req send =
    if requestMethod req == "OPTIONS" then
        send $ responseLBS status200 Default.corsHeaders mempty
    else
        route (pathInfo req)
  where
    route = \case
        ("health" : args) ->
            routeHealth (requestMethod req, args)

        ("checkpoints" : args) ->
            cacheOr readHealth req send $ flip routeCheckpoints (requestMethod req, args)

        ("matches" : args) ->
            cacheOr readHealth req send $ flip routeMatches (requestMethod req, args)

        ("datums" : args) ->
            cacheOr readHealth req send $ flip routeDatums (requestMethod req, args)

        ("scripts" : args) ->
            cacheOr readHealth req send $ flip routeScripts (requestMethod req, args)

        ("metadata" : args) ->
            cacheOr readHealth req send $ flip routeMetadata (requestMethod req, args)

        ("patterns" : args) ->
            routePatterns (requestMethod req, args)

        ("v1" : args) ->
            route args

        _unmatchedRoutes ->
            send Errors.notFound

    routeHealth = \case
        ("GET", []) -> do
            health <- readHealth
            send =<< handleGetHealth (requestHeaders req) health
        ("GET", _) ->
            send Errors.notFound
        (_, _) ->
            send Errors.methodNotAllowed

    routeCheckpoints headers = \case
        ("GET", []) ->
            withDatabase send ReadOnly $ \db -> do
                send (handleGetCheckpoints headers db)
        ("GET", [arg]) ->
            withDatabase send ReadOnly $ \db -> do
                send =<< handleGetCheckpointBySlot
                            headers
                            (slotNoFromText arg)
                            (queryString req)
                            db
        ("GET", _) ->
            send Errors.notFound
        (_, _) ->
            send Errors.methodNotAllowed

    routeMatches headers = \case
        ("GET", args) ->
            withDatabase send ReadOnly $ \db -> do
                send $ handleGetMatches
                            headers
                            (pathParametersToText args)
                            (queryString req)
                            db
        ("DELETE", args) ->
            withDatabase send ReadWrite $ \db -> do
                send =<< handleDeleteMatches
                            headers
                            patternsVar
                            (pathParametersToText args)
                            db
        (_, _) ->
            send Errors.methodNotAllowed

    routeDatums headers = \case
        ("GET", [arg]) ->
            withDatabase send ReadOnly $ \db -> do
                send =<< handleGetDatum
                            headers
                            (datumHashFromText arg)
                            db
        ("GET", _) ->
            send Errors.notFound
        (_, _) ->
            send Errors.methodNotAllowed

    routeScripts headers = \case
        ("GET", [arg]) ->
            withDatabase send ReadOnly $ \db -> do
                send =<< handleGetScript
                            headers
                            (scriptHashFromText arg)
                            db
        ("GET", _) ->
            send Errors.notFound
        (_, _) ->
            send Errors.methodNotAllowed

    routeMetadata headers = \case
        ("GET", [arg]) ->
            withDatabase send ReadOnly $ \db -> do
                send =<< handleGetMetadata
                            headers
                            (slotNoFromText arg)
                            (queryString req)
                            db
                            fetchBlock
        ("GET", _) ->
            send Errors.notFound
        (_, _) ->
            send Errors.methodNotAllowed

    routePatterns = \case
        ("GET", []) -> do
            res <- handleGetPatterns Default.headers (Just wildcard)
                        <$> fmap const (readTVarIO patternsVar)
            send res
        ("GET", args) -> do
            res <- handleGetPatterns Default.headers (pathParametersToText args)
                        <$> fmap (flip included) (readTVarIO patternsVar)
            send res
        ("PUT", []) -> do
            args <- requestBodyJson
                (\json -> do
                    pointOrSlotNo <- decodeForcedRollback json
                    patterns <- Json.withObject "" (.: "patterns") json
                    pure (pointOrSlotNo, patterns)
                ) req
            withDatabase send ReadWrite $ \db -> do
                send =<< handlePutPatterns
                            Default.headers
                            readHealth
                            forceRollback
                            patternsVar
                            (fst <$> args)
                            (snd <$> args)
                            db
        ("PUT", args) -> do
            pointOrSlotNo <- requestBodyJson decodeForcedRollback req
            withDatabase send ReadWrite $ \db -> do
                send =<< handlePutPatterns
                            Default.headers
                            readHealth
                            forceRollback
                            patternsVar
                            pointOrSlotNo
                            (pure <$> pathParametersToText args)
                            db
        ("DELETE", args) ->
            withDatabase send ReadWrite $ \db -> do
                send =<< handleDeletePattern
                            Default.headers
                            patternsVar
                            (pathParametersToText args)
                            db
        (_, _) ->
            send Errors.methodNotAllowed

-- | Handle requests that provide a `if-none-match` request header with a proper response, or route
-- the query through the normal handler path.
cacheOr
    :: IO Health
    -> Request
    -> (Response -> IO ResponseReceived)
    -> ([Http.Header] -> IO ResponseReceived)
    -> IO ResponseReceived
cacheOr readHealth req send handler = do
    health <- readHealth
    let mostRecentHeaderHash =
            (encodeUtf8 . headerHashToText) <$> (mostRecentCheckpoint health >>= getPointHeaderHash)
    case List.lookup "if-none-match" (requestHeaders req) of
        Just etag | Just etag == mostRecentHeaderHash ->
            send $ responseLBS status304 (addCacheHeaders Default.headers health) ""
        _etagMismatchOrNotPresent ->
            handler (addCacheHeaders Default.headers health)

pathParametersToText :: [Text] -> Maybe Text
pathParametersToText = \case
    [] ->
        Just wildcard
    [arg0] ->
        Just arg0
    [arg0, arg1] ->
        Just (arg0 <> "/" <> arg1)
    _unexpectedPath ->
        Nothing

--
-- /health
--

handleGetHealth
    :: [Http.Header]
    -> Health
    -> IO Response
handleGetHealth reqHeaders health =
    case findContentType reqHeaders of
        Just ct | cTextPlain `BS.isInfixOf` ct -> do
            let resHeaders = addCacheHeaders [(hContentType, cTextPlain <> ";charset=utf-8")] health
            return $ responseBuilder status200 resHeaders (mkPrometheusMetrics health)
        Just ct | cApplicationJson `BS.isInfixOf` ct -> do
            let resHeaders = addCacheHeaders Default.headers health
            return $ responseJson status200 resHeaders health
        Just ct | cAny `BS.isInfixOf` ct -> do
            let resHeaders = addCacheHeaders Default.headers health
            return $ responseBuilder status200 resHeaders (mkPrometheusMetrics health)
        Nothing -> do
            let resHeaders = addCacheHeaders Default.headers health
            return $ responseJson status200 resHeaders health
        Just{} ->
            return $ Errors.unsupportedContentType (prettyContentTypes <$> [cApplicationJson, cTextPlain])
  where
    findContentType = \case
        [] -> Nothing
        (headerName, headerValue):rest ->
            if headerName == hAccept then
                Just headerValue
            else
                findContentType rest

    cTextPlain = "text/plain"
    cApplicationJson = "application/json"
    cAny = "*/*"

    prettyContentTypes ct = decodeUtf8 ("'" <> ct <> "'")

--
-- /checkpoints
--

handleGetCheckpoints
    :: [Http.Header]
    -> Database IO
    -> Response
handleGetCheckpoints headers Database{..} = do
    responseStreamJson headers pointToJson $ \yield done -> do
        points <- runTransaction listCheckpointsDesc
        mapM_ yield points
        done

handleGetCheckpointBySlot
    :: [Http.Header]
    -> Maybe SlotNo
    -> [Http.QueryItem]
    -> Database IO
    -> IO Response
handleGetCheckpointBySlot headers mSlotNo query Database{..} =
    case (mSlotNo, getCheckpointModeFromQuery query) of
        (Nothing, _) ->
            pure Errors.invalidSlotNo
        (_, Nothing) ->
            pure Errors.invalidStrictMode
        (Just slotNo, Just mode) -> do
            handleGetCheckpointBySlot' slotNo mode
  where
    handleGetCheckpointBySlot' slotNo mode = do
        let successor = next slotNo
        points <- runTransaction (listAncestorsDesc successor 1)
        pure $ responseJsonEncoding status200 headers $ case (points, mode) of
            ([point], GetCheckpointStrict) | getPointSlotNo point == slotNo ->
                pointToJson point
            ([point], GetCheckpointClosestAncestor) ->
                pointToJson point
            _pointNotFound ->
                Json.null_

--
-- /matches
--

handleGetMatches
    :: [Http.Header]
    -> Maybe Text
    -> Http.Query
    -> Database IO
    -> Response
handleGetMatches headers patternQuery queryParams Database{..} = handleRequest $ do
    pattern_ <- (patternQuery >>= patternFromText)
        `orAbort` Errors.invalidPattern

    pointRange <- slotRangeFromQueryParams queryParams
        `orAbort` Errors.invalidSlotRange

    statusFlag <- statusFlagFromQueryParams queryParams
        `orAbort` Errors.invalidStatusFlag

    yieldIf <- (mkYieldIf pattern_ <$> filterMatchesBy queryParams)
        `orAbort` Errors.invalidMatchFilter

    sortDirection <- mkSortDirection <$> orderMatchesBy queryParams
        `orAbort` Errors.invalidSortDirection

    pure $ responseStreamJson headers resultToJson $ \yield done -> do
        let assertPointExists :: Point -> DBTransaction IO ()
            assertPointExists requested = do
                let nextSlot = next (getPointSlotNo requested)
                points <- listAncestorsDesc nextSlot 1
                case points of
                    [found] | found == requested ->
                        pure ()
                    [found] ->
                        throwIO ErrPointMismatch{requested, found}
                    _pointNotFound ->
                        throwIO ErrPointNotFound{requested}
        runTransaction $ do
            slotRange <- intoSlotRange assertPointExists assertPointExists pointRange
            foldInputs pattern_ slotRange statusFlag sortDirection (yieldIf yield)
        done
  where
    -- NOTE: kupo does support two different ways for fetching results, via query parameters or via
    -- path parameters. Historically, there were only query parameters. Yet, with the introduction
    -- of patterns, users can go either way. While this may seem redundant, it is actually useful to
    -- combine filters (one as path-parameter, and one as query-parameter).
    --
    -- Querying by path-parameter leverages database indexes and is fast. Querying by query parameter
    -- however happens _after the fact_, linearly. The cost of filtering is relatively small because
    -- of the way results are streamed. However, if the original pattern provided as path-parameter
    -- yields large queries, this may end up doing a lot of useless work.
    --
    -- Note also that when querying by asset-id as path-parameter, Kupo still performs an on-the-fly
    -- filtering for the asset id. This is because we store results by policy id and only match them
    -- by policy id when querying the database. Hence the database only yields a pre-filtered results
    -- when fetching by asset id, which still needs to be filtered out fully to discards those
    -- elements not containing the target asset id.
    mkYieldIf pattern_ filter_ =
        let
            predicateA =
                case pattern_ of
                    MatchAssetId assetId ->
                        \result -> hasAssetId (value result) assetId
                    _otherCasesAlreadyMatchedFromSQL ->
                        const True
            predicateB =
                case filter_ of
                    NoFilter ->
                        const True
                    FilterByAssetId assetId ->
                        \result -> hasAssetId (value result) assetId
                    FilterByPolicyId policyId ->
                        \result -> hasPolicyId (value result) policyId
                    FilterByOutputReference outRef ->
                        \result -> fst (outputReference result) == outRef
                    FilterByTransactionId transactionId ->
                        \result -> (getTransactionId . fst . outputReference) result == transactionId
         in
            \yield result ->
                if predicateA result && predicateB result
                then yield result
                else pure ()

handleDeleteMatches
    :: [Http.Header]
    -> TVar IO (Set Pattern)
    -> Maybe Text
    -> Database IO
    -> IO Response
handleDeleteMatches headers patternsVar query Database{..} = do
    patterns <- readTVarIO patternsVar
    case query >>= patternFromText of
        Nothing -> do
            pure Errors.invalidPattern
        Just p | p `overlaps` patterns -> do
            pure Errors.stillActivePattern
        Just p -> do
            n <- runTransaction $ deleteInputs (Set.singleton p)
            pure $ responseJsonEncoding status200 headers $
                Json.pairs $ mconcat
                    [ Json.pair "deleted" (Json.int n)
                    ]

--
-- /datums
--

handleGetDatum
    :: [Http.Header]
    -> Maybe DatumHash
    -> Database IO
    -> IO Response
handleGetDatum headers datumArg Database{..} = do
    case datumArg of
        Nothing ->
            pure Errors.malformedDatumHash
        Just datumHash -> do
            datum <- runTransaction $
                getBinaryData datumHash
            pure $ responseJsonEncoding status200 headers $
                case datum of
                    Nothing ->
                        Json.null_
                    Just d  ->
                        Json.pairs $ mconcat
                            [ Json.pair "datum" (binaryDataToJson d)
                            ]

--
-- /scripts
--

handleGetScript
    :: [Http.Header]
    -> Maybe ScriptHash
    -> Database IO
    -> IO Response
handleGetScript headers scriptArg Database{..} = do
    case scriptArg of
        Nothing ->
            pure Errors.malformedScriptHash
        Just scriptHash -> do
            script <- runTransaction $
                getScript scriptHash
            pure $ responseJsonEncoding status200 headers $
                maybe Json.null_ scriptToJson script

--
-- /metadata
--

handleGetMetadata
    :: forall block.
        ( IsBlock block
        )
    => [Http.Header]
    -> Maybe SlotNo
    -> Http.Query
    -> Database IO
    -> FetchBlockClient IO block
    -> IO Response
handleGetMetadata baseHeaders slotArg queryParams Database{..} fetchBlock =
    case slotArg of
        Nothing ->
            pure Errors.invalidSlotNo
        Just slotNo | slotNo == 0 -> do
            pure $ responseStreamJson baseHeaders metadataToJson' $ \_yield done -> done
        Just slotNo -> do
            ancestor <- runTransaction (listAncestorsDesc slotNo 1) <&> \case
                [ancestor]  -> ancestor
                _noAncestor -> GenesisPoint
            case filterMatchesBy queryParams of
                Just NoFilter ->
                    fetchFromNode ancestor Nothing
                Just (FilterByTransactionId tid) ->
                    fetchFromNode ancestor (Just tid)
                _invalidFilter -> do
                    pure Errors.invalidMetadataFilter
  where
    fetchFromNode ancestor filterBy = do
        response <- newEmptyTMVarIO
        fetchBlock ancestor $ \case
            Nothing -> do
                atomically (putTMVar response Errors.noAncestor)
            Just blk -> do
                let headers =
                        ( "X-Block-Header-Hash"
                        -- NOTE: Safe because it can't be origin (it has an ancestor)
                        , encodeUtf8 $ headerHashToText $ unsafeGetPointHeaderHash $ getPoint blk
                        ) : baseHeaders
                atomically $ putTMVar response $ responseStreamJson headers metadataToJson' $
                    \yield done -> do
                        traverse_ yield $ foldBlock
                            (\ix tx ->
                                if isNothing filterBy || Just (getTransactionId tx) == filterBy then
                                    Map.alter (const $ userDefinedMetadata @block tx) ix
                                else
                                    identity
                            )
                            mempty
                            blk
                        done
        atomically (takeTMVar response)

--
-- /patterns
--

handleGetPatterns
    :: [Http.Header]
    -> Maybe Text
    -> (Pattern -> Set Pattern)
    -> Response
handleGetPatterns headers patternQuery patterns = do
    case patternQuery >>= patternFromText of
        Nothing ->
            Errors.invalidPattern
        Just p -> do
            responseStreamJson headers Json.text $ \yield done -> do
                mapM_ (yield . patternToText) (patterns p)
                done

handleDeletePattern
    :: [Http.Header]
    -> TVar IO (Set Pattern)
    -> Maybe Text
    -> Database IO
    -> IO Response
handleDeletePattern headers patternsVar query Database{..} = do
    case query >>= patternFromText of
        Nothing ->
            pure Errors.invalidPattern
        Just p -> do
            n <- runTransaction $ deletePattern p
            atomically $ modifyTVar' patternsVar (Set.delete p)
            pure $ responseJsonEncoding status200 headers $
                Json.pairs $ mconcat
                    [ Json.pair "deleted" (Json.int n)
                    ]

handlePutPatterns
    :: [Http.Header]
    -> IO Health
    -> (Point -> ForcedRollbackHandler IO -> IO ())
    -> TVar IO (Set Pattern)
    -> Maybe ForcedRollback
    -> Maybe [Text]
    -> Database IO
    -> IO Response
handlePutPatterns headers readHealth forceRollback patternsVar mPointOrSlot query Database{..} = do
    mPoint <- traverse
        (\ForcedRollback{since, limit} -> (, limit) <$> resolvePointOrSlot since)
        mPointOrSlot
    case (traverse patternFromText <$> query, mPoint) of
        (Nothing, _) -> do
            pure Errors.invalidPattern
        (Just Nothing, _) -> do
            let validPatterns = mapMaybe (fmap patternToText . patternFromText) (fromMaybe [] query)
            pure $ Errors.invalidPatterns validPatterns
        (_, Nothing) -> do
            pure Errors.malformedPoint
        (_, Just (Nothing, _)) -> do
            pure Errors.nonExistingPoint
        (Just (Just ps), Just (Just point, lim)) -> do
            tip <- mostRecentNodeTip <$> readHealth
            let d = distanceToSlot <$> tip <*> pure (getPointSlotNo point)
            case ((LongestRollback <$> d) > Just longestRollback, lim) of
                (True, OnlyAllowRollbackWithinSafeZone) -> do
                    pure Errors.unsafeRollbackBeyondSafeZone
                _safeRollbackOrAllowedUnsafe -> do
                    ps `putPatternsAt` point
  where
    resolvePointOrSlot :: Either SlotNo Point -> IO (Maybe Point)
    resolvePointOrSlot = \case
        Right pt -> do
            let successor = next $ getPointSlotNo pt
            pts <- runTransaction $ listAncestorsDesc successor 1
            return $ case pts of
                [pt'] | pt == pt' ->
                    Just pt
                -- NOTE: It may be possible for clients to rollback to a point
                -- prior to any point we know, in which case, we keep things
                -- flexible and optimistically rollback to that point.
                [] ->
                    Just pt
                _pointDoesNotMatch ->
                    Nothing

        Left sl -> do
            let successor = next sl
            pts <- runTransaction $ listAncestorsDesc successor 1
            return $ case pts of
                [pt] ->
                    Just pt
                _unexpectedPoint ->
                    Nothing

    putPatternsAt :: [Pattern] -> Point -> IO Response
    putPatternsAt (fromList -> newPatterns) point = do
        response <- newEmptyTMVarIO
        forceRollback point $ ForcedRollbackHandler
            { onSuccess = do
                runTransaction $ insertPatterns newPatterns
                allPatterns <- atomically $ do
                    modifyTVar' patternsVar (Set.union newPatterns)
                    readTVar patternsVar
                atomically $ putTMVar response $ responseJsonEncoding status200 headers $
                    Json.list
                        (Json.text . patternToText)
                        (toList allPatterns)
            , onFailure = do
                atomically (putTMVar response Errors.failedToRollback)
            }
        atomically (takeTMVar response)

--
-- AssertPointException
--

-- | Thrown by some handlers when performing extra sanity checks on provided points.
data AssertPointException
    = ErrPointNotFound { requested :: !Point }
    | ErrPointMismatch { requested :: !Point, found :: !Point }
    deriving (Generic, Show)

instance Exception AssertPointException

--
-- Helpers
--

orAbort :: Maybe a -> Response -> Either Response a
orAbort l r = maybeToRight r l
{-# INLINABLE orAbort #-}

handleRequest :: Either Response Response -> Response
handleRequest = either identity identity
{-# INLINABLE handleRequest #-}

addCacheHeaders
    :: [Http.Header]
    -> Health
    -> [Http.Header]
addCacheHeaders defaultHeaders =
    toHeaders . mostRecentCheckpoint
  where
    toHeaders :: Maybe Point -> [Http.Header]
    toHeaders pt =
        case liftA2 (,) (getPointSlotNo <$> pt) (getPointHeaderHash =<< pt) of
            Nothing ->
                ("X-Most-Recent-Checkpoint", "0") : defaultHeaders
            Just (slot, headerHash) ->
                ("X-Most-Recent-Checkpoint", encodeUtf8 $ slotNoToText slot) :
                ("ETag", encodeUtf8 $ headerHashToText headerHash) :
                defaultHeaders

requestBodyJson
    :: (Json.Value -> Json.Parser a)
    -> Request
    -> IO (Maybe a)
requestBodyJson parser req = do
    bytes <- strictRequestBody req
    case Json.parse parser <$> Json.decodeStrict' (toStrict bytes) of
        Just (Json.Success a) -> return (Just a)
        _failureOrMalformed -> return Nothing

--
-- Tracer
--

tracerMiddleware :: Tracer IO TraceHttpServer -> Middleware
tracerMiddleware tr runApp req send = do
    start <- GHC.Clock.getMonotonicTimeNSec
    logWith tr $ HttpRequest {path, method}
    runApp req $ \res -> do
        result <- send res
        end <- GHC.Clock.getMonotonicTimeNSec
        let time = mkRequestTime start end
        let status = mkStatus (responseStatus res)
        logWith tr $ HttpResponse {status, time}
        pure result
  where
    method = decodeUtf8 (requestMethod req)
    path = pathInfo req

data TraceHttpServer where
    HttpUnexpectedError
        :: { hint :: Text }
        -> TraceHttpServer
    HttpServerListening
        :: { host :: String, port :: Int }
        -> TraceHttpServer
    HttpFailedToOpenDatabaseConnection
        :: { attempts :: Word, retryingIn :: DiffTime }
        -> TraceHttpServer
    HttpRequest
        :: { path :: [Text], method :: Text }
        -> TraceHttpServer
    HttpResponse
        :: { status :: Status, time :: RequestTime }
        -> TraceHttpServer
    deriving stock (Generic)

instance HasSeverityAnnotation TraceHttpServer where
    getSeverityAnnotation = \case
        HttpUnexpectedError{} -> Error
        HttpFailedToOpenDatabaseConnection{attempts} | attempts < maxReconnectionAttempts -> Debug
        HttpFailedToOpenDatabaseConnection{} -> Warning
        HttpServerListening{} -> Notice
        HttpRequest{} -> Info
        HttpResponse{} -> Info

instance ToJSON TraceHttpServer where
    toEncoding =
        defaultGenericToEncoding

newtype RequestTime = RequestTime { milliseconds :: Word64 }
    deriving stock (Generic, Show)

mkRequestTime :: Word64 -> Word64 -> RequestTime
mkRequestTime start end =
    RequestTime $ (end - start) `div` 1_000_000

instance ToJSON RequestTime where
    toEncoding =
        defaultGenericToEncoding
