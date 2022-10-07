--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

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

import Kupo.App.Database
    ( ConnectionType (..)
    , Database (..)
    )
import Kupo.App.Http.HealthCheck
    ( healthCheck
    )
import Kupo.Control.MonadCatch
    ( MonadCatch (..)
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
    )
import Kupo.Data.Database
    ( applyStatusFlag
    , binaryDataFromRow
    , datumHashToRow
    , mkSortDirection
    , patternToRow
    , patternToSql
    , pointFromRow
    , resultFromRow
    , scriptFromRow
    , scriptHashToRow
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
    , responseStatus
    , strictRequestBody
    )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
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
    -> (ConnectionType -> (Database IO -> IO ResponseReceived) -> IO ResponseReceived)
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
        & Warp.setBeforeMainLoop (logWith tr HttpServerListening{host,port})

    withDatabaseWrapped send connectionType action =
        withDatabase connectionType action `catch` onServerError
      where
        onServerError (hint :: SomeException) = do
            logWith tr $ HttpUnexpectedError (toText $ displayException hint)
            send Errors.serverError

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
    route (pathInfo req)
  where
    route = \case
        ("health" : args) ->
            routeHealth (requestMethod req, args)

        ("checkpoints" : args) ->
            routeCheckpoints (requestMethod req, args)

        ("matches" : args) ->
            routeMatches (requestMethod req, args)

        ("datums" : args) ->
            routeDatums (requestMethod req, args)

        ("scripts" : args) ->
            routeScripts (requestMethod req, args)

        ("metadata" : args) ->
            routeMetadata (requestMethod req, args)

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

    routeCheckpoints = \case
        ("GET", []) ->
            withDatabase send ReadOnly $ \db -> do
                headers <- responseHeaders readHealth Default.headers
                send (handleGetCheckpoints headers db)
        ("GET", [arg]) ->
            withDatabase send ReadOnly $ \db -> do
                headers <- responseHeaders readHealth Default.headers
                send =<< handleGetCheckpointBySlot
                            headers
                            (slotNoFromText arg)
                            (queryString req)
                            db
        ("GET", _) ->
            send Errors.notFound
        (_, _) ->
            send Errors.methodNotAllowed

    routeMatches = \case
        ("GET", args) ->
            withDatabase send ReadOnly $ \db -> do
                headers <- responseHeaders readHealth Default.headers
                send $ handleGetMatches
                            headers
                            (pathParametersToText args)
                            (queryString req)
                            db
        ("DELETE", args) ->
            withDatabase send ReadWrite $ \db -> do
                headers <- responseHeaders readHealth Default.headers
                send =<< handleDeleteMatches
                            headers
                            patternsVar
                            (pathParametersToText args)
                            db
        (_, _) ->
            send Errors.methodNotAllowed

    routeDatums = \case
        ("GET", [arg]) ->
            withDatabase send ReadOnly $ \db -> do
                headers <- responseHeaders readHealth Default.headers
                send =<< handleGetDatum
                            headers
                            (datumHashFromText arg)
                            db
        ("GET", _) ->
            send Errors.notFound
        (_, _) ->
            send Errors.methodNotAllowed

    routeScripts = \case
        ("GET", [arg]) ->
            withDatabase send ReadOnly $ \db -> do
                headers <- responseHeaders readHealth Default.headers
                send =<< handleGetScript
                            headers
                            (scriptHashFromText arg)
                            db
        ("GET", _) ->
            send Errors.notFound
        (_, _) ->
            send Errors.methodNotAllowed

    routeMetadata = \case
        ("GET", [arg]) ->
            withDatabase send ReadOnly $ \db -> do
                headers <- responseHeaders readHealth Default.headers
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
            res <- handleGetPatterns
                        <$> responseHeaders readHealth Default.headers
                        <*> pure (Just wildcard)
                        <*> fmap const (readTVarIO patternsVar)
            send res
        ("GET", args) -> do
            res <- handleGetPatterns
                        <$> responseHeaders readHealth Default.headers
                        <*> pure (pathParametersToText args)
                        <*> fmap (flip included) (readTVarIO patternsVar)
            send res
        ("PUT", args) -> do
            pointOrSlotNo <- requestBodyJson decodeForcedRollback req
            withDatabase send ReadWrite $ \db -> do
                headers <- responseHeaders readHealth Default.headers
                send =<< handlePutPattern
                            headers
                            readHealth
                            forceRollback
                            patternsVar
                            pointOrSlotNo
                            (pathParametersToText args)
                            db
        ("DELETE", args) ->
            withDatabase send ReadWrite $ \db -> do
                headers <- responseHeaders readHealth Default.headers
                send =<< handleDeletePattern
                            headers
                            patternsVar
                            (pathParametersToText args)
                            db
        (_, _) ->
            send Errors.methodNotAllowed

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
            resHeaders <- responseHeaders (pure health) [(hContentType, "text/plain;charset=utf-8")]
            return $ responseBuilder status200 resHeaders (mkPrometheusMetrics health)
        Just ct | cApplicationJson `BS.isInfixOf` ct -> do
            resHeaders <- responseHeaders (pure health) Default.headers
            return $ responseJson status200 resHeaders health
        Just ct | cAny `BS.isInfixOf` ct -> do
            resHeaders <- responseHeaders (pure health) [(hContentType, "text/plain;charset=utf-8")]
            return $ responseBuilder status200 resHeaders (mkPrometheusMetrics health)
        Nothing -> do
            resHeaders <- responseHeaders (pure health) Default.headers
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
        points <- runReadOnlyTransaction (listCheckpointsDesc pointFromRow)
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
        let successor = next (unSlotNo slotNo)
        points <- runReadOnlyTransaction (listAncestorsDesc successor 1 pointFromRow)
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
    p <- (patternQuery >>= patternFromText)
        `orAbort` Errors.invalidPattern

    statusFlag <- statusFlagFromQueryParams queryParams
        `orAbort` Errors.invalidStatusFlag

    yieldIf <- (mkYieldIf p <$> filterMatchesBy queryParams)
        `orAbort` Errors.invalidMatchFilter

    sortDirection <- mkSortDirection <$> orderMatchesBy queryParams
        `orAbort` Errors.invalidSortDirection

    let query = applyStatusFlag statusFlag (patternToSql p)

    pure $ responseStreamJson headers resultToJson $ \yield done -> do
        runReadOnlyTransaction $ foldInputs query sortDirection (yieldIf yield . resultFromRow)
        done
  where
    -- NOTE: kupo does support two different ways for fetching results, via query parameters or via
    -- path parameters. Historically, there were only query parameters. Yet, with the introduction
    -- of patterns, both approaches are now redundant with one another.
    --
    -- When it comes to asset id / policy id, querying via pattern or via query parameters will have
    -- the same effect & performance. However, when querying output reference / transaction id from
    -- a pattern, the query is optimized through a first pre-filtering via SQL.
    --
    -- Removing query parameters now would be a breaking change, although it would remove quite a
    -- bunch of code. This note serves as a reminder. For the next major version, filters should
    -- likely be removed.
    mkYieldIf pattern_ filter_ =
        let
            predicateA =
                case pattern_ of
                    MatchAssetId assetId ->
                        \result -> hasAssetId (value result) assetId
                    MatchPolicyId policyId ->
                        \result -> hasPolicyId (value result) policyId
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
            n <- runReadWriteTransaction $ deleteInputsByAddress (patternToSql p)
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
            datum <- runReadOnlyTransaction $
                getBinaryData (datumHashToRow datumHash) binaryDataFromRow
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
            script <- runReadOnlyTransaction $
                getScript (scriptHashToRow scriptHash) scriptFromRow
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
        Just (SlotNo slotNo) | slotNo == 0 -> do
            pure $ responseStreamJson baseHeaders metadataToJson' $ \_yield done -> done
        Just (SlotNo slotNo) -> do
            ancestor <- runReadOnlyTransaction (listAncestorsDesc slotNo 1 pointFromRow) <&> \case
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
            n <- runReadWriteTransaction $ deletePattern (patternToRow p)
            atomically $ modifyTVar' patternsVar (Set.delete p)
            pure $ responseJsonEncoding status200 headers $
                Json.pairs $ mconcat
                    [ Json.pair "deleted" (Json.int n)
                    ]

handlePutPattern
    :: [Http.Header]
    -> IO Health
    -> (Point -> ForcedRollbackHandler IO -> IO ())
    -> TVar IO (Set Pattern)
    -> Maybe ForcedRollback
    -> Maybe Text
    -> Database IO
    -> IO Response
handlePutPattern headers readHealth forceRollback patternsVar mPointOrSlot query Database{..} = do
    mPoint <- traverse
        (\ForcedRollback{since, limit} -> (, limit) <$> resolvePointOrSlot since)
        mPointOrSlot

    case (query >>= patternFromText, mPoint) of
        (Nothing, _) ->
            pure Errors.invalidPattern
        (_, Nothing) ->
            pure Errors.malformedPoint
        (_, Just (Nothing, _)) ->
            pure Errors.nonExistingPoint
        (Just p, Just (Just point, lim)) -> do
            tip <- mostRecentNodeTip <$> readHealth
            let d = distanceToSlot <$> tip <*> pure (getPointSlotNo point)
            case ((LongestRollback <$> d) > Just longestRollback, lim) of
                (True, OnlyAllowRollbackWithinSafeZone) ->
                    pure Errors.unsafeRollbackBeyondSafeZone
                _safeRollbackOrAllowedUnsafe ->
                    putPatternAt p point
  where
    resolvePointOrSlot :: Either SlotNo Point -> IO (Maybe Point)
    resolvePointOrSlot = \case
        Right pt -> do
            let successor = unSlotNo $ next $ getPointSlotNo pt
            pts <- runReadOnlyTransaction $ listAncestorsDesc successor 1 pointFromRow
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
            let successor = unSlotNo (next sl)
            pts <- runReadOnlyTransaction $ listAncestorsDesc successor 1 pointFromRow
            return $ case pts of
                [pt] | sl == getPointSlotNo pt ->
                    Just pt
                _unexpectedPoint ->
                    Nothing

    putPatternAt :: Pattern -> Point -> IO Response
    putPatternAt p point = do
        response <- newEmptyTMVarIO
        forceRollback point $ ForcedRollbackHandler
            { onSuccess = do
                runReadWriteTransaction $ insertPatterns [patternToRow p]
                patterns <- atomically $ do
                    modifyTVar' patternsVar (Set.insert p)
                    readTVar patternsVar
                atomically $ putTMVar response $ responseJsonEncoding status200 headers $
                    Json.list
                        (Json.text . patternToText)
                        (toList patterns)
            , onFailure = do
                atomically (putTMVar response Errors.failedToRollback)
            }
        atomically (takeTMVar response)

--
-- Helpers
--

orAbort :: Maybe a -> Response -> Either Response a
orAbort l r = maybeToRight r l
{-# INLINABLE orAbort #-}

handleRequest :: Either Response Response -> Response
handleRequest = either identity identity
{-# INLINABLE handleRequest #-}

responseHeaders
    :: Applicative m
    => m Health
    -> [Http.Header]
    -> m [Http.Header]
responseHeaders readHealth defaultHeaders =
    toHeaders . mostRecentCheckpoint <$> readHealth
  where
    toHeaders :: Maybe SlotNo -> [Http.Header]
    toHeaders slot =
        ("X-Most-Recent-Checkpoint", encodeUtf8 $ slotNoToText $ fromMaybe 0 slot)
        : defaultHeaders

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
    HttpServerListening
        :: { host :: String, port :: Int }
        -> TraceHttpServer
    HttpUnexpectedError
        :: { hint :: Text }
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
        HttpServerListening{} -> Notice
        HttpUnexpectedError{} -> Error
        HttpRequest{}         -> Info
        HttpResponse{}        -> Info

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
