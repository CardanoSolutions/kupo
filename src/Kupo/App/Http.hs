--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
    ( Database (..)
    )
import Kupo.App.Http.HealthCheck
    ( healthCheck
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
    , Point
    , ScriptHash
    , SlotNo (..)
    , binaryDataToJson
    , datumHashFromText
    , distanceToSlot
    , getPointSlotNo
    , getTransactionId
    , hasAssetId
    , hasPolicyId
    , pointToJson
    , scriptHashFromText
    , scriptToJson
    , slotNoFromText
    , slotNoToText
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
    , patternToRow
    , patternToSql
    , pointFromRow
    , resultFromRow
    , scriptFromRow
    , scriptHashToRow
    )
import Kupo.Data.Health
    ( Health (..)
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
import Network.HTTP.Types.Status
    ( status200
    )
import Network.Wai
    ( Application
    , Middleware
    , Request
    , Response
    , pathInfo
    , queryString
    , requestMethod
    , responseStatus
    , strictRequestBody
    )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Set as Set
import qualified Kupo.Data.Http.Default as Default
import qualified Kupo.Data.Http.Error as Errors
import qualified Network.HTTP.Types.Header as Http
import qualified Network.HTTP.Types.URI as Http
import qualified Network.Wai.Handler.Warp as Warp

--
-- Server
--

httpServer
    :: Tracer IO TraceHttpServer
    -> (forall a. (Database IO -> IO a) -> IO a)
    -> (Point -> ForcedRollbackHandler IO -> IO ())
    -> TVar IO (Set Pattern)
    -> IO Health
    -> String
    -> Int
    -> IO ()
httpServer tr withDatabase forceRollback patternsVar readHealth host port =
    Warp.runSettings settings
        $ tracerMiddleware tr
        $ app withDatabase forceRollback patternsVar readHealth
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
    -> (Point -> ForcedRollbackHandler IO -> IO ())
    -> TVar IO (Set Pattern)
    -> IO Health
    -> Application
app withDatabase forceRollback patternsVar readHealth req send =
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

        ("patterns" : args) ->
            routePatterns (requestMethod req, args)

        ("v1" : args) ->
            route args

        _unmatchedRoutes ->
            send Errors.notFound

    routeHealth = \case
        ("GET", []) -> do
            health <- readHealth
            headers <- responseHeaders readHealth
            send (handleGetHealth headers health)
        ("GET", _) ->
            send Errors.notFound
        (_, _) ->
            send Errors.methodNotAllowed

    routeCheckpoints = \case
        ("GET", []) ->
            withDatabase $ \db -> do
                headers <- responseHeaders readHealth
                send (handleGetCheckpoints headers db)
        ("GET", [arg]) ->
            withDatabase $ \db -> do
                headers <- responseHeaders readHealth
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
            withDatabase $ \db -> do
                headers <- responseHeaders readHealth
                send $ handleGetMatches
                            headers
                            (pathParametersToText args)
                            (queryString req)
                            db
        ("DELETE", args) ->
            withDatabase $ \db -> do
                headers <- responseHeaders readHealth
                send =<< handleDeleteMatches
                            headers
                            patternsVar
                            (pathParametersToText args)
                            db
        (_, _) ->
            send Errors.methodNotAllowed

    routeDatums = \case
        ("GET", [arg]) ->
            withDatabase $ \db -> do
                headers <- responseHeaders readHealth
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
            withDatabase $ \db -> do
                headers <- responseHeaders readHealth
                send =<< handleGetScript
                            headers
                            (scriptHashFromText arg)
                            db
        ("GET", _) ->
            send Errors.notFound
        (_, _) ->
            send Errors.methodNotAllowed

    routePatterns = \case
        ("GET", []) -> do
            res <- handleGetPatterns
                        <$> responseHeaders readHealth
                        <*> pure (Just wildcard)
                        <*> fmap const (readTVarIO patternsVar)
            send res
        ("GET", args) -> do
            res <- handleGetPatterns
                        <$> responseHeaders readHealth
                        <*> pure (pathParametersToText args)
                        <*> fmap (flip included) (readTVarIO patternsVar)
            send res
        ("PUT", args) -> do
            pointOrSlotNo <- requestBodyJson decodeForcedRollback req
            withDatabase $ \db -> do
                headers <- responseHeaders readHealth
                send =<< handlePutPattern
                            headers
                            readHealth
                            forceRollback
                            patternsVar
                            pointOrSlotNo
                            (pathParametersToText args)
                            db
        ("DELETE", args) ->
            withDatabase $ \db -> do
                headers <- responseHeaders readHealth
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
    -> Response
handleGetHealth =
    responseJson status200

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
handleGetMatches headers patternQuery queryParams Database{..} = do
    case (patternQuery >>= patternFromText, statusFlagFromQueryParams queryParams) of
        (Nothing, _) ->
            Errors.invalidPattern
        (Just{}, Nothing) ->
            Errors.invalidStatusFlag
        (Just p, Just statusFlag) -> do
            let query = applyStatusFlag statusFlag (patternToSql p)
            case filterMatchesBy queryParams of
                Nothing ->
                    Errors.invalidMatchFilter
                Just (mkYieldIf p -> yieldIf) -> do
                    responseStreamJson headers resultToJson $ \yield done -> do
                        runReadOnlyTransaction $ foldInputs query (yieldIf yield . resultFromRow)
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
                        \result -> outputReference result == outRef
                    FilterByTransactionId transactionId ->
                        \result -> (getTransactionId . outputReference) result == transactionId
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

responseHeaders
    :: Applicative m
    => m Health
    -> m [Http.Header]
responseHeaders readHealth =
    toHeaders . mostRecentCheckpoint <$> readHealth
  where
    toHeaders :: Maybe SlotNo -> [Http.Header]
    toHeaders slot =
        ("X-Most-Recent-Checkpoint", encodeUtf8 $ slotNoToText $ fromMaybe 0 slot)
        : Default.headers

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
