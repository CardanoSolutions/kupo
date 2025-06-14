-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}

module Test.Kupo.App.HttpSpec
    ( spec
    ) where

import Kupo.Prelude hiding
    ( get
    , put
    )

import Data.OpenApi
    ( OpenApi
    , Operation
    , Param
    , ParamLocation (..)
    , PathItem
    , Reference (..)
    , Referenced (..)
    , Schema
    , ValidationError
    , components
    , content
    , delete
    , get
    , in_
    , name
    , parameters
    , patch
    , paths
    , post
    , put
    , responses
    , schemas
    , validateJSON
    )
import Kupo.App.Database.Types
    ( Database (..)
    )
import Kupo.App.Http
    ( app
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Data.Cardano
    ( SlotNo (..)
    , getPointSlotNo
    , pattern BlockPoint
    , unsafeHeaderHashFromBytes
    )
import Kupo.Data.ChainSync
    ( ForcedRollbackHandler (..)
    )
import Kupo.Data.Configuration
    ( DeferIndexesInstallation (..)
    )
import Kupo.Data.Database
    ( policyToRow
    )
import Kupo.Data.Health
    ( ConnectionStatus (..)
    , Health (..)
    )
import Kupo.Data.PartialBlock
    ( PartialBlock (..)
    , PartialTransaction (..)
    )
import Kupo.Data.Pattern
    ( Pattern
    , patternToText
    )
import Network.HTTP.Media.MediaType
    ( MediaType
    , (//)
    , (/:)
    )
import Network.HTTP.Media.RenderHeader
    ( renderHeader
    )
import Network.HTTP.Types
    ( hAccept
    )
import Network.Wai
    ( Application
    )
import Test.Hspec
    ( Spec
    , parallel
    , runIO
    , shouldContain
    , shouldSatisfy
    , specify
    )
import Test.Hspec.QuickCheck
    ( prop
    )
import Test.Kupo.Data.Generators
    ( genBinaryData
    , genHeaderHash
    , genMetadata
    , genNonGenesisPoint
    , genOutputReference
    , genPattern
    , genPolicyId
    , genResult
    , genScript
    , genTransactionId
    , generateWith
    )
import Test.Kupo.Data.Pattern.Fixture
    ( patterns
    )
import Test.QuickCheck
    ( Gen
    , arbitrary
    , counterexample
    , elements
    , frequency
    , generate
    , listOf1
    , oneof
    , suchThat
    , vectorOf
    )
import Test.QuickCheck.Monadic
    ( assert
    , monadicIO
    , monitor
    , run
    )

import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Network.HTTP.Types.Header as Http
import qualified Network.HTTP.Types.Status as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai
import qualified Prelude
import qualified Test.Kupo.Data.Pattern.Fixture as Fixture

spec :: Spec
spec = do
    specification <- runIO $ do
        Yaml.decodeFileThrow @IO @OpenApi "./docs/api/nightly.yaml"

    parallel $ do
        specify "Preflight" $ do
            stub <- newStubbedApplication []
            Wai.withSession stub $ do
                res <- Wai.request $ Wai.defaultRequest { Wai.requestMethod = "OPTIONS" }
                    & flip Wai.setPath "/health"
                res & Wai.assertStatus (Http.statusCode Http.status200)
                res & Wai.assertHeader "Access-Control-Allow-Origin" "*"

        session specification get "/metrics" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/metrics"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson True schema

        session specification get "/health" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/health"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson True schema

        session specification get "/health" $ \_assertJson _endpoint -> do
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestHeaders = [ (hAccept, "text/plain") ] }
                & flip Wai.setPath "/health"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & Wai.assertHeader Http.hContentType (renderHeader mediaTypeTextPlain)
            liftIO $ (fst <$> Wai.simpleHeaders res) `shouldContain` ["X-Most-Recent-Checkpoint"]
            let bytes = toStrict (Wai.simpleBody res)
            liftIO $ bytes `shouldSatisfy` (BS.isInfixOf "kupo_connection_status")
            liftIO $ bytes `shouldSatisfy` (BS.isInfixOf "TYPE kupo_most_recent_checkpoint counter")
            return (Json.Null, [])

        session specification get "/health" $ \_assertJson _endpoint -> do
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestHeaders = [ (hAccept, "*/*") ] }
                & flip Wai.setPath "/health"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)
            liftIO $ (fst <$> Wai.simpleHeaders res) `shouldContain` ["X-Most-Recent-Checkpoint"]
            return (Json.Null, [])

        session specification get "/checkpoints" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/checkpoints"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson True schema

        session specification get "/matches" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/matches"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson True schema

        session specification get "/matches?resolve_hashes" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/matches?resolve_hashes"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson True schema

        session specification get "/matches?spent" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/matches?spent"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson True schema

        session specification get "/matches?unspent" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/matches?unspent"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson True schema

        session specification get "/matches?order=oldest_first" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/matches?oldest_first"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson True schema

        session specification get "/matches/{pattern}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            fragment <- liftIO $ generate (elements Fixture.unaryFragments)
            res <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath ("/matches/" <> fragment)
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson True schema

        session specification get "/matches/{pattern}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            fragment <- liftIO $ generate (elements Fixture.binaryFragments)
            res <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath ("/matches/" <> fragment)
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson True schema

        session specification get "/matches/{pattern}?order=most_recent_first" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            fragment <- liftIO $ generate (elements Fixture.binaryFragments)
            res <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath ("/matches/" <> fragment <> "?order=most_recent_first")
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson True schema

        sessionWith noWildcard specification delete "/matches/{pattern}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            let allPatterns = (\(p, _, _) -> p) <$> patterns
            fragment <- liftIO $ generate $
                (patternToText <$> genPattern) `suchThat` (`notElem` allPatterns)
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "DELETE" }
                & flip Wai.setPath ("/matches/" <> encodeUtf8 fragment)
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson True schema

        session specification get "/datums/{datum-hash}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "GET" }
                & flip Wai.setPath "/datums/309706b92ad8340cd6a5d31bf9d2e682fdab9fc8865ee3de14e09dedf9b1b635"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson True schema

        session specification get "/scripts/{script-hash}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "GET" }
                & flip Wai.setPath "/scripts/309706b92ad8340cd6a5d31bf9d2e682fdab9fc8865ee3de14e09ded"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson True schema

        session specification get "/metadata/{slot-no}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "GET" }
                & flip Wai.setPath "/metadata/42"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson True schema

        session specification get "/metadata/{slot-no}?transaction_id=309706b92ad8340cd6a5d31bf9d2e682fdab9fc8865ee3de14e09dedf9b1b635" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "GET" }
                & flip Wai.setPath "/metadata/42?transaction_id=309706b92ad8340cd6a5d31bf9d2e682fdab9fc8865ee3de14e09dedf9b1b635"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson True schema

        session specification get "/patterns" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/patterns"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson False schema

        session specification delete "/patterns/{pattern}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            fragment <- liftIO $ generate $ patternToText <$> genPattern
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "DELETE" }
                & flip Wai.setPath ("/patterns/" <> encodeUtf8 fragment)
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson False schema

        session specification put "/patterns" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            reqBody <- liftIO $ generate genPutPatternsRequestBody
            res <- Wai.srequest $ Wai.SRequest
                { Wai.simpleRequest = Wai.defaultRequest
                    { Wai.requestMethod = "PUT" }
                    & flip Wai.setPath "/patterns"
                , Wai.simpleRequestBody =
                    reqBody
                }
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson False schema

        session specification put "/patterns/{pattern}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            reqBody <- liftIO $ generate genPutPatternRequestBody
            res <- Wai.srequest $ Wai.SRequest
                { Wai.simpleRequest = Wai.defaultRequest
                    { Wai.requestMethod = "PUT" }
                    & flip Wai.setPath "/patterns/*"
                , Wai.simpleRequestBody =
                    reqBody
                }
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson False schema

        session specification put "/patterns/{pattern}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            reqBody <- liftIO $ generate genPutPatternRequestBody
            res <- Wai.srequest $ Wai.SRequest
                { Wai.simpleRequest = Wai.defaultRequest
                    { Wai.requestMethod = "PUT" }
                    & flip Wai.setPath "/patterns/*"
                , Wai.simpleRequestBody =
                    reqBody
                }
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson False schema

        session' "† GET /does-not-exist" $ do
            resNotFound <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath "/does-not-exist"
            resNotFound
                & Wai.assertStatus (Http.statusCode Http.status404)
            resNotFound
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "† POST /health" $ do
            resNotAllowed <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "POST" }
                & flip Wai.setPath "/health"
            resNotAllowed
                & Wai.assertStatus (Http.statusCode Http.status406)
            resNotAllowed
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "† GET /matches/*/*/*" $ do
            resBadRequest <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath "/matches/*/*/*"
            resBadRequest
                & Wai.assertStatus (Http.statusCode Http.status400)
            resBadRequest
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "† GET /matches?spent&unspent" $ do
            resBadRequest <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath "/matches?spent&unspent"
            resBadRequest
                & Wai.assertStatus (Http.statusCode Http.status400)
            resBadRequest
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "† GET /matches?order=foo" $ do
            resBadRequest <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath "/matches?order=foo"
            resBadRequest
                & Wai.assertStatus (Http.statusCode Http.status400)
            resBadRequest
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "† DELETE /matches/{pattern}" $ do
            overlappingFragment <- liftIO $ generate (elements Fixture.overlappingUnaryFragments)
            resBadRequest <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "DELETE" }
                & flip Wai.setPath ("/matches/" <> overlappingFragment)
            resBadRequest
                & Wai.assertStatus (Http.statusCode Http.status400)
            resBadRequest
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "† DELETE /matches/{pattern}" $ do
            overlappingFragment <- liftIO $ generate (elements Fixture.overlappingBinaryFragments)
            resBadRequest <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "DELETE" }
                & flip Wai.setPath ("/matches/" <> overlappingFragment)
            resBadRequest
                & Wai.assertStatus (Http.statusCode Http.status400)
            resBadRequest
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "† GET /checkpoints/42?foo" $ do
            resBadRequest <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath "/checkpoints/42?foo"
            resBadRequest
                & Wai.assertStatus (Http.statusCode Http.status400)
            resBadRequest
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "† GET /checkpoints/foo" $ do
            resBadRequest <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath "/checkpoints/foo"
            resBadRequest
                & Wai.assertStatus (Http.statusCode Http.status400)
            resBadRequest
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "† GET /datums/{datum-hash}" $ do
            resBadRequest <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath "/datums/foo"
            resBadRequest
                & Wai.assertStatus (Http.statusCode Http.status400)
            resBadRequest
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "† GET /scripts/{script-hash}" $ do
            resBadRequest <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath "/scripts/foo"
            resBadRequest
                & Wai.assertStatus (Http.statusCode Http.status400)
            resBadRequest
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "† GET /metadata/foo" $ do
            resBadRequest <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath "/metadata/foo"
            resBadRequest
                & Wai.assertStatus (Http.statusCode Http.status400)
            resBadRequest
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "† GET /metadata/999" $ do
            resBadRequest <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath "/metadata/999"
            resBadRequest
                & Wai.assertStatus (Http.statusCode Http.status400)
            resBadRequest
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "† GET /metadata/42?transaction_id=foo" $ do
            resBadRequest <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath "/metadata/42?transaction_id=foo"
            resBadRequest
                & Wai.assertStatus (Http.statusCode Http.status400)
            resBadRequest
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "† PUT /patterns/{pattern} (invalid / no request body)" $ do
            resBadRequest <-
                liftIO (generate genInvalidPutPatternRequestBody) >>= \case
                    Nothing ->
                        Wai.request $ Wai.defaultRequest
                            { Wai.requestMethod = "PUT" }
                            & flip Wai.setPath "/patterns/*"
                    Just reqBody ->
                        Wai.srequest $ Wai.SRequest
                            { Wai.simpleRequest = Wai.defaultRequest
                                { Wai.requestMethod = "PUT" }
                                & flip Wai.setPath "/patterns/*"
                            , Wai.simpleRequestBody = reqBody
                            }
            resBadRequest
                & Wai.assertStatus (Http.statusCode Http.status400)
            resBadRequest
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)
  where
    noWildcard :: [Pattern]
    noWildcard =
        [ p
        | (str, p, _) <- Fixture.patterns
        , str `notElem` ["*", "*/*"]
        ]

--
-- Stubs
--

newStubbedApplication :: [Pattern] -> IO Application
newStubbedApplication defaultPatterns = do
    patternsVar <- newTVarIO (fromList defaultPatterns)
    pure $ app
        (pure Nothing)
        (\_send _mode callback -> callback databaseStub)
        (\_point ForcedRollbackHandler{onSuccess} -> onSuccess)
        (\point reply ->
            if next (unSlotNo (getPointSlotNo point)) == 999 then
                reply Nothing
            else do
                reply $ Just $ generateWith 42 $ do
                    blockPoint <- genNonGenesisPoint
                    blockBody <- vectorOf 10 $ do
                        id <- genTransactionId
                        metadata <- oneof [pure Nothing, Just <$> genMetadata]
                        pure $ PartialTransaction
                            { id
                            , inputs = mempty
                            , outputs = mempty
                            , scripts = mempty
                            , datums = mempty
                            , spendRedeemers = mempty
                            , metadata
                            }
                    pure $ PartialBlock { blockPoint, blockBody }
        )
        patternsVar
        healthStub

healthStub :: IO Health
healthStub =
    pure $ Health
        { connectionStatus = Connected
        , mostRecentCheckpoint = Just $ BlockPoint 42 (generateWith 42 genHeaderHash)
        , mostRecentNodeTip = Just 42
        , mostRecentClockTick = Nothing
        , configuration = Just InstallIndexesIfNotExist
        }

databaseStub :: Database IO
databaseStub = Database
    { longestRollback =
        10
    , optimize =
        return ()
    , close =
        return ()
    , countInputs =
        \_ -> lift (generate arbitrary)
    , insertInputs =
        \_ -> return ()
    , foldInputs = \_ _ _ _ _ callback -> lift $ do
        rows <- generate (listOf1 genResult)
        mapM_ callback rows
    , deleteInputs =
        \_ -> liftIO (abs <$> generate arbitrary)
    , markInputs =
        \_ _ -> lift (generate arbitrary)
    , pruneInputs =
        liftIO (generate arbitrary)
    , countPolicies =
        \_ -> lift (generate arbitrary)
    , insertPolicies =
        \_ -> return ()
    , foldPolicies = \_ callback -> lift $ do
        rows <- generate (listOf1 $ policyToRow <$> genOutputReference <*> genPolicyId)
        mapM_ callback rows
    , insertCheckpoints =
        \_ -> return ()
    , listCheckpointsDesc = lift $ do
        generate (listOf1 genNonGenesisPoint)
    , listAncestorsDesc = \sl n -> lift $ do
        case n of
            1 -> do
                let headerHash = unsafeHeaderHashFromBytes $ unsafeDecodeBase16
                        "0000000000000000000000000000000000000000000000000000000000000000"
                let point = BlockPoint (prev sl) headerHash
                pure [point]
            _otherwise -> do
                generate (listOf1 genNonGenesisPoint)
    , insertPatterns =
        \_ -> return ()
    , deletePattern =
        \_ -> liftIO (abs <$> generate arbitrary)
    , listPatterns = lift $ do
        generate (fromList <$> listOf1 genPattern)
    , insertBinaryData =
        \_ -> return ()
    , getBinaryData =
        \_ -> liftIO $ generate $ do
            oneof [pure Nothing, Just <$> genBinaryData]
    , pruneBinaryData =
        liftIO (generate arbitrary)
    , insertScripts =
        \_ -> return ()
    , getScript =
        \_ -> liftIO $ generate $ do
            oneof [pure Nothing, Just <$> genScript]
    , rollbackTo =
        \_ -> return Nothing
    , runTransaction = \r ->
        runReaderT r (error "Connection")
    }

--
-- Helpers
--

mediaTypeJson :: MediaType
mediaTypeJson =
    "application" // "json" /: ("charset", "utf-8")

mediaTypeTextPlain :: MediaType
mediaTypeTextPlain =
    "text" // "plain" /: ("charset", "utf-8")

findSchema
    :: HasCallStack
    => OpenApi
    -> Operation
    -> Http.Status
    -> Schema
findSchema specification op status = runIdentity $ do
    res <- oops "no response for status" (op ^. responses . responses . at (Http.statusCode status)) >>= \case
        Inline a ->
            pure a
        Ref (Reference ref) ->
            oops ("no component for ref: " <> ref) $
                specification ^. components . responses . at ref
    ct <- oops "no content for media-type" (res ^. content . at mediaTypeJson)
    oops "content has no schema" (ct ^. OpenApi.schema) >>= \case
        Inline a ->
            pure a
        Ref (Reference ref) ->
            oops ("no schema for ref: " <> ref) $
                specification ^. components . schemas . at ref

session
    :: OpenApi
    -> Lens' PathItem (Maybe Operation)
    -> Text
    -> ( (Bool -> Schema -> Wai.SResponse -> Wai.Session (Json.Value, [ValidationError]))
       -> Operation
       -> Wai.Session (Json.Value, [ValidationError])
       )
    -> Spec
session =
    sessionWith [ p | (_, p, _) <- Fixture.patterns ]

sessionWith
    :: [Pattern]
    -> OpenApi
    -> Lens' PathItem (Maybe Operation)
    -> Text
    -> ( (Bool -> Schema -> Wai.SResponse -> Wai.Session (Json.Value, [ValidationError]))
       -> Operation
       -> Wai.Session (Json.Value, [ValidationError])
       )
    -> Spec
sessionWith defaultPatterns specification opL path callback =
    prop (method <> " " <> toString path) $ monadicIO $ do
        stub <- run (newStubbedApplication defaultPatterns)
        (json, errs) <- run $ Wai.runSession (callback assertJson endpoint) stub
        monitor $ counterexample (decodeUtf8 (Json.encode json))
        forM_ errs (monitor . counterexample . show)
        forM_ queryParams $ \param -> assert (param ^. in_ == ParamQuery)
        assert (null errs)
  where
    Identity (endpoint, queryParams, method) = do
        let rawPath = toString (Prelude.head (T.splitOn "?" path))
        item <- oops "No specification for path"
            (specification ^. paths . at rawPath)
        op <- oops "Operation not found" (item ^. opL)
        params <- findQueryParams op
        return
            ( op
            , params
            , if
               | item ^. get    == Just op -> "GET"
               | item ^. put    == Just op -> "PUT"
               | item ^. patch  == Just op -> "PATCH"
               | item ^. post   == Just op -> "POST"
               | item ^. delete == Just op -> "DELETE"
               | otherwise                 -> "UNKNOWN"
            )

    findQueryParams :: (HasCallStack, Monad f) => Operation -> f [Param]
    findQueryParams op =
        case T.splitOn "?" path of
            _:[params] -> do
                traverse (findQueryParam op) (T.splitOn "&" params)
            _notFound ->
                pure []

    findQueryParam :: (HasCallStack, Monad f) => Operation -> Text -> f Param
    findQueryParam op keyVal =
        case T.splitOn "=" keyVal of
            key:_ -> do
                oops
                    ("Definition for query parameter '" <> key <> "' not found")
                    (paramAt key (op ^. parameters))
            _malformedParam ->
                error "guardQueryParam: malformed param"

    paramAt :: Text -> [Referenced Param] -> Maybe Param
    paramAt paramName = \case
        [] ->
            Nothing
        (Inline param):_ | param ^. name == paramName ->
            Just param
        Ref (Reference ref):rest -> do
            param <- oops ("no component for ref: " <> ref) $
                specification ^. components . parameters . at ref
            paramAt paramName (Inline param:rest)
        _:rest ->
            paramAt paramName rest

    assertJson shouldHaveMostRecentCheckpoint schema res = do
        res & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)
        res & Wai.assertHeader "Access-Control-Allow-Origin" "*"
        when shouldHaveMostRecentCheckpoint $
            liftIO $ (fst <$> Wai.simpleHeaders res) `shouldContain` ["X-Most-Recent-Checkpoint"]
        case Json.eitherDecode' (Wai.simpleBody res) of
            Left e ->
                liftIO $ fail (show e)
            Right json ->
                pure (json, validateJSON definitions schema json)
      where
        definitions = specification ^. components . schemas

session' :: Text -> Wai.Session () -> Spec
session' path s = do
    specify (toString path) $ do
        stub <- newStubbedApplication [ p | (_, p, _) <- Fixture.patterns ]
        Wai.runSession s stub

oops :: (HasCallStack, Applicative f) => Text -> Maybe a -> f a
oops str = maybe (error str) pure

--
-- Generators
--

genPutPatternRequestBody :: Gen LByteString
genPutPatternRequestBody = elements
    [ "{ \"rollback_to\": { \"slot_no\": 42 }\
      \}"

    , "{ \"rollback_to\": { \"slot_no\": 42, \"header_hash\": \"0000000000000000000000000000000000000000000000000000000000000000\" }\
      \}"

    , "{ \"rollback_to\": { \"slot_no\": 42 }\
      \, \"limit\": \"within_safe_zone\"\
      \}"

    , "{ \"rollback_to\": { \"slot_no\": 42, \"header_hash\": \"0000000000000000000000000000000000000000000000000000000000000000\" }\
      \, \"limit\": \"within_safe_zone\"\
      \}"

    , "{ \"rollback_to\": { \"slot_no\": 14 }\
      \, \"limit\": \"unsafe_allow_beyond_safe_zone\"\
      \}"

    , "{ \"rollback_to\": { \"slot_no\": 14, \"header_hash\": \"0000000000000000000000000000000000000000000000000000000000000000\" }\
      \, \"limit\": \"unsafe_allow_beyond_safe_zone\"\
      \}"
    ]

genPutPatternsRequestBody :: Gen LByteString
genPutPatternsRequestBody = elements
    [ "{ \"rollback_to\": { \"slot_no\": 42 }\
      \, \"patterns\": []\
      \}"

    , "{ \"rollback_to\": { \"slot_no\": 42 }\
      \, \"patterns\": [\"*\"]\
      \}"

    , "{ \"rollback_to\": { \"slot_no\": 42 }\
      \, \"patterns\": [\"*\", \"*/*\"]\
      \}"
    ]

genInvalidPutPatternRequestBody :: Gen (Maybe LByteString)
genInvalidPutPatternRequestBody = frequency
    [ (1, pure Nothing)
    , (5, Just <$> elements
        [ "{}"
        , "{ \"rollback_to\": { \"slot_no\": 14 }\
          \}"
        , "{ \"rollback_to\": { \"slot_no\": 14 }\
          \, \"limit\": \"within_safe_zone\"\
          \}"
        , "{ \"foo\": { \"slot_no\": 42 }\
          \}"
        ]
      )
    ]
