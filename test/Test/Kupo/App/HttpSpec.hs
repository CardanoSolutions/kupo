-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.App.HttpSpec
    ( spec
    ) where

import Kupo.Prelude hiding
    ( get, put )

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
import Kupo.App.Database
    ( Database (..) )
import Kupo.App.Http
    ( app )
import Kupo.Control.MonadSTM
    ( MonadSTM (..) )
import Kupo.Data.Database
    ( binaryDataToRow, patternToRow, pointToRow, resultToRow, scriptToRow )
import Kupo.Data.Health
    ( Health )
import Kupo.Data.Pattern
    ( Pattern )
import Network.HTTP.Media.MediaType
    ( MediaType, (//), (/:) )
import Network.HTTP.Media.RenderHeader
    ( renderHeader )
import Network.Wai
    ( Application )
import Test.Hspec
    ( Spec, parallel, runIO, shouldContain, specify )
import Test.Hspec.QuickCheck
    ( prop )
import Test.Kupo.Data.Generators
    ( genBinaryData
    , genDatumHash
    , genHealth
    , genNonGenesisPoint
    , genPattern
    , genResult
    , genScript
    , genScriptHash
    )
import Test.QuickCheck
    ( arbitrary, counterexample, elements, generate, listOf1, oneof )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

import qualified Data.Aeson as Json
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
        Yaml.decodeFileThrow @IO @OpenApi "./docs/openapi.yaml"

    parallel $ do
        session specification get "/v1/health" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/v1/health"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification get "/v1/checkpoints" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/v1/checkpoints"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification get "/v1/matches" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/v1/matches"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification get "/v1/matches?spent" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/v1/matches?spent"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification get "/v1/matches?unspent" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/v1/matches?unspent"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification get "/v1/matches?policy_id=96cb...dc48" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/v1/matches?policy_id=96cb65293573e5c9f947d40bd06f80c465d4c6acee7598398765dc48"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification get "/v1/matches?policy_id=96cb...dc48&asset_name=40bd" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/v1/matches?policy_id=96cb65293573e5c9f947d40bd06f80c465d4c6acee7598398765dc48&asset_name=40bd"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification get "/v1/matches/{pattern-fragment}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            fragment <- liftIO $ generate (elements Fixture.unaryFragments)
            res <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath ("/v1/matches/" <> fragment)
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification get "/v1/matches/{pattern-fragment}/{pattern-fragment}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            fragment <- liftIO $ generate (elements Fixture.binaryFragments)
            res <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath ("/v1/matches/" <> fragment)
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        sessionWith noWildcard specification delete "/v1/matches/{pattern-fragment}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            fragment <- liftIO $ generate (elements Fixture.nonOverlappingUnaryFragments)
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "DELETE" }
                & flip Wai.setPath ("/v1/matches/" <> fragment)
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        sessionWith noWildcard specification delete "/v1/matches/{pattern-fragment}/{pattern-fragment}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            fragment <- liftIO $ generate (elements Fixture.nonOverlappingBinaryFragments)
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "DELETE" }
                & flip Wai.setPath ("/v1/matches/" <> fragment)
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification get "/v1/datums/{datum-hash}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "GET" }
                & flip Wai.setPath "/v1/datums/309706b92ad8340cd6a5d31bf9d2e682fdab9fc8865ee3de14e09dedf9b1b635"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification get "/v1/scripts/{script-hash}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "GET" }
                & flip Wai.setPath "/v1/scripts/309706b92ad8340cd6a5d31bf9d2e682fdab9fc8865ee3de14e09ded"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification get "/v1/patterns" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/v1/patterns"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification delete "/v1/patterns/{pattern-fragment}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            fragment <- liftIO $ generate (elements Fixture.unaryFragments)
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "DELETE" }
                & flip Wai.setPath ("/v1/patterns/" <> fragment)
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification delete "/v1/patterns/{pattern-fragment}/{pattern-fragment}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            fragment <- liftIO $ generate (elements Fixture.binaryFragments)
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "DELETE" }
                & flip Wai.setPath ("/v1/patterns/" <> fragment)
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification put "/v1/patterns/{pattern-fragment}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "PUT" }
                & flip Wai.setPath "/v1/patterns/*"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification put "/v1/patterns/{pattern-fragment}/{pattern-fragment}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "PUT" }
                & flip Wai.setPath "/v1/patterns/*/*"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session' "🕱 GET /v1/does-not-exist" $ do
            resNotFound <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath "/v1/does-not-exist"
            resNotFound
                & Wai.assertStatus (Http.statusCode Http.status404)
            resNotFound
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "🕱 POST /v1/health" $ do
            resNotAllowed <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "POST" }
                & flip Wai.setPath "/v1/health"
            resNotAllowed
                & Wai.assertStatus (Http.statusCode Http.status406)
            resNotAllowed
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "🕱 GET /v1/matches/*/*/*" $ do
            resBadRequest <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath "/v1/matches/*/*/*"
            resBadRequest
                & Wai.assertStatus (Http.statusCode Http.status400)
            resBadRequest
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "🕱 DELETE /v1/matches/{pattern-fragment}" $ do
            overlappingFragment <- liftIO $ generate (elements Fixture.overlappingUnaryFragments)
            resBadRequest <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "DELETE" }
                & flip Wai.setPath ("/v1/matches/" <> overlappingFragment)
            resBadRequest
                & Wai.assertStatus (Http.statusCode Http.status400)
            resBadRequest
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "🕱 DELETE /v1/matches/{pattern-fragment}/{pattern-fragment}" $ do
            overlappingFragment <- liftIO $ generate (elements Fixture.overlappingBinaryFragments)
            resBadRequest <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "DELETE" }
                & flip Wai.setPath ("/v1/matches/" <> overlappingFragment)
            resBadRequest
                & Wai.assertStatus (Http.statusCode Http.status400)
            resBadRequest
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "🕱 GET /v1/datums/{datum-hash}" $ do
            resBadRequest <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath "/v1/datums/foo"
            resBadRequest
                & Wai.assertStatus (Http.statusCode Http.status400)
            resBadRequest
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "🕱 GET /v1/scripts/{script-hash}" $ do
            resBadRequest <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath "/v1/scripts/foo"
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
newStubbedApplication patterns = do
    patternsVar <- newTVarIO patterns
    pure $ app
        (\callback -> callback databaseStub)
        patternsVar
        healthStub

healthStub :: IO Health
healthStub =
    generate genHealth

databaseStub :: Database IO
databaseStub = Database
    { insertInputs =
        \_ -> return ()
    , foldInputs = \_ callback -> lift $ do
        rows <- fmap resultToRow <$> generate (listOf1 genResult)
        mapM_ callback rows
    , deleteInputsByAddress =
        \_ -> liftIO (abs <$> generate arbitrary)
    , deleteInputsByReference =
        \_ -> return ()
    , markInputsByReference =
        \_ _ -> return ()
    , pruneInputs =
        liftIO (generate arbitrary)
    , insertCheckpoints =
        \_ -> return ()
    , listCheckpointsDesc = \mk -> lift $ do
        fmap (mk . pointToRow) <$> generate (listOf1 genNonGenesisPoint)
    , listAncestorsDesc = \_ _ mk -> lift $ do
        fmap (mk . pointToRow) <$> generate (listOf1 genNonGenesisPoint)
    , insertPatterns =
        \_ -> return ()
    , deletePattern =
        \_ -> liftIO (abs <$> generate arbitrary)
    , listPatterns = \mk -> lift $ do
        fmap (mk . patternToRow) <$> generate (listOf1 genPattern)
    , insertBinaryData =
        \_ -> return ()
    , getBinaryData =
        \_ mk -> liftIO $ generate $ do
            binaryDataHash <- genDatumHash
            binaryData <- oneof [pure Nothing, Just <$> genBinaryData]
            pure $ mk . binaryDataToRow binaryDataHash <$> binaryData
    , pruneBinaryData =
        liftIO (generate arbitrary)
    , insertScripts =
        \_ -> return ()
    , getScript =
        \_ mk -> liftIO $ generate $ do
            scriptHash <- genScriptHash
            script <- oneof [pure Nothing, Just <$> genScript]
            pure $ mk . scriptToRow scriptHash <$> script
    , rollbackTo =
        \_ -> return Nothing
    , runReadOnlyTransaction = \r ->
        runReaderT r (error "Connection")
    , runReadWriteTransaction = \r ->
        runReaderT r (error "Connection")
    }

--
-- Helpers
--

mediaTypeJson :: MediaType
mediaTypeJson =
    "application" // "json" /: ("charset", "utf-8")

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
    -> ( (Schema -> Wai.SResponse -> Wai.Session (Json.Value, [ValidationError]))
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
    -> ( (Schema -> Wai.SResponse -> Wai.Session (Json.Value, [ValidationError]))
       -> Operation
       -> Wai.Session (Json.Value, [ValidationError])
       )
    -> Spec
sessionWith patterns specification opL path callback =
    prop (method <> " " <> toString path) $ monadicIO $ do
        stub <- run (newStubbedApplication patterns)
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
            _ ->
                pure []

    findQueryParam :: (HasCallStack, Monad f) => Operation -> Text -> f Param
    findQueryParam op keyVal =
        case T.splitOn "=" keyVal of
            key:_ -> do
                oops
                    ("Definition for query parameter '" <> key <> "' not found")
                    (paramAt key (op ^. parameters))
            _ ->
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

    assertJson schema res = do
        res & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)
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
