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
    , PathItem
    , Reference (..)
    , Referenced (..)
    , Schema
    , ValidationError
    , components
    , content
    , delete
    , get
    , patch
    , paths
    , post
    , put
    , responses
    , schemas
    , validateJSON
    )
import Kupo.App.Http
    ( app )
import Kupo.Control.MonadDatabase
    ( Database (..) )
import Kupo.Control.MonadSTM
    ( MonadSTM (..) )
import Kupo.Data.Database
    ( patternToRow, pointToRow, resultToRow )
import Kupo.Data.Health
    ( Health )
import Network.HTTP.Media.MediaType
    ( MediaType, (//), (/:) )
import Network.HTTP.Media.RenderHeader
    ( renderHeader )
import Network.Wai
    ( Application )
import Test.Hspec
    ( Spec, parallel, runIO, specify )
import Test.Hspec.QuickCheck
    ( prop )
import Test.Kupo.Data.Generators
    ( genHealth, genNonGenesisPoint, genPattern, genResult )
import Test.QuickCheck
    ( counterexample, generate, listOf1, vectorOf )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

import qualified Data.Aeson as Json
import qualified Data.OpenApi as OpenApi
import qualified Data.Yaml as Yaml
import qualified Network.HTTP.Types.Header as Http
import qualified Network.HTTP.Types.Status as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai

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

        session specification get "/v1/matches/{pattern-fragment}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/v1/matches/*"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification get "/v1/matches/{pattern-fragment}/{pattern-fragment}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/v1/matches/*/*"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification get "/v1/patterns" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/v1/patterns"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification delete "/v1/patterns/{pattern-fragment}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "DELETE" }
                & flip Wai.setPath "/v1/patterns/addr_test1vql8x96dcf23cqz97l5kjzg6yc4x9fxetsnl9k3pffg5glchn9wgr"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification delete "/v1/patterns/{pattern-fragment}/{pattern-fragment}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "DELETE" }
                & flip Wai.setPath "/v1/patterns/addr_vkh18sma906m44dp9w7dtcmr6kk8mmqtue3qnacezsyt6t7u5dqw3xh/*"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification put "/v1/patterns/{pattern-fragment}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "PUT" }
                & flip Wai.setPath "/v1/patterns/addr_test1vql8x96dcf23cqz97l5kjzg6yc4x9fxetsnl9k3pffg5glchn9wgr"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification put "/v1/patterns/{pattern-fragment}/{pattern-fragment}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint Http.status200
            res <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "PUT" }
                & flip Wai.setPath "/v1/patterns/addr_vkh18sma906m44dp9w7dtcmr6kk8mmqtue3qnacezsyt6t7u5dqw3xh/*"
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

        session' "🕱 DELETE /v1/patterns/*" $ do
            resBadRequest <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "DELETE" }
                & flip Wai.setPath "/v1/patterns/*"
            resBadRequest
                & Wai.assertStatus (Http.statusCode Http.status400)
            resBadRequest
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

--
-- Stubs
--

newStubbedApplication :: IO Application
newStubbedApplication = do
    patterns <- newTVarIO =<< generate (vectorOf 10 genPattern)
    pure $ app (\callback -> callback databaseStub) patterns healthStub

healthStub :: IO Health
healthStub =
    generate genHealth

databaseStub :: Database IO
databaseStub = Database
    { insertInputs =
        \_ -> return ()
    , foldInputsByAddress = \_ callback -> lift $ do
        rows <- fmap resultToRow <$> generate (listOf1 genResult)
        mapM_ callback rows
    , insertCheckpoint =
        \_ -> return ()
    , listCheckpointsDesc = \mk -> lift $ do
        fmap (mk . pointToRow) <$> generate (listOf1 genNonGenesisPoint)
    , insertPatterns =
        \_ -> return ()
    , deletePattern =
        \_ -> return ()
    , listPatterns = \mk -> lift $ do
        fmap (mk . patternToRow) <$> generate (listOf1 genPattern)
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
session specification opL path callback =
    prop (method <> " " <> toString path) $ monadicIO $ do
        stub <- run newStubbedApplication
        (json, errs) <- run $ Wai.runSession (callback assertJson endpoint) stub
        monitor $ counterexample (decodeUtf8 (Json.encode json))
        forM_ errs (monitor . counterexample . show)
        assert (null errs)
  where
    Identity (endpoint, method) = do
        item <- oops "No specification for path"
            (specification ^. paths . at (toString path))
        op <- oops "Operation not found" (item ^. opL)
        pure ( op
             , if
                | item ^. get    == Just op -> "GET"
                | item ^. put    == Just op -> "PUT"
                | item ^. patch  == Just op -> "PATCH"
                | item ^. post   == Just op -> "POST"
                | item ^. delete == Just op -> "DELETE"
                | otherwise                 -> "UNKNOWN"
             )

    assertJson schema res = do
        res & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)
        case Json.eitherDecode' (Wai.simpleBody res) of
            Left e ->
                liftIO $ fail (show e)
            Right json ->
                pure (json, validateJSON definitions schema json)
      where
        definitions = specification ^. components . schemas

session'
    :: Text
    -> Wai.Session ()
    -> Spec
session' path s = do
    specify (toString path) $ do
        stub <- newStubbedApplication
        Wai.runSession s stub

oops :: (HasCallStack, Applicative f) => Text -> Maybe a -> f a
oops str = maybe (error str) pure
