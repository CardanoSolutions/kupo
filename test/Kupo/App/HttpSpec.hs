-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.HttpSpec
    ( spec
    ) where

import Kupo.Prelude hiding
    ( get )

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
    , get
    , paths
    , responses
    , schemas
    , validateJSON
    )
import Kupo.App.Http
    ( app )
import Kupo.Control.MonadDatabase
    ( Database (..) )
import Kupo.Data.Database
    ( pointToRow, resultToRow )
import Kupo.Data.Generators
    ( genHealth, genNonGenesisPoint, genResult )
import Kupo.Data.Health
    ( Health )
import Network.HTTP.Media.MediaType
    ( MediaType, (//), (/:) )
import Network.HTTP.Media.RenderHeader
    ( renderHeader )
import Network.Wai
    ( Application )
import Paths_kupo
    ( getDataFileName )
import Test.Hspec
    ( Spec, parallel, runIO, specify )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( counterexample, generate, listOf1 )
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
        filepath <- getDataFileName "docs/openapi.yaml"
        Yaml.decodeFileThrow @IO @OpenApi filepath

    parallel $ do
        session specification "/v1/health" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint get Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/v1/health"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification "/v1/checkpoints" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint get Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/v1/checkpoints"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification "/v1/matches" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint get Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/v1/matches"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification "/v1/matches/{pattern-fragment}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint get Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/v1/matches/*"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session specification "/v1/matches/{pattern-fragment}/{pattern-fragment}" $ \assertJson endpoint -> do
            let schema = findSchema specification endpoint get Http.status200
            res <- Wai.request $ Wai.setPath Wai.defaultRequest "/v1/matches/*/*"
            res & Wai.assertStatus (Http.statusCode Http.status200)
            res & assertJson schema

        session' "GET /v1/does-not-exist" $ do
            resNotFound <- Wai.request $ Wai.defaultRequest
                & flip Wai.setPath "/v1/does-not-exist"
            resNotFound
                & Wai.assertStatus (Http.statusCode Http.status404)
            resNotFound
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

        session' "POST /v1/health" $ do
            resNotAllowed <- Wai.request $ Wai.defaultRequest
                { Wai.requestMethod = "POST" }
                & flip Wai.setPath "/v1/health"
            resNotAllowed
                & Wai.assertStatus (Http.statusCode Http.status406)
            resNotAllowed
                & Wai.assertHeader Http.hContentType (renderHeader mediaTypeJson)

--
-- Stubs
--

newStubbedApplication :: IO Application
newStubbedApplication =
    pure $ app (\callback -> callback databaseStub) healthStub

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
    -> PathItem
    -> Lens' PathItem (Maybe Operation)
    -> Http.Status
    -> Schema
findSchema specification item l status = runIdentity $ do
    op  <- oops "operation not found" $ item ^. l
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
  where
    oops str = maybe (error str) pure

session
    :: OpenApi
    -> Text
    -> ( (Schema -> Wai.SResponse -> Wai.Session (Json.Value, [ValidationError]))
       -> PathItem
       -> Wai.Session (Json.Value, [ValidationError])
       )
    -> Spec
session specification path callback =
    prop (toString path) $ monadicIO $ do
        stub <- run newStubbedApplication
        (json, errs) <- run $ Wai.runSession (callback assertJson endpoint) stub
        monitor $ counterexample (decodeUtf8 (Json.encode json))
        forM_ errs (monitor . counterexample . show)
        assert (null errs)
  where
    endpoint = fromMaybe
        (error ("No path in specification for: " <> path))
        (specification ^. paths . at (toString path))

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
