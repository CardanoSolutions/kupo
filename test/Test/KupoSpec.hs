-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Test.KupoSpec
    ( spec
    ) where

import Kupo.Prelude

import Control.Exception
    ( catch, try )
import Data.Aeson.Lens
    ( key, _Integer )
import Data.List
    ( maximum )
import Kupo
    ( kupo, newEnvironment, runWith, version, withTracers )
import Kupo.App
    ( ConflictingOptionException, NoStartingPointException, Tracers )
import Kupo.App.Http
    ( healthCheck )
import Kupo.Configuration
    ( ChainProducer (..), Configuration (..), WorkDir (..) )
import Kupo.Control.MonadAsync
    ( race_ )
import Kupo.Control.MonadDelay
    ( threadDelay )
import Kupo.Control.MonadLog
    ( Severity (..), defaultTracers )
import Kupo.Control.MonadOuroboros
    ( IntersectionNotFoundException )
import Kupo.Control.MonadTime
    ( DiffTime, timeout )
import Kupo.Data.Cardano
    ( pattern GenesisPoint, SlotNo (..), getPointSlotNo )
import Kupo.Data.Pattern
    ( MatchBootstrap (..), Pattern (..) )
import Network.HTTP.Client
    ( HttpException
    , Manager
    , defaultManagerSettings
    , httpLbs
    , httpNoBody
    , newManager
    , parseRequest
    , responseBody
    )
import System.Environment
    ( lookupEnv )
import System.IO.Temp
    ( withSystemTempDirectory, withTempFile )
import Test.Hspec
    ( Spec
    , SpecWith
    , around
    , context
    , runIO
    , shouldBe
    , shouldSatisfy
    , specify
    , xcontext
    )
import Test.Kupo.Fixture
    ( someNonExistingPoint, someOtherPoint, somePoint )
import Type.Reflection
    ( tyConName, typeRep, typeRepTyCon )

import qualified Data.Aeson as Json
import qualified Prelude

spec :: Spec
spec = skippableContext "End-to-end" $ \manager defaultCfg -> do
    specify "in memory" $ \(_, tr) -> do
        let serverPort' = serverPort defaultCfg + 0
        env <- newEnvironment $ defaultCfg
            { workDir = InMemory
            , since = Just somePoint
            , patterns = [MatchAny OnlyShelley]
            , serverPort = serverPort'
            }
        let HttpClient{..} = newHttpClient manager serverPort'
        let timeLimit = case chainProducer defaultCfg of
                CardanoNode{} -> 5
                Ogmios{} -> 10
        timeoutOrThrow timeLimit $ race_
            (kupo tr `runWith` env)
            (do
                waitForServer
                waitUntil (> (getPointSlotNo somePoint + 100_000))
                matches <- getAllMatches
                length matches `shouldSatisfy` (> 12_000)
                healthCheck defaultHost serverPort'
            )

    specify "on disk (start → restart)" $ \(tmp, tr) -> do
        let serverPort' = serverPort defaultCfg + 1
        let HttpClient{..} = newHttpClient manager serverPort'
        let cfg = defaultCfg
                { workDir = Dir tmp
                , since = Just somePoint
                , patterns = [MatchAny IncludingBootstrap]
                , serverPort = serverPort'
                }

        ( do -- Can start the server on a fresh new db
            env <- newEnvironment cfg
            timeoutOrThrow 5 $ race_
                (kupo tr `runWith` env)
                (do
                    waitForServer
                    waitUntil (> (getPointSlotNo somePoint))
                )
          )

        ( do -- Can restart the server
            env <- newEnvironment cfg
            timeoutOrThrow 5 $ race_
                (kupo tr `runWith` env)
                (do
                    waitForServer
                    cps <- listCheckpoints
                    maximum cps `shouldSatisfy` (> (getPointSlotNo somePoint))
                )
          )

        ( do -- Can't restart with different, too recent, --since target on same db
            env <- newEnvironment $ cfg { since = Just someOtherPoint }
            shouldThrowTimeout @ConflictingOptionException 1 (kupo tr `runWith` env)
          )

    specify "Can't start the server on a fresh new db without explicit point" $ \(tmp, tr) -> do
        let serverPort' = serverPort defaultCfg + 2
        env <- newEnvironment $ defaultCfg
            { workDir = Dir tmp
            , since = Nothing
            , patterns = []
            , serverPort = serverPort'
            }
        shouldThrowTimeout @NoStartingPointException 1 (kupo tr `runWith` env)

    specify "Retry and wait when node isn't available" $ \(tmp, tr) -> do
        let serverPort' = serverPort defaultCfg + 3
        let HttpClient{..} = newHttpClient manager serverPort'
        env <- newEnvironment $ defaultCfg
            { workDir = Dir tmp
            , since = Just GenesisPoint
            , patterns = []
            , chainProducer =
                case chainProducer defaultCfg of
                    CardanoNode{nodeConfig} ->
                        CardanoNode
                            { nodeSocket = "/dev/null"
                            , nodeConfig
                            }
                    Ogmios{ogmiosPort} ->
                        Ogmios
                            { ogmiosHost = "/dev/null"
                            , ogmiosPort
                            }
            , serverPort = serverPort'
            }
        timeoutOrThrow 5 $ race_
            (kupo tr `runWith` env)
            (do
                waitForServer
                cps <- listCheckpoints
                threadDelay 1
                cps' <- listCheckpoints
                cps `shouldBe` cps'
            )

    specify "Crashes when no intersection is found" $ \(tmp, tr) -> do
        let serverPort' = serverPort defaultCfg + 4
        env <- newEnvironment $ defaultCfg
            { workDir = Dir tmp
            , since = Just someNonExistingPoint
            , patterns = []
            , serverPort = serverPort' + 1
            }
        shouldThrowTimeout @IntersectionNotFoundException 1 (kupo tr `runWith` env)

type EndToEndSpec
    =  Manager
    -> Configuration
    -> SpecWith (FilePath, Tracers)

skippableContext :: String -> EndToEndSpec -> Spec
skippableContext prefix skippableSpec = do
    let cardanoNode = prefix <> " (cardano-node)"
    runIO ((,) <$> lookupEnv varCardanoNodeSocket <*> lookupEnv varCardanoNodeConfig) >>= \case
        (Just nodeSocket, Just nodeConfig) -> do
            manager <- runIO $ newManager defaultManagerSettings
            let defaultCfg = Configuration
                    { chainProducer = CardanoNode { nodeSocket, nodeConfig }
                    , workDir = InMemory
                    , serverHost = defaultHost
                    , serverPort = 1442
                    , since = Nothing
                    , patterns = []
                    }
            context cardanoNode $ around withTempDirectory $
                skippableSpec manager defaultCfg
        _ ->
            xcontext cardanoNode (pure ())

    let ogmios = prefix <> " (ogmios)"
    runIO ((,) <$> lookupEnv varOgmiosHost <*> lookupEnv varOgmiosPort) >>= \case
        (Just ogmiosHost, Just (Prelude.read -> ogmiosPort)) -> do
            manager <- runIO $ newManager defaultManagerSettings
            let defaultCfg = Configuration
                    { chainProducer = Ogmios { ogmiosHost, ogmiosPort }
                    , workDir = InMemory
                    , serverHost = defaultHost
                    , serverPort = 2442
                    , since = Nothing
                    , patterns = []
                    }
            context ogmios $ around withTempDirectory $
                skippableSpec manager defaultCfg
        _ ->
            xcontext ogmios (pure ())
  where
    varCardanoNodeSocket :: String
    varCardanoNodeSocket = "CARDANO_NODE_SOCKET"

    varCardanoNodeConfig :: String
    varCardanoNodeConfig = "CARDANO_NODE_CONFIG"

    varOgmiosHost :: String
    varOgmiosHost = "OGMIOS_HOST"

    varOgmiosPort :: String
    varOgmiosPort = "OGMIOS_PORT"

    withTempDirectory :: ((FilePath, Tracers) -> IO ()) -> IO ()
    withTempDirectory action = withSystemTempDirectory "kupo-end-to-end" $ \dir -> do
        withTempFile dir "traces" $ \_ h ->
            withTracers h version (defaultTracers (Just Info)) $ \tr ->
                action (dir, tr)

timeoutOrThrow :: DiffTime -> IO () -> IO ()
timeoutOrThrow t action = do
    res <- timeout t action
    res `shouldSatisfy` isJust

shouldThrowTimeout :: forall e. (Exception e) => DiffTime -> IO () -> IO ()
shouldThrowTimeout t action = do
    timeout t (try action) >>= \case
        Nothing ->
            fail $ "shouldThrowTimeout: timed out after " <> show t
        Just (Right ()) ->
            fail "shouldThrowTimeout: should have thrown but didn't."
        Just (Left (e :: SomeException)) -> do
            case fromException e of
                Nothing ->
                    fail $ "shouldThrowTimeout: should have thrown '"
                         <> exceptionName <> "' but did throw instead: "
                         <> show e
                Just (_ :: e) ->
                    pure ()
  where
    exceptionName = tyConName (typeRepTyCon (typeRep @e))

--
-- Strawman HTTP DSL
--

data HttpClient (m :: Type -> Type) = HttpClient
    { waitForServer :: m ()
    , waitUntil :: (SlotNo -> Bool) -> m ()
    , listCheckpoints :: m [SlotNo]
    , getAllMatches :: m [Json.Value]
    }

newHttpClient :: Manager -> Int -> HttpClient IO
newHttpClient manager port = HttpClient
    { waitForServer
    , waitUntil
    , listCheckpoints
    , getAllMatches
    }
  where
    baseUrl :: String
    baseUrl = "http://" <> defaultHost <> ":" <> show port

    waitForServer :: IO ()
    waitForServer = do
        req <- parseRequest (baseUrl <> "/v1/health")
        void (httpNoBody req manager) `catch` (\(_ :: HttpException) -> do
            threadDelay 0.1
            waitForServer)

    waitUntil :: (SlotNo -> Bool) -> IO ()
    waitUntil predicate = do
        checkpoints <- listCheckpoints
        unless (predicate (maximum checkpoints)) $ do
            threadDelay 0.1
            waitUntil predicate

    listCheckpoints :: IO [SlotNo]
    listCheckpoints = do
        req <- parseRequest (baseUrl <> "/v1/checkpoints")
        res <- httpLbs req manager
        let body = responseBody res
        case Json.eitherDecode' body of
            Left e ->
                fail (show e)
            Right (xs :: [Json.Value]) -> do
                pure $ SlotNo . maybe 0 fromIntegral . (\x -> x ^? key "slot_no" . _Integer) <$> xs

    getAllMatches :: IO [Json.Value]
    getAllMatches = do
        req <- parseRequest (baseUrl <> "/v1/matches")
        res <- httpLbs req manager
        let body = responseBody res
        case Json.eitherDecode' body of
            Left e ->
                fail (show e)
            Right xs ->
                pure xs

--
-- Fixture / Defaults
--

defaultHost :: String
defaultHost = "127.0.0.1"
