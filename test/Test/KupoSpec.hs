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
spec = skippableContext "End-to-end" $ \manager -> do
    specify "in memory" $ \(_, tr, cfg) -> do
        env <- newEnvironment $ cfg
            { workDir = InMemory
            , since = Just somePoint
            , patterns = [MatchAny OnlyShelley]
            }
        let HttpClient{..} = newHttpClient manager cfg
        let timeLimit = case chainProducer cfg of
                CardanoNode{} -> 5
                Ogmios{} -> 10
        timeoutOrThrow timeLimit $ race_
            (kupo tr `runWith` env)
            (do
                waitForServer
                waitUntil (> (getPointSlotNo somePoint + 100_000))
                matches <- getAllMatches
                length matches `shouldSatisfy` (> 12_000)
                healthCheck (serverHost cfg) (serverPort cfg)
            )

    specify "on disk (start → restart)" $ \(tmp, tr, cfg) -> do
        let HttpClient{..} = newHttpClient manager cfg

        ( do -- Can start the server on a fresh new db
            env <- newEnvironment $ cfg
                { workDir = Dir tmp
                , since = Just somePoint
                , patterns = [MatchAny IncludingBootstrap]
                }
            timeoutOrThrow 5 $ race_
                (kupo tr `runWith` env)
                (do
                    waitForServer
                    waitUntil (> (getPointSlotNo somePoint))
                )
          )

        ( do -- Can restart the server
            env <- newEnvironment $ cfg
                { workDir = Dir tmp
                , since = Just somePoint
                , patterns = [MatchAny IncludingBootstrap]
                }
            timeoutOrThrow 5 $ race_
                (kupo tr `runWith` env)
                (do
                    waitForServer
                    cps <- listCheckpoints
                    maximum cps `shouldSatisfy` (> (getPointSlotNo somePoint))
                )
          )

        ( do -- Can't restart with different, too recent, --since target on same db
            env <- newEnvironment $ cfg
                { workDir = Dir tmp
                , since = Just someOtherPoint
                , patterns = [MatchAny IncludingBootstrap]
                }
            shouldThrowTimeout @ConflictingOptionException 1 (kupo tr `runWith` env)
          )

        ( do -- Can't restart with different, non-empty, patterns
            env <- newEnvironment $ cfg
                { workDir = Dir tmp
                , since = Just somePoint
                , patterns = [MatchAny OnlyShelley]
                }
            shouldThrowTimeout @ConflictingOptionException 1 (kupo tr `runWith` env)
          )

    specify "Can't start the server on a fresh new db without explicit point" $ \(tmp, tr, cfg) -> do
        env <- newEnvironment $ cfg
            { workDir = Dir tmp
            , since = Nothing
            , patterns = []
            }
        shouldThrowTimeout @NoStartingPointException 1 (kupo tr `runWith` env)

    specify "Retry and wait when node isn't available" $ \(tmp, tr, cfg) -> do
        let HttpClient{..} = newHttpClient manager cfg
        env <- newEnvironment $ cfg
            { workDir = Dir tmp
            , since = Just GenesisPoint
            , patterns = []
            , chainProducer =
                case chainProducer cfg of
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

    specify "Crashes when no intersection is found" $ \(tmp, tr, cfg) -> do
        env <- newEnvironment $ cfg
            { workDir = Dir tmp
            , since = Just someNonExistingPoint
            , patterns = []
            }
        shouldThrowTimeout @IntersectionNotFoundException 1 (kupo tr `runWith` env)

type EndToEndSpec
    =  Manager
    -> SpecWith (FilePath, Tracers, Configuration)

skippableContext :: String -> EndToEndSpec -> Spec
skippableContext prefix skippableSpec = do
    ref <- runIO $ newIORef 1442
    let cardanoNode = prefix <> " (cardano-node)"
    runIO ((,) <$> lookupEnv varCardanoNodeSocket <*> lookupEnv varCardanoNodeConfig) >>= \case
        (Just nodeSocket, Just nodeConfig) -> do
            manager <- runIO $ newManager defaultManagerSettings
            let defaultCfg = Configuration
                    { chainProducer = CardanoNode { nodeSocket, nodeConfig }
                    , workDir = InMemory
                    , serverHost = "127.0.0.1"
                    , serverPort = 0
                    , since = Nothing
                    , patterns = []
                    }
            context cardanoNode $ around (withTempDirectory ref defaultCfg) $
                skippableSpec manager
        _ ->
            xcontext cardanoNode (pure ())

    let ogmios = prefix <> " (ogmios)"
    runIO ((,) <$> lookupEnv varOgmiosHost <*> lookupEnv varOgmiosPort) >>= \case
        (Just ogmiosHost, Just (Prelude.read -> ogmiosPort)) -> do
            manager <- runIO $ newManager defaultManagerSettings
            let defaultCfg = Configuration
                    { chainProducer = Ogmios { ogmiosHost, ogmiosPort }
                    , workDir = InMemory
                    , serverHost = "127.0.0.1"
                    , serverPort = 0
                    , since = Nothing
                    , patterns = []
                    }
            context ogmios $ around (withTempDirectory ref defaultCfg) $
                skippableSpec manager
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

    withTempDirectory
        :: IORef Int
        -> Configuration
        -> ((FilePath, Tracers, Configuration) -> IO ())
        -> IO ()
    withTempDirectory ref cfg action = do
        serverPort <- atomicModifyIORef' ref $ \port -> (succ port, port)
        withSystemTempDirectory "kupo-end-to-end" $ \dir -> do
            withTempFile dir "traces" $ \_ h ->
                withTracers h version (defaultTracers (Just Info)) $ \tr ->
                    action (dir, tr, cfg { serverPort })

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

newHttpClient :: Manager -> Configuration -> HttpClient IO
newHttpClient manager cfg = HttpClient
    { waitForServer
    , waitUntil
    , listCheckpoints
    , getAllMatches
    }
  where
    baseUrl :: String
    baseUrl = "http://" <> serverHost cfg <> ":" <> show (serverPort cfg)

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
