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
import Kupo.App.ChainSync
    ( IntersectionNotFoundException )
import Kupo.App.Http
    ( healthCheck )
import Kupo.Configuration
    ( Configuration (..), NetworkParameters, WorkDir (..) )
import Kupo.Control.MonadAsync
    ( race_ )
import Kupo.Control.MonadDelay
    ( threadDelay )
import Kupo.Control.MonadLog
    ( Severity (..), defaultTracers )
import Kupo.Control.MonadTime
    ( DiffTime, timeout )
import Kupo.Data.Cardano
    ( pattern GenesisPoint, SlotNo (..), getPointSlotNo )
import Kupo.Data.Pattern
    ( MatchBootstrap (..), Pattern (..) )
import Kupo.Options
    ( parseNetworkParameters )
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
    , parallel
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

spec :: Spec
spec = parallel $ skippableContext "End to end" $ \manager ntwrk defaultCfg -> do
    specify "in memory" $ \(_, tr) -> do
        let serverPort = defaultPort + 0
        env <- newEnvironment tr ntwrk $ defaultCfg
            { workDir = InMemory
            , since = Just somePoint
            , patterns = [MatchAny OnlyShelley]
            , serverPort
            }
        let HttpClient{..} = newHttpClient manager serverPort
        timeoutOrThrow 5 $ race_
            (kupo tr `runWith` env)
            (do
                waitForServer
                waitUntil (> (getPointSlotNo somePoint + 100_000))
                matches <- getAllMatches
                length matches `shouldSatisfy` (> 12_000)
                healthCheck defaultHost serverPort
            )

    specify "on disk (start → restart)" $ \(tmp, tr) -> do
        let serverPort = defaultPort + 1
        let HttpClient{..} = newHttpClient manager serverPort
        let cfg = defaultCfg
                { workDir = Dir tmp
                , since = Just somePoint
                , patterns = [MatchAny IncludingBootstrap]
                , serverPort
                }

        ( do -- Can start the server on a fresh new db
            env <- newEnvironment tr ntwrk cfg
            timeoutOrThrow 5 $ race_
                (kupo tr `runWith` env)
                (do
                    waitForServer
                    waitUntil (> (getPointSlotNo somePoint))
                )
          )

        ( do -- Can restart the server
            env <- newEnvironment tr ntwrk cfg
            timeoutOrThrow 5 $ race_
                (kupo tr `runWith` env)
                (do
                    waitForServer
                    cps <- listCheckpoints
                    maximum cps `shouldSatisfy` (> (getPointSlotNo somePoint))
                )
          )

        ( do -- Can't restart with different, too recent, --since target on same db
            env <- newEnvironment tr ntwrk $ cfg { since = Just someOtherPoint }
            shouldThrowTimeout @ConflictingOptionException 1 (kupo tr `runWith` env)
          )

    specify "Can't start the server on a fresh new db without explicit point" $ \(tmp, tr) -> do
        let serverPort = defaultPort + 2
        env <- newEnvironment tr ntwrk $ defaultCfg
            { workDir = Dir tmp
            , since = Nothing
            , patterns = []
            , serverPort
            }
        shouldThrowTimeout @NoStartingPointException 1 (kupo tr `runWith` env)

    specify "Retry and wait when node isn't available" $ \(tmp, tr) -> do
        let serverPort = defaultPort + 3
        let HttpClient{..} = newHttpClient manager serverPort
        env <- newEnvironment tr ntwrk $ defaultCfg
            { workDir = Dir tmp
            , since = Just GenesisPoint
            , patterns = []
            , nodeSocket = "/dev/null"
            , serverPort
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
        let serverPort = defaultPort + 4
        env <- newEnvironment tr ntwrk $ defaultCfg
            { workDir = Dir tmp
            , since = Just someNonExistingPoint
            , patterns = []
            , serverPort
            }
        shouldThrowTimeout @IntersectionNotFoundException 1 (kupo tr `runWith` env)

type EndToEndSpec
    =  Manager
    -> NetworkParameters
    -> Configuration
    -> SpecWith (FilePath, Tracers)

skippableContext :: String -> EndToEndSpec -> Spec
skippableContext title skippableSpec = do
    runIO ((,) <$> lookupEnv varSocket <*> lookupEnv varConfig) >>= \case
        (Just nodeSocket, Just nodeConfig) -> do
            ntwrk <- runIO $ parseNetworkParameters nodeConfig
            manager <- runIO $ newManager defaultManagerSettings
            let defaultCfg = Configuration
                    { nodeSocket
                    , nodeConfig
                    , workDir = InMemory
                    , serverHost = defaultHost
                    , serverPort = defaultPort
                    , since = Nothing
                    , patterns = []
                    }
            context title $ around withTempDirectory $
                skippableSpec manager ntwrk defaultCfg
        _ ->
            xcontext title (pure ())
  where
    varSocket :: String
    varSocket = "CARDANO_NODE_SOCKET"

    varConfig :: String
    varConfig = "CARDANO_NODE_CONFIG"

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

defaultPort :: Int
defaultPort = 1442
