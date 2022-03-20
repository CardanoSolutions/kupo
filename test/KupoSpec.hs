-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module KupoSpec
    ( spec
    ) where

import Kupo.Prelude

import Control.Exception
    ( catch )
import Data.Aeson.Lens
    ( key, _Integer )
import Data.List
    ( maximum )
import Kupo
    ( kupo, newEnvironment, runWith, version, withTracers )
import Kupo.App
    ( Tracers )
import Kupo.App.Http
    ( healthCheck )
import Kupo.Configuration
    ( Configuration (..), NetworkParameters, StandardCrypto, WorkDir (..) )
import Kupo.Control.MonadAsync
    ( race_ )
import Kupo.Control.MonadDelay
    ( threadDelay )
import Kupo.Control.MonadLog
    ( Severity (..), defaultTracers )
import Kupo.Control.MonadTime
    ( timeout )
import Kupo.Data.ChainSync
    ( Block, Point, SlotNo (..), getPointSlotNo )
import Kupo.Data.Database
    ( pointFromRow )
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
    , shouldSatisfy
    , specify
    , xcontext
    )

import qualified Data.Aeson as Json
import qualified Kupo.Control.MonadDatabase as DB

spec :: Spec
spec = parallel $ skippableContext "End to end" $ \manager ntwrk defaultCfg -> do
    specify "in memory, only shelley" $ \(_, tr) -> do
        let serverPort = defaultPort
        env <- newEnvironment tr ntwrk $ defaultCfg
            { workDir = InMemory
            , since = Just somePoint
            , patterns = [MatchAny OnlyShelley]
            , serverPort
            }
        let HttpClient{..} = newHttpClient manager serverPort
        res <- timeout 5 $ race_
            (kupo tr `runWith` env)
            (do
                waitForServer
                waitUntil (> (getPointSlotNo somePoint + 100_000))
                matches <- getAllMatches
                length matches `shouldSatisfy` (> 12_000)
                healthCheck defaultHost serverPort
            )
        res `shouldSatisfy` isJust

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

somePoint :: Point (Block StandardCrypto)
somePoint = pointFromRow $ DB.Checkpoint
    { DB.checkpointSlotNo =
        51292637
    , DB.checkpointHeaderHash =
        unsafeDecodeBase16 "2e7ee124eccbc648789008f866969548\
                           \6f5727cada41b2d86d1c36355c76b771"
    }
