-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
import Kupo.App.Http
    ( healthCheck )
import Kupo.Configuration
    ( Configuration (..), StandardCrypto, WorkDir (..) )
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
    ( withSystemTempFile )
import Test.Hspec
    ( Spec, context, runIO, shouldSatisfy, specify, xcontext )

import qualified Data.Aeson as Json
import qualified Kupo.Control.MonadDatabase as DB

spec :: Spec
spec = do
    skippableContext "End to end" $ \nodeSocket nodeConfig -> do
        specify "in memory, only shelley" $ do
            let cfg = Configuration
                    { nodeSocket
                    , nodeConfig
                    , workDir = InMemory
                    , serverHost = defaultHost
                    , serverPort = defaultPort
                    , since = Just somePoint
                    , patterns = [MatchAny OnlyShelley]
                    }
            ntwrk <- parseNetworkParameters nodeConfig
            manager <- newManager defaultManagerSettings
            withSystemTempFile "kupo-end-to-end" $ \_ h -> do
                withTracers h version (defaultTracers (Just Info)) $ \tr -> do
                    env <- newEnvironment tr ntwrk cfg
                    res <- timeout 5 $ race_
                        (kupo tr `runWith` env)
                        (do
                            waitForServer manager
                            waitUntil manager (> (getPointSlotNo somePoint + 100_000))
                            matches <- getAllMatches manager
                            length matches `shouldSatisfy` (> 12_000)
                            healthCheck defaultHost defaultPort
                        )
                    res `shouldSatisfy` isJust

skippableContext :: String -> (FilePath -> FilePath -> Spec) -> Spec
skippableContext title skippableSpec = do
    runIO ((,) <$> lookupEnv varSocket <*> lookupEnv varConfig) >>= \case
        (Just nodeSocket, Just nodeConfig) -> do
            context title (skippableSpec nodeSocket nodeConfig)
        _ ->
            xcontext title (pure ())
  where
    varSocket :: String
    varSocket = "CARDANO_NODE_SOCKET"

    varConfig :: String
    varConfig = "CARDANO_NODE_CONFIG"

--
-- Strawman HTTP DSL
--

waitForServer :: Manager -> IO ()
waitForServer manager = do
    req <- parseRequest ("http://" <> defaultHost <> ":" <> show defaultPort <> "/v1/health")
    void (httpNoBody req manager) `catch` (\(_ :: HttpException) -> do
        threadDelay 0.1
        waitForServer manager)

waitUntil :: Manager -> (SlotNo -> Bool) -> IO ()
waitUntil manager predicate = do
    checkpoints <- listCheckpoints manager
    unless (predicate (maximum checkpoints)) $ do
        threadDelay 0.1
        waitUntil manager predicate

listCheckpoints :: Manager -> IO [SlotNo]
listCheckpoints manager = do
    req <- parseRequest ("http://" <> defaultHost <> ":" <> show defaultPort <> "/v1/checkpoints")
    res <- httpLbs req manager
    let body = responseBody res
    case Json.eitherDecode' body of
        Left e ->
            fail (show e)
        Right (xs :: [Json.Value]) -> do
            pure $ SlotNo . maybe 0 fromIntegral . (\x -> x ^? key "slot_no" . _Integer) <$> xs

getAllMatches :: Manager -> IO [Json.Value]
getAllMatches manager = do
    req <- parseRequest ("http://" <> defaultHost <> ":" <> show defaultPort <> "/v1/matches")
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
