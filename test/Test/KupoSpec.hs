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
    ( try )
import Data.Aeson.Lens
    ( key, _Value )
import Data.List
    ( maximum )
import Kupo
    ( kupo, newEnvironment, runWith, version, withTracers )
import Kupo.App.Configuration
    ( ConflictingOptionsException, NoStartingPointException )
import Kupo.App.Http
    ( healthCheck )
import Kupo.Control.MonadAsync
    ( race_ )
import Kupo.Control.MonadDelay
    ( threadDelay )
import Kupo.Control.MonadLog
    ( Severity (..), TracerDefinition (..), defaultTracers )
import Kupo.Control.MonadTime
    ( DiffTime, timeout )
import Kupo.Data.Cardano
    ( pattern GenesisPoint, getPointSlotNo )
import Kupo.Data.ChainSync
    ( IntersectionNotFoundException )
import Kupo.Data.Configuration
    ( ChainProducer (..)
    , Configuration (..)
    , InputManagement (..)
    , WorkDir (..)
    )
import Kupo.Data.Http.GetCheckpointMode
    ( GetCheckpointMode (..) )
import Kupo.Data.Pattern
    ( MatchBootstrap (..), Pattern (..) )
import Kupo.Options
    ( Tracers (..) )
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
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
    , shouldReturn
    , shouldSatisfy
    , specify
    , xcontext
    )
import Test.Kupo.App.Http.Client
    ( HttpClient (..), newHttpClientWith )
import Test.Kupo.Fixture
    ( eraBoundaries
    , lastAlonzoPoint
    , someDatumHashInOutput
    , someDatumHashInWitness
    , someDatumInOutput
    , someDatumInWitness
    , someNonExistingPoint
    , someOtherPoint
    , somePoint
    , somePointAncestor
    , somePointSuccessor
    )
import Type.Reflection
    ( tyConName, typeRep, typeRepTyCon )

import qualified Data.Aeson as Json
import qualified Prelude

spec :: Spec
spec = skippableContext "End-to-end" $ \manager -> do
    specify "in memory" $ \(_, tr, cfg) -> do
        env <- newEnvironment $ cfg
            { workDir = InMemory
            , since = Just GenesisPoint
            , patterns = [MatchAny IncludingBootstrap]
            }
        HttpClient{..} <- newHttpClientWith manager cfg
        let timeLimit = case chainProducer cfg of
                CardanoNode{} -> 5
                Ogmios{} -> 10
        timeoutOrThrow timeLimit $ race_
            (kupo tr `runWith` env)
            (do
                waitUntilM $ do
                    matches <- getAllMatches
                    pure (length matches > 10)
                healthCheck (serverHost cfg) (serverPort cfg)
            )

    forM_ eraBoundaries $ \(era, point) -> specify ("quick sync through " <> era) $ \(_, tr, cfg) -> do
        env <- newEnvironment $ cfg
            { workDir = InMemory
            , since = Just point
            , patterns = [MatchAny IncludingBootstrap]
            }
        HttpClient{..} <- newHttpClientWith manager cfg
        timeoutOrThrow 5 $ race_
            (kupo tr `runWith` env)
            (do
                waitSlot (> 1_000)
                healthCheck (serverHost cfg) (serverPort cfg)
            )

    specify "on disk (start → restart)" $ \(tmp, tr, cfg) -> do
        HttpClient{..} <- newHttpClientWith manager cfg

        ( do -- Can start the server on a fresh new db
            env <- newEnvironment $ cfg
                { workDir = Dir tmp
                , since = Just somePoint
                , patterns = [MatchAny OnlyShelley]
                }
            timeoutOrThrow 5 $ race_
                (kupo tr `runWith` env)
                (do
                    waitSlot (> (getPointSlotNo somePoint))
                )
          )

        ( do -- Can restart the server
            env <- newEnvironment $ cfg
                { workDir = Dir tmp
                , since = Just somePoint
                , patterns = [MatchAny OnlyShelley]
                }
            timeoutOrThrow 5 $ race_
                (kupo tr `runWith` env)
                (do
                    cps <- fmap getPointSlotNo <$> listCheckpoints
                    maximum cps `shouldSatisfy` (> (getPointSlotNo somePoint))
                )
          )

        ( do -- Can't restart with different, too recent, --since target on same db
            env <- newEnvironment $ cfg
                { workDir = Dir tmp
                , since = Just someOtherPoint
                , patterns = [MatchAny OnlyShelley]
                }
            shouldThrowTimeout @ConflictingOptionsException 1 (kupo tr `runWith` env)
          )

        ( do -- Can't restart with different, non-empty, patterns
            env <- newEnvironment $ cfg
                { workDir = Dir tmp
                , since = Just somePoint
                , patterns = [MatchAny IncludingBootstrap]
                }
            shouldThrowTimeout @ConflictingOptionsException 1 (kupo tr `runWith` env)
          )

    specify "Can't start the server on a fresh new db without explicit point" $ \(tmp, tr, cfg) -> do
        env <- newEnvironment $ cfg
            { workDir = Dir tmp
            , since = Nothing
            , patterns = [MatchAny OnlyShelley]
            }
        shouldThrowTimeout @NoStartingPointException 1 (kupo tr `runWith` env)

    specify "Retry and wait when chain producer isn't available" $ \(tmp, tr, cfg) -> do
        HttpClient{..} <- newHttpClientWith manager cfg
        env <- newEnvironment $ cfg
            { workDir = Dir tmp
            , since = Just GenesisPoint
            , patterns = [MatchAny OnlyShelley]
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
                cps <- listCheckpoints
                threadDelay 1
                cps' <- listCheckpoints
                cps `shouldBe` cps'
            )

    specify "Crashes when no intersection is found" $ \(tmp, tr, cfg) -> do
        env <- newEnvironment $ cfg
            { workDir = Dir tmp
            , since = Just someNonExistingPoint
            , patterns = [MatchAny OnlyShelley]
            }
        shouldThrowTimeout @IntersectionNotFoundException 1 (kupo tr `runWith` env)

    specify "Crashes when no patterns are defined" $ \(tmp, tr, cfg) -> do
        env <- newEnvironment $ cfg
            { workDir = Dir tmp
            , since = Just somePoint
            , patterns = []
            }
        shouldThrowTimeout @ConflictingOptionsException 1 (kupo tr `runWith` env)

    specify "Can prune utxo on-the-fly" $ \(tmp, tr, cfg) -> do
        HttpClient{..} <- newHttpClientWith manager cfg
        env <- newEnvironment $ cfg
            { workDir = Dir tmp
            , since = Just somePoint
            , patterns = [MatchAny OnlyShelley]
            , inputManagement = RemoveSpentInputs
            }

        let isUnspent :: Json.Value -> Bool
            isUnspent x = (x ^? key "spent_at" . _Value) == Just Json.Null

        timeoutOrThrow 30 $ race_
            (kupo tr `runWith` env)
            (do
                waitSlot (> 50_000)
                matches <- getAllMatches
                all isUnspent matches `shouldBe` True
            )

    specify "Retrieve checkpoints and ancestors" $ \(tmp, tr, cfg) -> do
        HttpClient{..} <- newHttpClientWith manager cfg
        env <- newEnvironment $ cfg
            { workDir = Dir tmp
            , since = Just somePointAncestor
            , patterns = [MatchAny OnlyShelley]
            , inputManagement = RemoveSpentInputs
            }
        let strict = GetCheckpointStrict
        let flex = GetCheckpointClosestAncestor
        timeoutOrThrow 5 $ race_
            (kupo tr `runWith` env)
            (do
                waitSlot (> (getPointSlotNo somePointSuccessor))
                getCheckpointBySlot strict (getPointSlotNo somePoint)
                    `shouldReturn` Just somePoint
                getCheckpointBySlot strict (pred (getPointSlotNo somePoint))
                    `shouldReturn` Nothing
                getCheckpointBySlot strict (getPointSlotNo somePointSuccessor)
                    `shouldReturn` Just somePointSuccessor
                getCheckpointBySlot strict  (pred (getPointSlotNo somePointSuccessor))
                    `shouldReturn` Nothing
                getCheckpointBySlot flex (pred (getPointSlotNo somePointSuccessor))
                    `shouldReturn` Just somePoint
            )

    specify "Retrieve datum(s) associated to datum-hash" $ \(tmp, tr, cfg) -> do
        HttpClient{..} <- newHttpClientWith manager cfg
        env <- newEnvironment $ cfg
            { workDir = Dir tmp
            , since = Just lastAlonzoPoint
            , patterns = [MatchAny OnlyShelley]
            }
        timeoutOrThrow 10 $ race_
            (kupo tr `runWith` env)
            (do
                lookupDatum someDatumHashInWitness `shouldReturn` someDatumInWitness
                lookupDatum someDatumHashInOutput `shouldReturn` someDatumInOutput
            )

type EndToEndSpec
    =  Manager
    -> SpecWith (FilePath, Tracers IO 'Concrete, Configuration)

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
                    , inputManagement = MarkSpentInputs
                    , longestRollback = 43200
                    , pruneThrottleDelay = 60
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
                    , inputManagement = MarkSpentInputs
                    , longestRollback = 43200
                    , pruneThrottleDelay = 60
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
        -> ((FilePath, Tracers IO 'Concrete, Configuration) -> IO ())
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
