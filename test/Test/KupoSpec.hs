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
    ( try
    )
import Data.List
    ( maximum
    , (\\)
    )
import GHC.IORef
    ( atomicSwapIORef
    )
import Kupo
    ( kupo
    , newEnvironment
    , runWith
    , version
    , withTracers
    )
import Kupo.App.Configuration
    ( ConflictingOptionsException
    , NoStartingPointException
    )
import Kupo.App.Http
    ( healthCheck
    )
import Kupo.Control.MonadAsync
    ( race_
    )
import Kupo.Control.MonadDelay
    ( threadDelay
    )
import Kupo.Control.MonadLog
    ( Severity (..)
    , TracerDefinition (..)
    , defaultTracers
    )
import Kupo.Control.MonadTime
    ( DiffTime
    , timeout
    )
import Kupo.Data.Cardano
    ( getPointSlotNo
    , hasPolicyId
    , mkOutputReference
    , pattern GenesisPoint
    )
import Kupo.Data.ChainSync
    ( IntersectionNotFoundException
    )
import Kupo.Data.Configuration
    ( ChainProducer (..)
    , Configuration (..)
    , InputManagement (..)
    , WorkDir (..)
    )
import Kupo.Data.Http.GetCheckpointMode
    ( GetCheckpointMode (..)
    )
import Kupo.Data.Http.StatusFlag
    ( StatusFlag (..)
    )
import Kupo.Data.Pattern
    ( MatchBootstrap (..)
    , Pattern (..)
    , Result (..)
    )
import Kupo.Options
    ( Tracers (..)
    )
import Network.HTTP.Client
    ( Manager
    , defaultManagerSettings
    , newManager
    )
import System.Environment
    ( lookupEnv
    )
import System.IO.Temp
    ( withSystemTempDirectory
    , withTempFile
    )
import Test.Hspec
    ( Spec
    , SpecWith
    , around
    , context
    , runIO
    , shouldBe
    , shouldContain
    , shouldReturn
    , shouldSatisfy
    , specify
    , xcontext
    )
import Test.Kupo.App.Http.Client
    ( HttpClient (..)
    , newHttpClientWith
    )
import Test.Kupo.Fixture
    ( eraBoundaries
    , lastAlonzoPoint
    , lastByronPoint
    , lastMaryPoint
    , someDatumHashInOutput
    , someDatumHashInWitness
    , someDatumInOutput
    , someDatumInWitness
    , someNonExistingPoint
    , someOtherPoint
    , someOtherStakeKey
    , somePoint
    , somePointAncestor
    , somePointNearScripts
    , somePointSuccessor
    , somePolicyId
    , someScriptHashInMetadata
    , someScriptHashInOutput
    , someScriptHashInWitness
    , someScriptInMetadata
    , someScriptInOutput
    , someScriptInWitness
    , someStakeKey
    , someTransactionId
    )
import Type.Reflection
    ( tyConName
    , typeRep
    , typeRepTyCon
    )

import qualified Prelude

spec :: Spec
spec = skippableContext "End-to-end" $ \manager -> do
    specify "in memory" $ \(_, tr, cfg) -> do
        env <- newEnvironment $ cfg
            { workDir = InMemory
            , since = Just GenesisPoint
            , patterns = [MatchAny IncludingBootstrap]
            }
        let HttpClient{..} = newHttpClientWith manager (serverHost cfg, serverPort cfg)
        let timeLimit = case chainProducer cfg of
                CardanoNode{} -> 5
                Ogmios{} -> 10
        timeoutOrThrow timeLimit $ race_
            (kupo tr `runWith` env)
            (do
                waitUntilM $ do
                    matches <- getAllMatches NoStatusFlag
                    pure (length matches > 10)
                healthCheck (serverHost cfg) (serverPort cfg)
            )

    forM_ eraBoundaries $ \(era, point) -> specify ("quick sync through " <> era) $ \(_, tr, cfg) -> do
        env <- newEnvironment $ cfg
            { workDir = InMemory
            , since = Just point
            , patterns = [MatchAny IncludingBootstrap]
            }
        let HttpClient{..} = newHttpClientWith manager (serverHost cfg, serverPort cfg)
        timeoutOrThrow 5 $ race_
            (kupo tr `runWith` env)
            (do
                waitSlot (> 1_000)
                healthCheck (serverHost cfg) (serverPort cfg)
            )

    specify "on disk (start → restart)" $ \(tmp, tr, cfg) -> do
        let HttpClient{..} = newHttpClientWith manager (serverHost cfg, serverPort cfg)

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
        let HttpClient{..} = newHttpClientWith manager (serverHost cfg, serverPort cfg)
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
        let HttpClient{..} = newHttpClientWith manager (serverHost cfg, serverPort cfg)
        env <- newEnvironment $ cfg
            { workDir = Dir tmp
            , since = Just somePoint
            , patterns = [MatchAny OnlyShelley]
            , inputManagement = RemoveSpentInputs
            }

        timeoutOrThrow 30 $ race_
            (kupo tr `runWith` env)
            (do
                waitSlot (> 50_000)
                matches <- getAllMatches NoStatusFlag
                all (isNothing . spentAt) matches `shouldBe` True
            )

    specify "Retrieve checkpoints and ancestors" $ \(tmp, tr, cfg) -> do
        let HttpClient{..} = newHttpClientWith manager (serverHost cfg, serverPort cfg)
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
                getCheckpointBySlot strict (prev (getPointSlotNo somePoint))
                    `shouldReturn` Nothing
                getCheckpointBySlot strict (getPointSlotNo somePointSuccessor)
                    `shouldReturn` Just somePointSuccessor
                getCheckpointBySlot strict  (prev (getPointSlotNo somePointSuccessor))
                    `shouldReturn` Nothing
                getCheckpointBySlot flex (prev (getPointSlotNo somePointSuccessor))
                    `shouldReturn` Just somePoint
            )

    specify "Retrieve datums associated with datum hashes" $ \(tmp, tr, cfg) -> do
        let HttpClient{..} = newHttpClientWith manager (serverHost cfg, serverPort cfg)
        env <- newEnvironment $ cfg
            { workDir = Dir tmp
            , since = Just lastAlonzoPoint
            , patterns = [MatchAny OnlyShelley]
            }
        timeoutOrThrow 20 $ race_
            (kupo tr `runWith` env)
            (do
                waitDatum someDatumHashInWitness
                    `shouldReturn` someDatumInWitness
                waitDatum someDatumHashInOutput
                    `shouldReturn` someDatumInOutput
            )

    specify "Retrieve scripts associated with script hashes" $ \(tmp, tr, cfg) -> do
        let HttpClient{..} = newHttpClientWith manager (serverHost cfg, serverPort cfg)
        env <- newEnvironment $ cfg
            { workDir = Dir tmp
            , since = Just somePointNearScripts
            , patterns = [MatchAny OnlyShelley]
            }
        timeoutOrThrow 20 $ race_
            (kupo tr `runWith` env)
            (do
                waitScript someScriptHashInWitness
                    `shouldReturn` someScriptInWitness
                waitScript someScriptHashInMetadata
                    `shouldReturn` someScriptInMetadata
                waitScript someScriptHashInOutput
                    `shouldReturn` someScriptInOutput
            )

    specify "Dynamically add pattern and restart to a past point" $ \(tmp, tr, cfg) -> do
        let HttpClient{..} = newHttpClientWith manager (serverHost cfg, serverPort cfg)
        env <- newEnvironment $ cfg
            { workDir = Dir tmp
            , since = Just lastByronPoint
            , patterns = [MatchDelegation someStakeKey]
            }
        let maxSlot = getPointSlotNo lastByronPoint + 50_000
        let onlyInWindow r
                | getPointSlotNo (createdAt r) <= maxSlot =
                    Just r { spentAt = Nothing }
                | otherwise =
                    Nothing
        ref <- newIORef ([], [])
        timeoutOrThrow 10 $ race_
            (kupo tr `runWith` env)
            (do
                waitSlot (>= maxSlot)
                xs <- mapMaybe onlyInWindow <$> getAllMatches NoStatusFlag
                res <- putPatternSince
                    (MatchDelegation someOtherStakeKey)
                    (Right lastByronPoint)
                res `shouldBe` True
                waitSlot (< maxSlot) -- Observe rollback
                waitSlot (>= maxSlot)
                ys <- mapMaybe onlyInWindow <$> getAllMatches NoStatusFlag
                ys `shouldContain` xs
                (sort <$> listPatterns) `shouldReturn`
                    [ MatchDelegation someStakeKey
                    , MatchDelegation someOtherStakeKey
                    ]
                atomicSwapIORef ref (xs, ys)
            )
        (xs, ys) <- readIORef ref
        withSystemTempDirectory "kupo-end-to-end" $ \tmp' -> do
            env' <- newEnvironment $ cfg
                { workDir = Dir tmp'
                , since = Just lastByronPoint
                , patterns = [MatchDelegation someOtherStakeKey]
                }
            timeoutOrThrow 10 $ race_
                (kupo tr `runWith` env')
                (do
                    waitSlot (>= maxSlot)
                    zs <- mapMaybe onlyInWindow <$> getAllMatches NoStatusFlag
                    ys \\ zs `shouldBe` xs
                )

    specify "Failing to insert patterns (failed to resolve point) doesn't disturb normal operations" $ \(tmp, tr, cfg) -> do
        let HttpClient{..} = newHttpClientWith manager (serverHost cfg, serverPort cfg)
        env <- newEnvironment $ cfg
            { workDir = Dir tmp
            , since = Just lastByronPoint
            , patterns = [MatchAny OnlyShelley]
            }
        timeoutOrThrow 10 $ race_
            (kupo tr `runWith` env)
            (do
                let maxSlot = getPointSlotNo lastByronPoint + 10_000
                waitSlot (>= maxSlot)
                slot <- maximum . fmap getPointSlotNo <$> listCheckpoints
                res <- putPatternSince
                    (MatchDelegation someOtherStakeKey)
                    (Left (maxSlot - 20_000))
                slot' <- maximum . fmap getPointSlotNo <$> listCheckpoints
                res `shouldBe` False
                listPatterns `shouldReturn` [MatchAny OnlyShelley]
                slot' `shouldSatisfy` (>= slot)
            )

    specify "Failing to insert patterns (non-existing point) doesn't disturb normal operations" $ \(tmp, tr, cfg) -> do
        let HttpClient{..} = newHttpClientWith manager (serverHost cfg, serverPort cfg)
        env <- newEnvironment $ cfg
            { workDir = Dir tmp
            , since = Just lastByronPoint
            , patterns = [MatchAny OnlyShelley]
            }
        timeoutOrThrow 10 $ race_
            (kupo tr `runWith` env)
            (do
                let maxSlot = getPointSlotNo lastByronPoint + 10_000
                waitSlot (>= maxSlot)
                slot <- maximum . fmap getPointSlotNo <$> listCheckpoints
                res <- putPatternSince
                    (MatchDelegation someOtherStakeKey)
                    (Right someNonExistingPoint)
                slot' <- maximum . fmap getPointSlotNo <$> listCheckpoints
                res `shouldBe` False
                listPatterns `shouldReturn` [MatchAny OnlyShelley]
                slot' `shouldSatisfy` (>= slot)
            )

    specify "Match by transaction id" $ \(tmp, tr, cfg) -> do
        let HttpClient{..} = newHttpClientWith manager (serverHost cfg, serverPort cfg)
        env <- newEnvironment $ cfg
            { workDir = Dir tmp
            , since = Just lastMaryPoint
            , patterns = [MatchTransactionId someTransactionId]
            }
        timeoutOrThrow 10 $ race_
            (kupo tr `runWith` env)
            (do
                waitUntilM $ do
                    outRefs <- fmap outputReference <$> getAllMatches NoStatusFlag
                    return $
                        mkOutputReference someTransactionId 0 `elem` outRefs
                        &&
                        mkOutputReference someTransactionId 1 `elem` outRefs
            )

    specify "Match by policy id" $ \(tmp, tr, cfg) -> do
        let HttpClient{..} = newHttpClientWith manager (serverHost cfg, serverPort cfg)
        env <- newEnvironment $ cfg
            { workDir = Dir tmp
            , since = Just lastMaryPoint
            , patterns = [MatchPolicyId somePolicyId]
            }
        timeoutOrThrow 10 $ race_
            (kupo tr `runWith` env)
            (do
                waitUntilM $ do
                    values <- fmap value <$> getAllMatches NoStatusFlag
                    return $ all (`hasPolicyId` somePolicyId) values
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
                    , garbageCollectionInterval = 180
                    }
            context cardanoNode $ around (withTempDirectory ref defaultCfg) $
                skippableSpec manager
        _skipOtherwise ->
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
                    , garbageCollectionInterval = 180
                    }
            context ogmios $ around (withTempDirectory ref defaultCfg) $
                skippableSpec manager
        _skipOtherwise ->
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
        serverPort <- atomicModifyIORef' ref $ \port -> (next port, port)
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
