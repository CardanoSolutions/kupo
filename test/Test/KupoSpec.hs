-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}

module Test.KupoSpec
    ( spec
    ) where

import Kupo.Prelude

import Control.Monad.Trans.Writer
    ( execWriterT
    , tell
    )
import Data.Aeson.Lens
    ( _Integer
    , _String
    , key
    )
import Data.List
    ( maximum
    , (\\)
    )
import GHC.IORef
    ( atomicSwapIORef
    )
import Kupo
    ( Env
    , Kupo
    , kupo
    , newEnvironmentWith
    , runWith
    , version
    , withTracers
    )
import Kupo.App.Configuration
    ( ConflictingOptionsException
    , NoStartingPointException
    )
import Kupo.App.Http.HealthCheck
    ( healthCheck
    )
import Kupo.Control.MonadAsync
    ( race_
    )
import Kupo.Control.MonadCatch
    ( MonadCatch (..)
    )
import Kupo.Control.MonadDelay
    ( threadDelay
    )
import Kupo.Control.MonadLog
    ( Severity (..)
    , defaultTracers
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Control.MonadTime
    ( DiffTime
    , diffTimeToMicroseconds
    , timeout
    )
import Kupo.Data.Cardano
    ( Point
    , getPointSlotNo
    , hasPolicyId
    , mkOutputReference
    , pattern GenesisPoint
    , pointFromText
    , unsafeValueFromList
    )
import Kupo.Data.ChainSync
    ( IntersectionNotFoundException
    )
import Kupo.Data.Configuration
    ( ChainProducer (..)
    , Configuration (..)
    , DatabaseLocation (..)
    , DeferIndexesInstallation (..)
    , InputManagement (..)
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
import Network.HTTP.Client
    ( Manager
    , ManagerSettings (..)
    , defaultManagerSettings
    , newManager
    , responseTimeoutNone
    )
import System.IO.Temp
    ( withSystemTempDirectory
    , withTempFile
    )
import System.Process
    ( CreateProcess (..)
    , proc
    , readCreateProcess
    )
import Test.Hspec
    ( Arg
    , Spec
    , SpecWith
    , around
    , context
    , runIO
    , shouldBe
    , shouldNotBe
    , shouldReturn
    , shouldSatisfy
    , specify
    , xcontext
    )
import Test.Kupo.App.Http.Client
    ( HttpClient (..)
    , newHttpClient
    , newHttpClientWith
    )
import Test.Kupo.Fixture
    ( eraBoundaries
    , lastAlonzoPoint
    , lastByronPoint
    , someDatumInOutput
    , someDatumInOutputHash
    , someDatumInWitness
    , someDatumInWitnessHash
    , someMetadata
    , someNonExistingPoint
    , someOtherPoint
    , someOtherStakeKey
    , somePhase2FailedTransactionIdWithReturn
    , somePoint
    , somePointAncestor
    , somePointNearPhase2Failure
    , somePointNearScripts
    , somePointSuccessor
    , somePolicyId
    , someScriptInMetadata
    , someScriptInMetadataHash
    , someScriptInOutput
    , someScriptInOutputHash
    , someScriptInWitness
    , someScriptInWitnessHash
    , someSlotWithMetadata
    , someStakeKey
    , someThirdTransactionId
    , someTransactionId
    , someTransactionIdWithMetadata
    )
import Type.Reflection
    ( tyConName
    , typeRep
    , typeRepTyCon
    )

import Control.Monad.Class.MonadThrow
    ( throwIO
    )
import Kupo.Data.Health
    ( ConnectionStatus (..)
    , Health (..)
    )
import System.IO
    ( hClose
    , hGetLine
    )

import qualified Data.Aeson as Json
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as Builder
import Kupo.Data.Configuration
    ( Since (..)
    )
import qualified Prelude

varCardanoNodeSocket :: String
varCardanoNodeSocket = "CARDANO_NODE_SOCKET"

varCardanoNodeConfig :: String
varCardanoNodeConfig = "CARDANO_NODE_CONFIG"

varOgmiosHost :: String
varOgmiosHost = "OGMIOS_HOST"

varOgmiosPort :: String
varOgmiosPort = "OGMIOS_PORT"

varHydraHost :: String
varHydraHost = "HYDRA_HOST"

varHydraPort :: String
varHydraPort = "HYDRA_PORT"

type EndToEndContext
    = ( (Configuration -> Configuration) -> IO (Configuration, Env Kupo)
      , Env Kupo -> DiffTime -> IO () -> IO ()
      , HttpClient IO
      )

endToEnd :: HasCallStack => String -> (EndToEndContext -> IO ()) -> SpecWith EndToEndContext
endToEnd = specify

spec :: Spec
spec = skippableContext "End-to-end" $ do
    endToEnd "can connect" $ \(configure, runSpec, HttpClient{..}) -> do
        (_cfg, env) <- configure $ \defaultCfg -> defaultCfg
            { databaseLocation = InMemory Nothing
            , since = Just (SincePoint GenesisPoint)
            , patterns = fromList [MatchAny OnlyShelley]
            }
        runSpec env 5 $ do
            waitSlot (> 0)
            matches <- getAllMatches NoStatusFlag
            matches `shouldSatisfy` not . null

    endToEnd "in-memory" $ \(configure, runSpec, HttpClient{..}) -> do
        (cfg, env) <- configure $ \defaultCfg -> defaultCfg
            { databaseLocation = InMemory Nothing
            , since = Just (SincePoint lastByronPoint)
            , patterns = fromList [MatchAny IncludingBootstrap]
            , deferIndexes = SkipNonEssentialIndexes
            }
        runSpec env 10 $ do
            waitSlot (> 1000)
            healthCheck (serverHost cfg) (serverPort cfg)

    forM_ eraBoundaries $ \(era, point) ->
        endToEnd ("quick sync through " <> era) $ \(configure, runSpec, HttpClient{..}) -> do
            (cfg, env) <- configure $ \defaultCfg -> defaultCfg
                { since = Just (SincePoint point)
                , patterns = fromList [MatchAny IncludingBootstrap]
                , deferIndexes = SkipNonEssentialIndexes
                }
            runSpec env 5 $ do
                cp <- maximum . (<> [0]) . fmap getPointSlotNo <$> listCheckpoints
                waitSlot (> (cp + 1_000))
                healthCheck (serverHost cfg) (serverPort cfg)

    endToEnd "start → restart(s)" $ \(configure, runSpec, HttpClient{..}) -> do
        do -- Can start the server on a fresh new db
            (_, env) <- configure $ \defaultCfg -> defaultCfg
                { since = Just (SincePoint somePoint)
                , patterns = fromList [MatchAny OnlyShelley]
                }
            runSpec env 5 $ waitSlot (> (getPointSlotNo somePoint))

        do -- Can restart the server
            (_, env) <- configure $ \defaultCfg -> defaultCfg
                { since = Just (SincePoint somePoint)
                , patterns = fromList [MatchAny OnlyShelley]
                }
            runSpec env 5 $ do
                cps <- fmap getPointSlotNo <$> listCheckpoints
                maximum cps `shouldSatisfy` (> (getPointSlotNo somePoint))

        do -- Can't restart with different, too recent, --since target on same db
            (_, env) <- configure $ \defaultCfg -> defaultCfg
                { since = Just (SincePoint someOtherPoint)
                , patterns = fromList [MatchAny OnlyShelley]
                }
            shouldThrowTimeout @ConflictingOptionsException 1 (runSpec env)

        do -- Can't restart with different, non-empty, patterns
            (_, env) <- configure $ \defaultCfg -> defaultCfg
                { since = Just (SincePoint somePoint)
                , patterns = fromList [MatchAny IncludingBootstrap]
                }
            shouldThrowTimeout @ConflictingOptionsException 1 (runSpec env)

    endToEnd "Can't start the server on a fresh new db without explicit point" $ \(configure, runSpec, _) -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Nothing
            , patterns = fromList [MatchAny OnlyShelley]
            }
        shouldThrowTimeout @NoStartingPointException 1 (runSpec env)

    endToEnd "Retry and wait when chain producer isn't available" $ \(configure, runSpec, HttpClient{..}) -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint GenesisPoint)
            , patterns = fromList [MatchAny OnlyShelley]
            , chainProducer =
                case chainProducer defaultCfg of
                    ReadOnlyReplica ->
                        ReadOnlyReplica
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
                    Hydra{hydraPort} ->
                        Hydra
                            { hydraHost = "/dev/null"
                            , hydraPort
                            }
            }
        runSpec env 5 $ do
            cps <- listCheckpoints
            threadDelay 1
            cps' <- listCheckpoints
            cps `shouldBe` cps'

    endToEnd "Crashes when no intersection is found" $ \(configure, runSpec, _) -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint someNonExistingPoint)
            , patterns = fromList [MatchAny OnlyShelley]
            }
        shouldThrowTimeout @IntersectionNotFoundException 1 (runSpec env)

    endToEnd "Crashes when no patterns are defined" $ \(configure, runSpec, _) -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint somePoint)
            , patterns = fromList []
            }
        shouldThrowTimeout @ConflictingOptionsException 1 (runSpec env)

    endToEnd "Can prune utxo on-the-fly" $ \(configure, runSpec, HttpClient{..}) -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint somePoint)
            , patterns = fromList [MatchAny OnlyShelley]
            , inputManagement = RemoveSpentInputs
            }
        runSpec env 30 $ do
            waitSlot (> 50_000)
            matches <- getAllMatches NoStatusFlag
            all (isNothing . spentAt) matches `shouldBe` True

    endToEnd "Retrieve checkpoints and ancestors" $ \(configure, runSpec, HttpClient{..}) -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint somePointAncestor)
            , patterns = fromList [MatchAny OnlyShelley]
            , inputManagement = RemoveSpentInputs
            }
        let slot = getPointSlotNo
        runSpec env 5 $ do
            waitSlot (> (getPointSlotNo somePointSuccessor))
            getCheckpointBySlot GetCheckpointStrict (slot somePoint)
                `shouldReturn` Just somePoint
            getCheckpointBySlot GetCheckpointStrict (prev (slot somePoint))
                `shouldReturn` Nothing
            getCheckpointBySlot GetCheckpointStrict (slot somePointSuccessor)
                `shouldReturn` Just somePointSuccessor
            getCheckpointBySlot GetCheckpointStrict  (prev (slot somePointSuccessor))
                `shouldReturn` Nothing
            getCheckpointBySlot GetCheckpointClosestAncestor (prev (slot somePointSuccessor))
                `shouldReturn` Just somePoint

    endToEnd "Retrieve datums associated with datum hashes" $ \(configure, runSpec, HttpClient{..}) -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint lastAlonzoPoint)
            , patterns = fromList [MatchAny OnlyShelley]
            }
        runSpec env 20 $ do
            waitDatum someDatumInWitnessHash
                `shouldReturn` someDatumInWitness
            waitDatum someDatumInOutputHash
                `shouldReturn` someDatumInOutput

    endToEnd "Retrieve scripts associated with script hashes" $ \(configure, runSpec, HttpClient{..}) -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint somePointNearScripts)
            , patterns = fromList [MatchAny OnlyShelley]
            }
        runSpec env 20 $ do
            waitScript someScriptInWitnessHash
                `shouldReturn` someScriptInWitness
            waitScript someScriptInMetadataHash
                `shouldReturn` someScriptInMetadata
            waitScript someScriptInOutputHash
                `shouldReturn` someScriptInOutput

    endToEnd "Dynamically add pattern and restart to a past point when syncing" $ \(configure, runSpec, HttpClient{..}) -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint lastByronPoint)
            , patterns = fromList [MatchDelegation someStakeKey]
            }
        -- NOTE: maxSlot must be high enough after the rollback point to have a chance to observe the
        -- rollback. If too low, Kupo might have already re-synchronised and we can't assert that a
        -- rollback did happen.
        let maxSlot = getPointSlotNo lastByronPoint + 100_000
        let onlyInWindow r
                | getPointSlotNo (createdAt r) <= maxSlot =
                    Just r { spentAt = Nothing }
                | otherwise =
                    Nothing
        ref <- newIORef ([], [])
        runSpec env 10 $ do
            waitSlot (>= maxSlot)
            xs <- mapMaybe onlyInWindow <$> getAllMatches NoStatusFlag
            xs `shouldNotBe` []
            res <- putPatternSince (MatchDelegation someOtherStakeKey) (Right lastByronPoint)
            res `shouldBe` True
            waitSlot (< maxSlot) -- Observe rollback
            waitSlot (>= maxSlot)
            ys <- mapMaybe onlyInWindow <$> getAllMatches NoStatusFlag
            (xs \\ ys) `shouldBe` []
            (sort <$> listPatterns) `shouldReturn`
                [ MatchDelegation someStakeKey
                , MatchDelegation someOtherStakeKey
                ]
            void $ atomicSwapIORef ref (xs, ys)
        (xs, ys) <- readIORef ref
        withSystemTempDirectory "kupo-end-to-end" $ \tmp' -> do
            (_, env') <- configure $ \defaultCfg -> defaultCfg
                { databaseLocation = Dir tmp'
                , since = Just (SincePoint lastByronPoint)
                , patterns = fromList [MatchDelegation someOtherStakeKey]
                }
            runSpec env' 10 $ do
                waitSlot (>= maxSlot)
                zs <- mapMaybe onlyInWindow <$> getAllMatches NoStatusFlag
                ys \\ zs `shouldBe` xs

    endToEnd "Failing to insert patterns (failed to resolve point) doesn't disturb normal operations" $ \(configure, runSpec, HttpClient{..})  -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint lastByronPoint)
            , patterns = fromList [MatchAny OnlyShelley]
            }
        runSpec env 10 $ do
            let maxSlot = getPointSlotNo lastByronPoint + 10_000
            waitSlot (>= maxSlot)
            slot <- maximum . fmap getPointSlotNo <$> listCheckpoints
            res <- putPatternSince (MatchDelegation someOtherStakeKey) (Left (maxSlot - 20_000))
            slot' <- maximum . fmap getPointSlotNo <$> listCheckpoints
            res `shouldBe` False
            listPatterns `shouldReturn` [MatchAny OnlyShelley]
            slot' `shouldSatisfy` (>= slot)

    endToEnd "Failing to insert patterns (non-existing point) doesn't disturb normal operations" $ \(configure, runSpec, HttpClient{..}) -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint lastByronPoint)
            , patterns = fromList [MatchAny OnlyShelley]
            }
        runSpec env 5 $ do
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

    endToEnd "Match by transaction id / output reference" $ \(configure, runSpec, HttpClient{..}) -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint lastAlonzoPoint)
            , patterns = fromList
                [ MatchTransactionId someTransactionId
                , MatchOutputReference (mkOutputReference someThirdTransactionId 0)
                ]
            }
        runSpec env 10 $ waitUntilM $ do
            outRefs <- fmap outputReference <$> getAllMatches NoStatusFlag
            return $
                (mkOutputReference someTransactionId 0, 0) `elem` outRefs
                &&
                (mkOutputReference someThirdTransactionId 0, 2) `elem` outRefs

    endToEnd "Match by policy id" $ \(configure, runSpec, HttpClient{..}) -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint lastAlonzoPoint)
            , patterns = fromList [MatchPolicyId somePolicyId]
            }
        runSpec env 10 $ waitUntilM $ do
            values <- fmap value <$> getAllMatches NoStatusFlag
            return $ all (`hasPolicyId` somePolicyId) values

    endToEnd "Fetch metadata by slot" $ \(configure, runSpec, HttpClient{..}) -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint lastAlonzoPoint)
            , patterns = fromList [MatchAny OnlyShelley]
            }
        runSpec env 10 $ do
            waitSlot (> someSlotWithMetadata)
            xs <- lookupMetadataBySlotNo someSlotWithMetadata Nothing
            [ hash | (hash, _meta) <- xs ] `shouldBe` someMetadata
            xs' <- lookupMetadataBySlotNo someSlotWithMetadata (Just someTransactionIdWithMetadata)
            [ hash | (hash, _meta) <- xs' ] `shouldBe` someMetadata

    endToEnd "Index collateral return from failed transactions" $ \(configure, runSpec, HttpClient{..}) -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint somePointNearPhase2Failure)
            , patterns = fromList [MatchTransactionId somePhase2FailedTransactionIdWithReturn]
            }
        runSpec env 10 $ do
            let predicate = (== (mkOutputReference somePhase2FailedTransactionIdWithReturn 1)) . fst . outputReference
            waitUntilM $ do
                results <- getAllMatches NoStatusFlag
                return (any predicate results)
            let matches = find predicate <$> getAllMatches NoStatusFlag
            matches >>= \case
                Nothing -> fail "impossible: the result disappeared?"
                Just r  -> value r `shouldBe` unsafeValueFromList 7_000_000 []

    endToEnd "Read-only replica eventually synchronize" $ \(configure, runSpec, httpClient) -> do
        (cfg, env) <- configure $ \defaultCfg -> defaultCfg
                { since = Just (SincePoint lastAlonzoPoint)
                , patterns = fromList [MatchAny OnlyShelley]
                }
        runSpec env 5 $ do
            waitSlot httpClient (> (getPointSlotNo lastAlonzoPoint))
            withReplica cfg $ \replicaHttpClient -> do
                mostRecentCheckpoint <- Prelude.head <$> listCheckpoints httpClient
                waitSlot replicaHttpClient (>= (getPointSlotNo mostRecentCheckpoint))
                Health{connectionStatus, configuration} <- getHealth replicaHttpClient
                connectionStatus `shouldBe` Connected
                configuration `shouldBe` Nothing

    endToEnd "Dynamically add pattern and restart to a past point when at the tip" $ \(configure, runSpec, HttpClient{..}) -> do
        tip <- currentNetworkTip
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint tip)
            , patterns = fromList [MatchAny IncludingBootstrap]
            }
        runSpec env 120 $ do
            waitSlot (>= getPointSlotNo tip)
            res <- putPatternSince (MatchDelegation someOtherStakeKey) (Right tip)
            res `shouldBe` True

    endToEnd "Auto-magically restart when reaching the tip (--defer-db-indexes enabled)" $ \(configure, runSpec, HttpClient{..}) -> do
        tip <- currentNetworkTip
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint tip)
            , patterns = fromList [MatchAny IncludingBootstrap]
            , deferIndexes = SkipNonEssentialIndexes
            }
        runSpec env 120 $ do
            waitSlot (>= getPointSlotNo tip)
            Health{configuration} <- getHealth
            configuration `shouldBe` (Just InstallIndexesIfNotExist)


-- | Create an 'EndToEndContext' around each child specification item within that 'Spec' tree. The
-- spec items are 'skippable' and only executed if the appropriate environment variables are present.
--
-- - If 'varCardanoNodeSocket' AND 'varCardanoNodeConfig' are set, the spec items will execute against
--   a Cardano node expected to be running and available through the context defined by these
--   variables.
--
-- - If 'varOgmiosHost' AND 'varOgmiosPort' are set, the spec items will execute against an Ogmios
-- server expected to be running and available through the context defined by these variables.
--
-- - If 'varHydraHost' AND 'varHydraPort' are set, the spec items will execute against a Hydra node
-- with an open head running and available through the context defined by these variables.
--
-- If either set of variables is missing, then the spec items do not run for that item.
skippableContext :: String -> SpecWith (Arg (EndToEndContext -> IO ())) -> Spec
skippableContext prefix skippableSpec = do
    ref <- runIO $ newTVarIO 1442
    let cardanoNode = prefix <> " (cardano-node)"
    runIO ((,) <$> lookupEnv varCardanoNodeSocket <*> lookupEnv varCardanoNodeConfig) >>= \case
        (Just nodeSocket, Just nodeConfig) -> do
            manager <- runIO $ newManager defaultManagerSettings
            let defaultCfg = Configuration
                    { chainProducer = CardanoNode { nodeSocket, nodeConfig }
                    , databaseLocation = InMemory Nothing
                    , serverHost = "127.0.0.1"
                    , serverPort = 0
                    , since = Nothing
                    , patterns = fromList []
                    , inputManagement = MarkSpentInputs
                    , longestRollback = 43200
                    , garbageCollectionInterval = 180
                    , deferIndexes = InstallIndexesIfNotExist
                    }
            context cardanoNode $ around (withTempDirectory manager ref defaultCfg) skippableSpec
        _skipOtherwise ->
            xcontext cardanoNode (pure ())

    let ogmios = prefix <> " (ogmios)"
    runIO ((,) <$> lookupEnv varOgmiosHost <*> lookupEnv varOgmiosPort) >>= \case
        (Just ogmiosHost, Just (Prelude.read -> ogmiosPort)) -> do
            manager <- runIO $ newManager $
                defaultManagerSettings { managerResponseTimeout = responseTimeoutNone }
            let defaultCfg = Configuration
                    { chainProducer = Ogmios { ogmiosHost, ogmiosPort }
                    , databaseLocation = InMemory Nothing
                    , serverHost = "127.0.0.1"
                    , serverPort = 0
                    , since = Nothing
                    , patterns = fromList []
                    , inputManagement = MarkSpentInputs
                    , longestRollback = 43200
                    , garbageCollectionInterval = 180
                    , deferIndexes = InstallIndexesIfNotExist
                    }
            context ogmios $ around (withTempDirectory manager ref defaultCfg) skippableSpec
        _skipOtherwise ->
            xcontext ogmios (pure ())

    let hydra = prefix <> " (hydra)"
    runIO ((,) <$> lookupEnv varHydraHost <*> lookupEnv varHydraPort) >>= \case
        (Just hydraHost, Just (Prelude.read -> hydraPort)) -> do
            manager <- runIO $ newManager $
                defaultManagerSettings { managerResponseTimeout = responseTimeoutNone }
            let defaultCfg = Configuration
                    { chainProducer = Hydra {hydraHost, hydraPort}
                    , databaseLocation = InMemory Nothing
                    , serverHost = "127.0.0.1"
                    , serverPort = 0
                    , since = Nothing
                    , patterns = fromList []
                    , inputManagement = MarkSpentInputs
                    , longestRollback = 43200
                    , garbageCollectionInterval = 180
                    , deferIndexes = InstallIndexesIfNotExist
                    }
            context hydra $ around (withTempDirectory manager ref defaultCfg) skippableSpec
        _skipOtherwise ->
            xcontext hydra (pure ())
  where
    withTempDirectory
        :: Manager
        -> TVar IO Int
        -> Configuration
        -> (EndToEndContext -> IO ())
        -> IO ()
    withTempDirectory manager ref defaultCfg action = do
        serverPort <- atomically $ stateTVar ref $ \port -> (port, next port)
        httpClientLogsVar <- newTVarIO []
        let writeLogs = atomically . modifyTVar' httpClientLogsVar . (:)
        let httpClient = newHttpClientWith manager (serverHost defaultCfg, serverPort) writeLogs
        withSystemTempDirectory "kupo-end-to-end" $ \dir -> do
            action
                ( \mkConfig -> do
                    let cfg = mkConfig (defaultCfg { serverPort, databaseLocation = Dir dir })
                    (cfg,) <$> newEnvironmentWith throwIO cfg
                , \env t test -> do
                        withTempFile dir "traces" $ \fp h -> do
                            withTracers h version (defaultTracers (Just Info)) $ \tr -> do
                                let runner = do
                                        res <- timeout
                                            (fromInteger @Int (diffTimeToMicroseconds t))
                                            (race_ (kupo tr `runWith` env) test)
                                        res `shouldSatisfy` isJust
                                runner `catch` \(e :: SomeException) -> do
                                    throwIO =<< collectLogs e (fp, h) httpClientLogsVar
                , httpClient
                )

    collectLogs :: SomeException -> (FilePath, Handle) -> TVar IO [Text] -> IO EndToEndException
    collectLogs originalException (fp, h) logs = do
        hClose h
        applicationLogs <- withFile fp ReadMode $ \h' -> do
            let hPrintLines = do
                    unlessM (lift (hIsEOF h')) $ do
                        lift (hGetLine h') >>= tell . pure . Builder.fromString
                        hPrintLines

            msgs <- execWriterT hPrintLines

            return $ toStrict . Builder.toLazyText . foldMap ((<> "\n")) $ drop (max 0 (length msgs - 10)) msgs

        httpClientLogs <- toStrict. Builder.toLazyText . foldMap ((<> "\n") . Builder.fromText) . compact
            <$> readTVarIO logs

        pure $ EndToEndException { httpClientLogs, applicationLogs, originalException }
      where
        compact xs
            = zip (drop 1 xs) xs
            & foldl'
                (\(count, msgs) (nextMsg, currentMsg) ->
                    if nextMsg == currentMsg then
                        (next count, msgs)
                    else if count > 1 then
                        (1, currentMsg <> " (" <> show count <> " times" <> ")" : msgs)
                    else
                        (1, currentMsg : msgs)
                )
                (1 :: Word, mempty)
            & snd


data EndToEndException = EndToEndException
    { httpClientLogs :: Text
    , applicationLogs :: Text
    , originalException :: SomeException
    }

instance Exception EndToEndException

instance Show EndToEndException where
    show EndToEndException{httpClientLogs, applicationLogs, originalException} =
        toString $ T.unlines
            [ toText (displayException originalException)
            , ""
            , "== Application logs"
            , applicationLogs
            , "== Http client's logs"
            , httpClientLogs
            ]

withReplica :: Configuration -> (HttpClient IO -> IO b) -> IO ()
withReplica cfg test = do
    let replicaCfg = cfg
            { chainProducer = ReadOnlyReplica
            , serverPort = serverPort cfg + 1000
            }

    replicaHttpClient <- newHttpClient (serverHost replicaCfg, serverPort replicaCfg)

    replicaEnv <- newEnvironmentWith throwIO replicaCfg

    withSystemTempDirectory "kupo-end-to-end-replica" $ \dir -> do
        withTempFile dir "traces" $ \_fp h -> do
            withTracers h version (defaultTracers (Just Info)) $ \tr -> do
                race_ (kupo tr `runWith` replicaEnv) (test replicaHttpClient)

currentNetworkTip :: IO Point
currentNetworkTip = do
    lookupEnv varCardanoNodeSocket >>= \case
        Nothing ->
            fail $ varCardanoNodeSocket <> " not set but necessary for this test."
        Just socket -> do
            let env = Just [("CARDANO_NODE_SOCKET_PATH", socket)]
            let args =
                    [ "query", "tip"
                    , "--testnet-magic", "1"
                    , "--cardano-mode"
                    ]
            out <- readCreateProcess ((proc "cardano-cli" args) { env }) ""
            case Json.eitherDecode @Json.Value (encodeUtf8 out) of
                Left err ->
                    fail err
                Right json -> do
                    maybe (fail "couldn't decode tip from cardano-cli") pure $ do
                        slotNo <- json ^? key "slot" . _Integer
                        headerHash <- json ^? key "hash" ._String
                        pointFromText (show slotNo <> "." <> headerHash)

shouldThrowTimeout :: forall e. (Exception e) => DiffTime -> (DiffTime -> IO () -> IO ()) -> IO ()
shouldThrowTimeout t action = do
    let stub = action (t + 1) (threadDelay (t + 1))
    timeout (fromInteger @Int (diffTimeToMicroseconds t)) (try stub) >>= \case
        Nothing ->
            fail $ "timed out (unexpectedly) after " <> show t
        Just (Right ()) ->
            fail "should have thrown but didn't."
        Just (Left (e :: EndToEndException)) -> do
            case fromException (originalException e) of
                Nothing ->
                    fail $ "should have thrown '" <> exceptionName
                         <> "' but did throw instead: " <> show (originalException e)
                Just (_ :: e) ->
                    pure ()
  where
    exceptionName = tyConName (typeRepTyCon (typeRep @e))
