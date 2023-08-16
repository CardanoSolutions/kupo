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
    , newEnvironment
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
    , TracerDefinition (..)
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
    , DeferIndexesInstallation (..)
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
    , shouldReturn
    , shouldSatisfy
    , specify
    , xcontext
    , xspecify
    )
import Test.Kupo.App.Http.Client
    ( HttpClient (..)
    , newHttpClientWith
    )
import Test.Kupo.Fixture
    ( eraBoundaries
    , lastAllegraPoint
    , lastAlonzoPoint
    , lastByronPoint
    , lastMaryPoint
    , someDatumHashInOutput
    , someDatumHashInWitness
    , someDatumInOutput
    , someDatumInWitness
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
    , someScriptHashInMetadata
    , someScriptHashInOutput
    , someScriptHashInWitness
    , someScriptInMetadata
    , someScriptInOutput
    , someScriptInWitness
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
import qualified Data.Aeson as Json
import qualified Data.Text as T
import Data.Text.Lazy.Builder
    ( Builder
    )
import qualified Data.Text.Lazy.Builder as Builder
import qualified Prelude
import System.IO
    ( hClose
    , hGetLine
    , isEOF
    )
import System.IO.Error
    ( IOError
    )

varCardanoNodeSocket :: String
varCardanoNodeSocket = "CARDANO_NODE_SOCKET"

varCardanoNodeConfig :: String
varCardanoNodeConfig = "CARDANO_NODE_CONFIG"

varOgmiosHost :: String
varOgmiosHost = "OGMIOS_HOST"

varOgmiosPort :: String
varOgmiosPort = "OGMIOS_PORT"

type EndToEndContext
    = ( (Configuration -> Configuration) -> IO (Configuration, Env Kupo)
      , Env Kupo -> DiffTime -> IO () -> IO ()
      , HttpClient IO
      )

endToEnd :: HasCallStack => String -> (EndToEndContext -> IO ()) -> SpecWith EndToEndContext
endToEnd = specify

spec :: Spec
spec = skippableContext "End-to-end" $ do
    endToEnd "in-memory" $ \(configure, runSpec, HttpClient{..}) -> do
        (cfg, env) <- configure $ \defaultCfg -> defaultCfg
            { workDir = InMemory
            , since = Just lastByronPoint
            , patterns = fromList [MatchAny IncludingBootstrap]
            , deferIndexes = SkipNonEssentialIndexes
            }
        runSpec env 10 $ do
            waitSlot (> 1000)
            healthCheck (serverHost cfg) (serverPort cfg)

    forM_ eraBoundaries $ \(era, point) ->
        endToEnd ("quick sync through " <> era) $ \(configure, runSpec, HttpClient{..}) -> do
            (cfg, env) <- configure $ \defaultCfg -> defaultCfg
                { since = Just point
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
                { since = Just somePoint
                , patterns = fromList [MatchAny OnlyShelley]
                }
            runSpec env 5 $ waitSlot (> (getPointSlotNo somePoint))

        do -- Can restart the server
            (_, env) <- configure $ \defaultCfg -> defaultCfg
                { since = Just somePoint
                , patterns = fromList [MatchAny OnlyShelley]
                }
            runSpec env 5 $ do
                cps <- fmap getPointSlotNo <$> listCheckpoints
                maximum cps `shouldSatisfy` (> (getPointSlotNo somePoint))

        do -- Can't restart with different, too recent, --since target on same db
            (_, env) <- configure $ \defaultCfg -> defaultCfg
                { since = Just someOtherPoint
                , patterns = fromList [MatchAny OnlyShelley]
                }
            shouldThrowTimeout @ConflictingOptionsException 1 (runSpec env)

        do -- Can't restart with different, non-empty, patterns
            (_, env) <- configure $ \defaultCfg -> defaultCfg
                { since = Just somePoint
                , patterns = fromList [MatchAny IncludingBootstrap]
                }
            shouldThrowTimeout @ConflictingOptionsException 1 (runSpec env)

--    xspecify "Can't start the server on a fresh new db without explicit point" $ \(tmp, tr, cfg, _, _) -> do
--        env <- newEnvironment $ cfg
--            { workDir = Dir tmp
--            , since = Nothing
--            , patterns = fromList [MatchAny OnlyShelley]
--            }
--        shouldThrowTimeout @NoStartingPointException 1 (kupo tr `runWith` env)
--
--    xspecify "Retry and wait when chain producer isn't available" $ \(tmp, tr, cfg, HttpClient{..}, timeoutOrThrow) -> do
--        env <- newEnvironment $ cfg
--            { workDir = Dir tmp
--            , since = Just GenesisPoint
--            , patterns = fromList [MatchAny OnlyShelley]
--            , chainProducer =
--                case chainProducer cfg of
--                    CardanoNode{nodeConfig} ->
--                        CardanoNode
--                            { nodeSocket = "/dev/null"
--                            , nodeConfig
--                            }
--                    Ogmios{ogmiosPort} ->
--                        Ogmios
--                            { ogmiosHost = "/dev/null"
--                            , ogmiosPort
--                            }
--            }
--        timeoutOrThrow 5 $ race_
--            (kupo tr `runWith` env)
--            (do
--                cps <- listCheckpoints
--                threadDelay 1
--                cps' <- listCheckpoints
--                cps `shouldBe` cps'
--            )
--
--    xspecify "Crashes when no intersection is found" $ \(tmp, tr, cfg, _, _) -> do
--        env <- newEnvironment $ cfg
--            { workDir = Dir tmp
--            , since = Just someNonExistingPoint
--            , patterns = fromList [MatchAny OnlyShelley]
--            }
--        shouldThrowTimeout @IntersectionNotFoundException 1 (kupo tr `runWith` env)
--
--    xspecify "Crashes when no patterns are defined" $ \(tmp, tr, cfg, _, _) -> do
--        env <- newEnvironment $ cfg
--            { workDir = Dir tmp
--            , since = Just somePoint
--            , patterns = fromList []
--            }
--        shouldThrowTimeout @ConflictingOptionsException 1 (kupo tr `runWith` env)
--
--    xspecify "Can prune utxo on-the-fly" $ \(tmp, tr, cfg, HttpClient{..}, timeoutOrThrow) -> do
--        env <- newEnvironment $ cfg
--            { workDir = Dir tmp
--            , since = Just somePoint
--            , patterns = fromList [MatchAny OnlyShelley]
--            , inputManagement = RemoveSpentInputs
--            }
--
--        timeoutOrThrow 30 $ race_
--            (kupo tr `runWith` env)
--            (do
--                waitSlot (> 50_000)
--                matches <- getAllMatches NoStatusFlag
--                all (isNothing . spentAt) matches `shouldBe` True
--            )
--
--    xspecify "Retrieve checkpoints and ancestors" $ \(tmp, tr, cfg, HttpClient{..}, timeoutOrThrow) -> do
--        env <- newEnvironment $ cfg
--            { workDir = Dir tmp
--            , since = Just somePointAncestor
--            , patterns = fromList [MatchAny OnlyShelley]
--            , inputManagement = RemoveSpentInputs
--            }
--        let strict = GetCheckpointStrict
--        let flex = GetCheckpointClosestAncestor
--        timeoutOrThrow 5 $ race_
--            (kupo tr `runWith` env)
--            (do
--                waitSlot (> (getPointSlotNo somePointSuccessor))
--                getCheckpointBySlot strict (getPointSlotNo somePoint)
--                    `shouldReturn` Just somePoint
--                getCheckpointBySlot strict (prev (getPointSlotNo somePoint))
--                    `shouldReturn` Nothing
--                getCheckpointBySlot strict (getPointSlotNo somePointSuccessor)
--                    `shouldReturn` Just somePointSuccessor
--                getCheckpointBySlot strict  (prev (getPointSlotNo somePointSuccessor))
--                    `shouldReturn` Nothing
--                getCheckpointBySlot flex (prev (getPointSlotNo somePointSuccessor))
--                    `shouldReturn` Just somePoint
--            )
--
--    xspecify "Retrieve datums associated with datum hashes" $ \(tmp, tr, cfg, HttpClient{..}, timeoutOrThrow) -> do
--        env <- newEnvironment $ cfg
--            { workDir = Dir tmp
--            , since = Just lastAlonzoPoint
--            , patterns = fromList [MatchAny OnlyShelley]
--            }
--        timeoutOrThrow 20 $ race_
--            (kupo tr `runWith` env)
--            (do
--                waitDatum someDatumHashInWitness
--                    `shouldReturn` someDatumInWitness
--                waitDatum someDatumHashInOutput
--                    `shouldReturn` someDatumInOutput
--            )
--
--    xspecify "Retrieve scripts associated with script hashes" $ \(tmp, tr, cfg, HttpClient{..}, timeoutOrThrow) -> do
--        env <- newEnvironment $ cfg
--            { workDir = Dir tmp
--            , since = Just somePointNearScripts
--            , patterns = fromList [MatchAny OnlyShelley]
--            }
--        timeoutOrThrow 20 $ race_
--            (kupo tr `runWith` env)
--            (do
--                waitScript someScriptHashInWitness
--                    `shouldReturn` someScriptInWitness
--                waitScript someScriptHashInMetadata
--                    `shouldReturn` someScriptInMetadata
--                waitScript someScriptHashInOutput
--                    `shouldReturn` someScriptInOutput
--            )
--
--    xspecify "Dynamically add pattern and restart to a past point when syncing" $ \(tmp, tr, cfg, HttpClient{..}, timeoutOrThrow) -> do
--        env <- newEnvironment $ cfg
--            { workDir = Dir tmp
--            , since = Just lastByronPoint
--            , patterns = fromList [MatchDelegation someStakeKey]
--            }
--        let maxSlot = getPointSlotNo lastByronPoint + 100_000
--        let onlyInWindow r
--                | getPointSlotNo (createdAt r) <= maxSlot =
--                    Just r { spentAt = Nothing }
--                | otherwise =
--                    Nothing
--        ref <- newIORef ([], [])
--        timeoutOrThrow 10 $ race_
--            (kupo tr `runWith` env)
--            (do
--                waitSlot (>= maxSlot)
--                xs <- mapMaybe onlyInWindow <$> getAllMatches NoStatusFlag
--                res <- putPatternSince (MatchDelegation someOtherStakeKey) (Right lastByronPoint)
--                res `shouldBe` True
--                waitSlot (< maxSlot) -- Observe rollback
--                waitSlot (>= maxSlot)
--                ys <- mapMaybe onlyInWindow <$> getAllMatches NoStatusFlag
--                (xs \\ ys) `shouldBe` []
--                (sort <$> listPatterns) `shouldReturn`
--                    [ MatchDelegation someStakeKey
--                    , MatchDelegation someOtherStakeKey
--                    ]
--                atomicSwapIORef ref (xs, ys)
--            )
--        (xs, ys) <- readIORef ref
--        withSystemTempDirectory "kupo-end-to-end" $ \tmp' -> do
--            env' <- newEnvironment $ cfg
--                { workDir = Dir tmp'
--                , since = Just lastByronPoint
--                , patterns = fromList [MatchDelegation someOtherStakeKey]
--                }
--            timeoutOrThrow 10 $ race_
--                (kupo tr `runWith` env')
--                (do
--                    waitSlot (>= maxSlot)
--                    zs <- mapMaybe onlyInWindow <$> getAllMatches NoStatusFlag
--                    ys \\ zs `shouldBe` xs
--                )
--
--    xspecify "Dynamically add pattern and restart to a past point when at the tip" $ \(tmp, tr, cfg, HttpClient{..}, timeoutOrThrow) -> do
--        tip <- currentNetworkTip
--        env <- newEnvironment $ cfg
--            { workDir = Dir tmp
--            , since = Just tip
--            , patterns = fromList [MatchAny IncludingBootstrap]
--            }
--        timeoutOrThrow 180 $ race_
--            (kupo tr `runWith` env)
--            (do
--                waitSlot (>= getPointSlotNo tip)
--                res <- putPatternSince (MatchDelegation someOtherStakeKey) (Right tip)
--                res `shouldBe` True
--            )
--
--    xspecify "Failing to insert patterns (failed to resolve point) doesn't disturb normal operations" $ \(tmp, tr, cfg, HttpClient{..}, timeoutOrThrow) -> do
--        env <- newEnvironment $ cfg
--            { workDir = Dir tmp
--            , since = Just lastByronPoint
--            , patterns = fromList [MatchAny OnlyShelley]
--            }
--        timeoutOrThrow 10 $ race_
--            (kupo tr `runWith` env)
--            (do
--                let maxSlot = getPointSlotNo lastByronPoint + 10_000
--                waitSlot (>= maxSlot)
--                slot <- maximum . fmap getPointSlotNo <$> listCheckpoints
--                res <- putPatternSince (MatchDelegation someOtherStakeKey) (Left (maxSlot - 20_000))
--                slot' <- maximum . fmap getPointSlotNo <$> listCheckpoints
--                res `shouldBe` False
--                listPatterns `shouldReturn` [MatchAny OnlyShelley]
--                slot' `shouldSatisfy` (>= slot)
--            )
--
--    xspecify "Failing to insert patterns (non-existing point) doesn't disturb normal operations" $ \(tmp, tr, cfg, HttpClient{..}, timeoutOrThrow) -> do
--        env <- newEnvironment $ cfg
--            { workDir = Dir tmp
--            , since = Just lastByronPoint
--            , patterns = fromList [MatchAny OnlyShelley]
--            }
--        timeoutOrThrow 5 $ race_
--            (kupo tr `runWith` env)
--            (do
--                let maxSlot = getPointSlotNo lastByronPoint + 10_000
--                waitSlot (>= maxSlot)
--                slot <- maximum . fmap getPointSlotNo <$> listCheckpoints
--                res <- putPatternSince
--                    (MatchDelegation someOtherStakeKey)
--                    (Right someNonExistingPoint)
--                slot' <- maximum . fmap getPointSlotNo <$> listCheckpoints
--                res `shouldBe` False
--                listPatterns `shouldReturn` [MatchAny OnlyShelley]
--                slot' `shouldSatisfy` (>= slot)
--            )
--
--    xspecify "Match by transaction id / output reference" $ \(tmp, tr, cfg, HttpClient{..}, timeoutOrThrow) -> do
--        env <- newEnvironment $ cfg
--            { workDir = Dir tmp
--            , since = Just lastMaryPoint
--            , patterns = fromList
--                [ MatchTransactionId someTransactionId
--                , MatchOutputReference (mkOutputReference someThirdTransactionId 0)
--                ]
--            }
--        timeoutOrThrow 10 timeoutOrThrow $ race_
--            (kupo tr `runWith` env)
--            (do
--                waitUntilM $ do
--                    outRefs <- fmap outputReference <$> getAllMatches NoStatusFlag
--                    return $
--                        (mkOutputReference someTransactionId 0, 0) `elem` outRefs
--                        &&
--                        (mkOutputReference someTransactionId 1, 0) `elem` outRefs
--                        &&
--                        (mkOutputReference someThirdTransactionId 0, 2) `elem` outRefs
--            )
--
--
--    xspecify "Match by policy id" $ \(tmp, tr, cfg, HttpClient{..}, timeoutOrThrow) -> do
--        env <- newEnvironment $ cfg
--            { workDir = Dir tmp
--            , since = Just lastMaryPoint
--            , patterns = fromList [MatchPolicyId somePolicyId]
--            }
--        timeoutOrThrow 10 timeoutOrThrow $ race_
--            (kupo tr `runWith` env)
--            (do
--                waitUntilM $ do
--                    values <- fmap value <$> getAllMatches NoStatusFlag
--                    return $ all (`hasPolicyId` somePolicyId) values
--            )
--
--    xspecify "Fetch metadata by slot" $ \(tmp, tr, cfg, HttpClient{..}, timeoutOrThrow) -> do
--        env <- newEnvironment $ cfg
--            { workDir = Dir tmp
--            , since = Just lastAllegraPoint
--            , patterns = fromList [MatchAny OnlyShelley]
--            }
--        timeoutOrThrow 20 timeoutOrThrow $ race_
--            (kupo tr `runWith` env)
--            (do
--                waitSlot (> someSlotWithMetadata)
--                xs <- lookupMetadataBySlotNo someSlotWithMetadata Nothing
--                [ hash | (hash, _meta) <- xs ] `shouldBe` someMetadata
--
--                xs' <- lookupMetadataBySlotNo someSlotWithMetadata (Just someTransactionIdWithMetadata)
--                [ hash | (hash, _meta) <- xs' ] `shouldBe` take 1 someMetadata
--            )
--
--    xspecify "Index collateral return from failed transactions" $ \(tmp, tr, cfg, HttpClient{..}, timeoutOrThrow) -> do
--        env <- newEnvironment $ cfg
--            { workDir = Dir tmp
--            , since = Just somePointNearPhase2Failure
--            , patterns = fromList [MatchTransactionId somePhase2FailedTransactionIdWithReturn]
--            }
--        timeoutOrThrow 5 timeoutOrThrow $ race_
--            (kupo tr `runWith` env)
--            (do
--                let predicate = (== (mkOutputReference somePhase2FailedTransactionIdWithReturn 1)) . fst . outputReference
--                waitUntilM $ do
--                    results <- getAllMatches NoStatusFlag
--                    return (any predicate results)
--                (find predicate <$> getAllMatches NoStatusFlag) >>= \case
--                    Nothing -> fail "impossible: the result disappeared?"
--                    Just r  -> value r `shouldBe` unsafeValueFromList 7000000 []
--            )


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
                    , workDir = InMemory
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
                    , workDir = InMemory
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
                    let cfg = mkConfig (defaultCfg { serverPort, workDir = Dir dir })
                    (cfg,) <$> newEnvironmentWith throwIO cfg
                , \env t test -> do
                        withTempFile dir "traces" $ \fp h -> do
                            withTracers h version (defaultTracers (Just Info)) $ \tr -> do
                                let runner = do
                                        res <- timeout (fromInteger @Int (diffTimeToMicroseconds t)) (race_ (kupo tr `runWith` env) test)
                                        res `shouldSatisfy` isJust
                                runner `catch` \(e :: SomeException) -> do
                                    throwIO =<< collectLogs e (fp, h) httpClientLogsVar
                , httpClient
                )

    collectLogs :: SomeException -> (FilePath, Handle) -> TVar IO [Text] -> IO EndToEndException
    collectLogs originalException (fp, h) logs = do
        hClose h
        applicationLogs <- withFile fp ReadMode $ \h' ->
            let hPrintLines = do
                    unlessM (lift (hIsEOF h')) $ do
                        lift (hGetLine h') >>= tell . (<> "\n") . Builder.fromString
                        hPrintLines
             in toStrict . Builder.toLazyText <$> execWriterT hPrintLines

        httpClientLogs <- toStrict. Builder.toLazyText . foldMap ((<> "\n") . Builder.fromText) . reverse
            <$> atomically (readTVar logs)

        pure $ EndToEndException { httpClientLogs, applicationLogs, originalException }

data EndToEndException = EndToEndException
    { httpClientLogs :: Text
    , applicationLogs :: Text
    , originalException :: SomeException
    }

instance Exception EndToEndException

instance Show EndToEndException where
    show EndToEndException{httpClientLogs, applicationLogs} =
        toString $ T.unlines
            [ "== Application logs"
            , applicationLogs
            , "== Http client's logs"
            , httpClientLogs
            ]

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
