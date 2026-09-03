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
import Data.List
    ( maximum
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
    ( Datum (..)
    , ScriptReference (..)
    , checkpointPoint
    , getCheckpointSlotNo
    , getPointSlotNo
    , hasPolicyId
    , mkOutputReference
    , pattern GenesisPoint
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
import Kupo.Data.Http.ReferenceFlag
    ( ReferenceFlag (..)
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
import Kupo.Data.Configuration
    ( Since (..)
    , Until (..)
    )
import Kupo.Data.Health
    ( ConnectionStatus (..)
    , Health (..)
    )
import System.IO
    ( hClose
    , hGetLine
    )

import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as Builder
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
                cp <- maximum . (<> [0]) . fmap getCheckpointSlotNo <$> listCheckpoints
                waitSlot (> (cp + 1_000))
                healthCheck (serverHost cfg) (serverPort cfg)

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
                            , networkParameters = ()
                            }
                    Ogmios{ogmiosPort} ->
                        Ogmios
                            { ogmiosHost = "/dev/null"
                            , ogmiosPort
                            , networkParameters = ()
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
            matches <- getAllMatches NoStatusFlag AsReference
            all (isNothing . spentAt) matches `shouldBe` True

    endToEnd "Retrieve checkpoints and ancestors" $ \(configure, runSpec, HttpClient{..}) -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint somePointAncestor)
            , patterns = fromList [MatchAny OnlyShelley]
            , inputManagement = RemoveSpentInputs
            }
        let slot = getPointSlotNo
        runSpec env 5 $ do
            let
                getPointBySlot mode slotNo = getCheckpointBySlot mode slotNo <&> fmap checkpointPoint
            waitSlot (> (getPointSlotNo somePointSuccessor))
            getPointBySlot GetCheckpointStrict (slot somePoint)
                `shouldReturn` Just somePoint
            getPointBySlot GetCheckpointStrict (prev (slot somePoint))
                `shouldReturn` Nothing
            getPointBySlot GetCheckpointStrict (slot somePointSuccessor)
                `shouldReturn` Just somePointSuccessor
            getPointBySlot GetCheckpointStrict  (prev (slot somePointSuccessor))
                `shouldReturn` Nothing
            getPointBySlot GetCheckpointClosestAncestor (prev (slot somePointSuccessor))
                `shouldReturn` Just somePoint

    endToEnd "Retrieve datums associated with datum hashes" $ \(configure, runSpec, HttpClient{..}) -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint lastAlonzoPoint)
            , patterns = fromList [MatchAny OnlyShelley]
            }

        let extractInline = mapMaybe $ \Result { datum } ->
                case datum of
                  Reference (Right binaryData) -> Just binaryData
                  Inline (Right binaryData) -> Just binaryData
                  _ -> Nothing

        runSpec env 20 $ do
            waitDatum someDatumInWitnessHash
                `shouldReturn` someDatumInWitness
            waitDatum someDatumInOutputHash
                `shouldReturn` someDatumInOutput

            whenRefs <- getAllMatches NoStatusFlag AsReference <&> extractInline
            whenRefs `shouldSatisfy` notElem someDatumInWitness
            whenRefs `shouldSatisfy` notElem someDatumInOutput

            whenInline <- getAllMatches NoStatusFlag InlineAll <&> extractInline
            whenInline `shouldSatisfy` elem someDatumInWitness
            whenInline `shouldSatisfy` elem someDatumInOutput

    endToEnd "Retrieve scripts associated with script hashes" $ \(configure, runSpec, HttpClient{..}) -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint somePointNearScripts)
            , patterns = fromList [MatchAny OnlyShelley]
            }

        let extractInline = mapMaybe $ \Result { scriptReference } ->
                case scriptReference of
                  InlineScript script -> Just script
                  _ -> Nothing

        runSpec env 20 $ do
            waitScript someScriptInWitnessHash
                `shouldReturn` someScriptInWitness
            waitScript someScriptInMetadataHash
                `shouldReturn` someScriptInMetadata
            waitScript someScriptInOutputHash
                `shouldReturn` someScriptInOutput

            whenRefs <- getAllMatches NoStatusFlag AsReference <&> extractInline
            whenRefs `shouldSatisfy` notElem someScriptInOutput

            whenInline <- getAllMatches NoStatusFlag InlineAll <&> extractInline
            whenInline `shouldSatisfy` elem someScriptInOutput

    endToEnd "Failing to insert patterns (failed to resolve point) doesn't disturb normal operations" $ \(configure, runSpec, HttpClient{..})  -> do
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint lastByronPoint)
            , patterns = fromList [MatchAny OnlyShelley]
            }
        runSpec env 10 $ do
            let maxSlot = getPointSlotNo lastByronPoint + 10_000
            waitSlot (>= maxSlot)
            slot <- maximum . fmap getCheckpointSlotNo <$> listCheckpoints
            res <- putPatternSince (MatchDelegation someOtherStakeKey) (Left (maxSlot - 20_000))
            slot' <- maximum . fmap getCheckpointSlotNo <$> listCheckpoints
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
            slot <- maximum . fmap getCheckpointSlotNo <$> listCheckpoints
            res <- putPatternSince
                (MatchDelegation someOtherStakeKey)
                (Right someNonExistingPoint)
            slot' <- maximum . fmap getCheckpointSlotNo <$> listCheckpoints
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
            outRefs <- fmap outputReference <$> getAllMatches NoStatusFlag AsReference
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
            values <- fmap value <$> getAllMatches NoStatusFlag AsReference
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
                results <- getAllMatches NoStatusFlag AsReference
                return (any predicate results)
            let matches = find predicate <$> getAllMatches NoStatusFlag AsReference
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
                mostRecentPoint <- Prelude.head <$> listCheckpoints httpClient
                waitSlot replicaHttpClient (>= (getCheckpointSlotNo mostRecentPoint))
                Health{connectionStatus, configuration} <- getHealth replicaHttpClient
                connectionStatus `shouldBe` Connected
                configuration `shouldBe` Nothing

    endToEnd "Does not synchronize beyond a given point when asked (--until)" $ \(configure, runSpec, HttpClient{..}) -> do
        let maxSlot = 11037873 -- Somewhat after `somePoint`, but close enough. Note that this slot must still exist (i.e. be active)
                               -- if we don't want `waitSlot` down below to be waiting forever!
        (_, env) <- configure $ \defaultCfg -> defaultCfg
            { since = Just (SincePoint somePoint)
            , until = Just (UntilSlot maxSlot)
            , patterns = fromList [MatchAny IncludingBootstrap]
            , deferIndexes = SkipNonEssentialIndexes
            }
        runSpec env 30 $ do
            waitSlot (>= maxSlot)
            points <- listCheckpoints
            forM_ points $ \point -> getCheckpointSlotNo point `shouldSatisfy` (<= maxSlot)
            -- Ensures that even if we let time pass, we're not synchronizing beyond --until
            threadDelay 1
            points' <- listCheckpoints
            forM_ points' $ \point -> getCheckpointSlotNo point `shouldSatisfy` (<= maxSlot)

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
                    { chainProducer = CardanoNode { nodeSocket, nodeConfig, networkParameters = () }
                    , databaseLocation = InMemory Nothing
                    , serverHost = "127.0.0.1"
                    , serverPort = 0
                    , since = Nothing
                    , until = Nothing
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
                    { chainProducer = Ogmios { ogmiosHost, ogmiosPort, networkParameters = () }
                    , databaseLocation = InMemory Nothing
                    , serverHost = "127.0.0.1"
                    , serverPort = 0
                    , since = Nothing
                    , until = Nothing
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
                    , until = Nothing
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
