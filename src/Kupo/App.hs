--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Kupo.App
    ( -- * Producer(s)
      ChainSyncClient
    , newProducer
    , withFetchBlockClient
    , newFetchTipClient

      -- * Consumer
    , consumer
    , readOnlyConsumer

      -- * Gardener
    , gardener

      -- * Utils
    , idle
    , mkNotifyTip

      -- * Tracers
    , TraceConsumer (..)
    , TraceGardener (..)
    , TraceKupo (..)
    ) where

import Kupo.Prelude

import Kupo.App.ChainSync
    ( TraceChainSync (..)
    , withChainSyncExceptionHandler
    )
import Kupo.App.Configuration
    ( TraceConfiguration (..)
    , parseNetworkParameters
    )
import Kupo.App.Database.Types
    ( DBTransaction
    , Database (..)
    )
import Kupo.App.Health
    ( recordCheckpoint
    )
import Kupo.App.Mailbox
    ( Mailbox
    , flushMailbox
    , newMailbox
    )
import Kupo.Control.MonadAsync
    ( MonadAsync (..)
    , race_
    )
import Kupo.Control.MonadCatch
    ( MonadCatch (..)
    )
import Kupo.Control.MonadDelay
    ( MonadDelay
    , threadDelay
    )
import Kupo.Control.MonadLog
    ( HasSeverityAnnotation (..)
    , MonadLog (..)
    , Severity (..)
    , Tracer
    , nullTracer
    )
import Kupo.Control.MonadOuroboros
    ( MonadOuroboros (..)
    , NodeToClientVersion (..)
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Control.MonadThrow
    ( MonadThrow (..)
    )
import Kupo.Data.Cardano
    ( IsBlock
    , Point
    , SlotNo (..)
    , Tip
    , distanceToTip
    , getPoint
    , getPointSlotNo
    , getTipSlotNo
    , pattern GenesisPoint
    )
import Kupo.Data.ChainSync
    ( ForcedRollbackHandler (..)
    , IntersectionNotFoundException (..)
    )
import Kupo.Data.Configuration
    ( ChainProducer (..)
    , Configuration (..)
    , DeferIndexesInstallation (..)
    , InputManagement (..)
    , LongestRollback (..)
    , NetworkParameters (..)
    , NodeTipHasBeenReachedException (..)
    , UnableToFetchBlockFromReadOnlyReplicaException (..)
    , mailboxCapacity
    , pruneInputsMaxIncrement
    )
import Kupo.Data.Database
    ( binaryDataToRow
    , policyToRow
    , resultToRow
    , scriptToRow
    )
import Kupo.Data.FetchBlock
    ( FetchBlockClient
    )
import Kupo.Data.FetchTip
    ( FetchTipClient
    , UnableToFetchTipFromHydraException (..)
    , UnableToFetchTipFromReadOnlyReplicaException (..)
    )
import Kupo.Data.Health
    ( Health
    )
import Kupo.Data.PartialBlock
    ( PartialBlock
    )
import Kupo.Data.Pattern
    ( Codecs (..)
    , Match (..)
    , MatchBootstrap (..)
    , Pattern (..)
    , included
    , matchBlock
    )

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Kupo.App.ChainSync.Hydra as Hydra
import qualified Kupo.App.ChainSync.Node as Node
import qualified Kupo.App.ChainSync.Ogmios as Ogmios
import qualified Kupo.App.FetchBlock.Node as Node
import qualified Kupo.App.FetchBlock.Ogmios as Ogmios
import qualified Kupo.App.FetchTip.Node as Node
import qualified Kupo.App.FetchTip.Ogmios as Ogmios

--
-- Producer
--

type ChainSyncClient m block =
       Tracer IO TraceChainSync
    -> [Point] -- Checkpoints
    -> ConnectionStatusToggle m
    -> m ()

newProducer
    :: Tracer IO TraceConfiguration
    -> ChainProducer
    -> ( forall block. IsBlock block
        => (Point -> ForcedRollbackHandler IO -> IO ())
        -> Mailbox IO (Tip, block) (Tip, Point)
        -> ChainSyncClient IO block
        -> IO ()
       )
    -> IO ()
newProducer tr chainProducer callback = do
    forcedRollbackVar <- newEmptyTMVarIO
    let forcedRollbackCallback point handler =
            atomically (putTMVar forcedRollbackVar (point, handler))

    case chainProducer of
        ReadOnlyReplica -> do
            mailbox <- atomically (newMailbox mailboxCapacity)
            callback @Void forcedRollbackCallback mailbox $ \_ _ _ -> pure ()

        Ogmios{ogmiosHost, ogmiosPort} -> do
            logWith tr ConfigurationOgmios{ogmiosHost, ogmiosPort}
            mailbox <- atomically (newMailbox mailboxCapacity)
            callback forcedRollbackCallback mailbox $ \_tracerChainSync checkpoints statusToggle -> do
                let runOgmios pts beforeMainLoop onIntersectionNotFound continuation = do
                        res <- race
                            (Ogmios.connect statusToggle ogmiosHost ogmiosPort $
                                Ogmios.runChainSyncClient mailbox beforeMainLoop pts
                            )
                            (atomically (takeTMVar forcedRollbackVar))
                        case res of
                            Left notFound ->
                                onIntersectionNotFound notFound
                            Right (point, handler) ->
                                continuation point handler

                let restart point handler =
                        runOgmios [point]
                            (onSuccess handler)
                            (\e -> onFailure handler >> throwIO e)
                            restart

                runOgmios checkpoints (return ()) throwIO restart

        Hydra{hydraHost, hydraPort} -> do
            logWith tr ConfigurationHydra{hydraHost, hydraPort}
            mailbox <- atomically (newMailbox mailboxCapacity)

            callback forcedRollbackCallback mailbox $ \_tracerChainSync checkpoints statusToggle -> do
                let runHydra pts beforeMainLoop onIntersectionNotFound continuation = do
                        res <- race
                            (Hydra.connect statusToggle hydraHost hydraPort $
                                -- NOTE: Kupo does not generally index the genesis 'block' / configuration.
                                -- That's true for the normal case with ogmios or cardano-node; but for
                                -- Hydra, we might want to.
                                --
                                -- This is debatable and should be backed by use-cases, and may also be
                                -- confusing since normally Kupo starts indexing only the blocks that
                                -- follows the given checkpoints. But now we make an exception for genesis.
                                Hydra.runChainSyncClient mailbox beforeMainLoop (filter (/= GenesisPoint) pts)
                            )
                            (atomically (takeTMVar forcedRollbackVar))
                        case res of
                            Left notFound ->
                                onIntersectionNotFound notFound
                            Right (point, handler) ->
                                continuation point handler

                let restart point handler =
                        runHydra [point]
                            (onSuccess handler)
                            (\e -> onFailure handler >> throwIO e)
                            restart

                runHydra checkpoints (return ()) throwIO restart

        CardanoNode{nodeSocket, nodeConfig} -> do
            logWith tr ConfigurationCardanoNode{nodeSocket,nodeConfig}
            mailbox <- atomically (newMailbox mailboxCapacity)
            network@NetworkParameters
                { networkMagic
                , slotsPerEpoch
                } <- liftIO (parseNetworkParameters nodeConfig)
            logWith tr (ConfigurationNetwork network)
            callback forcedRollbackCallback mailbox $ \tracerChainSync checkpoints statusToggle -> do
                withChainSyncServer
                  statusToggle
                  [ NodeToClientV_9 .. maxBound ]
                  networkMagic
                  slotsPerEpoch
                  nodeSocket
                  (Node.mkChainSyncClient forcedRollbackVar mailbox checkpoints)
                  & handle
                    (\case
                        e@IntersectionNotFound{requestedPoints = points} -> do
                            logWith tracerChainSync ChainSyncIntersectionNotFound{points}
                            throwIO e
                        e@ForcedIntersectionNotFound{point} -> do
                            logWith tracerChainSync $ ChainSyncIntersectionNotFound [point]
                            throwIO e
                    )

-- | A background client that answers on-demand requests to fetch specific block. It evolved on a
-- completely different connection than the main chain producer to not conflict with one another.
--
--
-- The FetchBlockClient is more geared towards fetching precise information (e.g. metadata of a
-- transaction in a known block).
withFetchBlockClient
    :: ChainProducer
    -> ( forall block. IsBlock block
        => FetchBlockClient IO block
        -> IO ()
       )
    -> IO ()
withFetchBlockClient chainProducer callback = do
    case chainProducer of
        ReadOnlyReplica{} ->
            callback @Void (\_point _respond -> throwIO UnableToFetchBlockFromReadOnlyReplica)
        Ogmios{ogmiosHost, ogmiosPort} ->
            Ogmios.withFetchBlockClient ogmiosHost ogmiosPort callback
        Hydra{} ->
            callback @PartialBlock (\_point respond -> respond Nothing)
        CardanoNode{nodeSocket, nodeConfig} -> do
            NetworkParameters
                { networkMagic
                , slotsPerEpoch
                } <- liftIO (parseNetworkParameters nodeConfig)
            (chainSyncClient, fetchBlockClient) <- Node.newFetchBlockClient
            race_
                (callback fetchBlockClient)
                (withChainSyncExceptionHandler nullTracer noConnectionStatusToggle $
                    withChainSyncServer
                        noConnectionStatusToggle
                        [ NodeToClientV_9 .. maxBound ]
                        networkMagic
                        slotsPerEpoch
                        nodeSocket
                        chainSyncClient
                )

newFetchTipClient
    :: ChainProducer
    -> FetchTipClient IO
newFetchTipClient = \case
    ReadOnlyReplica ->
        throwIO UnableToFetchTipFromReadOnlyReplica
    Hydra{} ->
        throwIO UnableToFetchTipFromHydra
    Ogmios{ogmiosHost, ogmiosPort} ->
        Ogmios.newFetchTipClient ogmiosHost ogmiosPort
    CardanoNode{nodeSocket, nodeConfig} -> do
        NetworkParameters
            { networkMagic
            , slotsPerEpoch
            } <- liftIO (parseNetworkParameters nodeConfig)
        response <- newEmptyTMVarIO
        withChainSyncServer
            noConnectionStatusToggle
            [ NodeToClientV_9 .. maxBound ]
            networkMagic
            slotsPerEpoch
            nodeSocket
            (Node.newFetchTipClient response)
        atomically $ takeTMVar response

-- | Consumer process that is reading messages from the 'Mailbox'. Messages are
-- enqueued by another process (the producer).
consumer
    :: forall m block.
        ( MonadSTM m
        , MonadLog m
        , Monad (DBTransaction m)
        , IsBlock block
        )
    => Tracer IO TraceConsumer
    -> InputManagement
    -> (Tip -> Maybe Point -> DBTransaction m ())
    -> Mailbox m (Tip, block) (Tip, Point)
    -> TVar m (Set Pattern)
    -> Database m
    -> m Void
consumer tr inputManagement notifyTip mailbox patternsVar Database{..} =
    forever $ do
        logWith tr ConsumerWaitingForNextBatch
        atomically ((,) <$> flushMailbox mailbox <*> readTVar patternsVar) >>= \case
            (Left blks, patterns) ->
                rollForward blks patterns
            (Right pt, _) ->
                rollBackward pt
  where
    rollForward :: (NonEmpty (Tip, block) -> Set Pattern -> m ())
    rollForward blks patterns = do
        let (lastKnownTip, lastKnownBlk) = last blks
        let lastKnownPoint = getPoint lastKnownBlk
        let lastKnownSlot = getPointSlotNo lastKnownPoint
        let Match{consumed, produced, datums, scripts, policies} =
                foldMap (matchBlock codecs patterns . snd) blks
        isNonEmptyBlock <- runTransaction $ do
            insertCheckpoints (foldr ((:) . getPoint . snd) [] blks)
            insertInputs produced
            insertPolicies policies
            nSpentInputs <- onSpentInputs lastKnownTip lastKnownSlot consumed
            -- NOTE: In case where the user has entered a relatively restrictive
            -- pattern (e.g. one specific address), we do a best-effort at not
            -- storing all the garbage of the world and only store scripts and
            -- binary_data from the block if there's at least one transaction
            -- that is relevant to that configuration.
            -- Note that this isn't done from within 'matchBlock' because we
            -- only know if we've spent inputs after running the above database
            -- operation.
            let isNonEmptyBlock = nSpentInputs > 0 || not (null produced)
            when isNonEmptyBlock $ do
                insertBinaryData datums
                insertScripts scripts
            notifyTip lastKnownTip (Just lastKnownPoint)
            return isNonEmptyBlock
        logWith tr $ ConsumerRollForward
            { slotNo = lastKnownSlot
            , inputs = length produced
            , binaryData = if isNonEmptyBlock then length datums else 0
            , scripts = if isNonEmptyBlock then length scripts else 0
            }

    rollBackward :: (Tip, Point) -> m ()
    rollBackward (tip, pt) = do
        logWith tr (ConsumerRollBackward { point = getPointSlotNo pt })
        runTransaction $ do
            lastKnownSlot <- rollbackTo (getPointSlotNo pt)
            notifyTip tip lastKnownSlot

    codecs = Codecs
        { toResult = resultToRow
        , toBinaryData = binaryDataToRow
        , toScript = scriptToRow
        , toPolicy = policyToRow
        }

    onSpentInputs
        :: Tip
        -> SlotNo
        -> Map SlotNo (Set Pattern)
        -> DBTransaction m Int
    onSpentInputs = case inputManagement of
        MarkSpentInputs ->
            \_ _ -> fmap sum . Map.traverseWithKey markInputs
        RemoveSpentInputs ->
            \lastKnownTip lastKnownSlot ->
                -- Only delete when safe (i.e. deep enough in the chain).
                -- Otherwise, mark as 'spent' and leave the pruning to the
                -- periodic 'gardener' / garbage-collector.
                if distanceToTip lastKnownTip lastKnownSlot > unstableWindow then
                    fmap sum . traverse deleteInputs
                else
                    fmap sum . Map.traverseWithKey markInputs
      where
        unstableWindow =
            getLongestRollback longestRollback

readOnlyConsumer
    :: forall m.
        ( MonadSTM m
        , MonadDelay m
        )
    => TVar m Health
    -> Database m
    -> m Void
readOnlyConsumer health Database{..} = do
    forever $ do
        mostRecentCheckpoint <- runTransaction listCheckpointsDesc <&> \case
            []  -> Nothing
            h:_ -> Just h
        recordCheckpoint health (maybe 0 getPointSlotNo mostRecentCheckpoint) mostRecentCheckpoint
        threadDelay 5

-- | Periodically garbage collect the database from entries that aren't of
-- interest. This is mainly the case for:
--
-- - spent inputs that are _definitely spent_
-- - binary data stored in the database that isn't associated with any known input
--
-- Indeed, when kupo is set in 'RemoveSpentInputs' mode, it trims the database
-- as inputs get spent. However, Cardano being a decentralized and only
-- eventually immutable data-source. Indeed, the most recent part of the chain
-- (i.e. last k blocks) _may change unpredictably. By removing 'spent inputs'
-- too early, we may take the risk of removing an input which may be reinstatated
-- later (because the chain forked and the transaction spending that input isn't
-- included in that fork).
--
-- In brief, we can only delete inputs after `k` blocks (or `3*k/f` slots) have
-- passed (since it is guaranteed to have `k` blocks in a `3*k/f` slot window).
--
-- TODO: We should / could also try to prune scripts data. This is a little
-- trickier than for datums because scripts may also be referenced in addresses.
-- To cleanup, we would therefore need to remove scripts that are not referenced
-- in output, nor addresses.
gardener
    :: forall m.
        ( MonadSTM m
        , MonadLog m
        , MonadDelay m
        )
    => Tracer IO TraceGardener
    -> Configuration
    -> TVar m (Set Pattern)
    -> (forall a. (Database m -> m a) -> m a)
    -> m Void
gardener tr config patterns withDatabase = forever $ do
    threadDelay (garbageCollectionInterval config)
    xs <- readTVarIO patterns

    case inputManagement config of
        RemoveSpentInputs -> do
            logWith tr GardenerBeginGarbageCollection { pruning = "inputs" }
            totalPrunedRows <- incrementally $ \Database{..} -> runTransaction pruneInputs
            logWith tr $ GardenerExitGarbageCollection { totalPrunedRows }
        MarkSpentInputs ->
            return ()

    case (inputManagement config, MatchAny OnlyShelley `included` xs) of
        (MarkSpentInputs, s) | not (Set.null s) ->
            return ()
        _needPruning -> do
            logWith tr GardenerBeginGarbageCollection { pruning = "binary_data" }
            totalPrunedRows <- incrementally $ \Database{..} -> runTransaction pruneBinaryData
            logWith tr $ GardenerExitGarbageCollection { totalPrunedRows }

    withDatabase $ \Database{..} -> runTransaction optimize
  where
    incrementally action = do
        prunedRows <- withDatabase action
        prunedRows' <-
            if prunedRows < pruneInputsMaxIncrement
            then return 0
            else do
                logWith tr GardenerPrunedIncrement { prunedRows }
                incrementally action
        return (prunedRows + prunedRows')

--
-- Utils
--

idle :: (MonadDelay m) => m Void
idle = forever (threadDelay 86400)

mkNotifyTip
    :: (MonadThrow m, MonadSTM m)
    => DeferIndexesInstallation
    -> TVar m Health
    -> Tip
    -> Maybe Point
    -> m ()
mkNotifyTip indexMode health tip point = do
    case indexMode of
        InstallIndexesIfNotExist -> pure ()
        SkipNonEssentialIndexes  -> do
            let distance = maybe maxBound (distanceToTip tip . getPointSlotNo) point
            when (distance <= 60) $ throwIO NodeTipHasBeenReached{ distance }
    recordCheckpoint health (getTipSlotNo tip) point

--
-- Tracer
--

data TraceConsumer where
    ConsumerWaitingForNextBatch
        :: TraceConsumer
    ConsumerRollBackward
        :: { point :: SlotNo }
        -> TraceConsumer
    ConsumerRollForward
        :: { slotNo :: SlotNo, inputs :: Int, binaryData :: Int, scripts :: Int }
        -> TraceConsumer
    ConsumerChainSync
        :: { chainSync :: TraceChainSync }
        -> TraceConsumer
    deriving stock (Generic, Show)

instance ToJSON TraceConsumer where
    toEncoding =
        defaultGenericToEncoding

instance HasSeverityAnnotation TraceConsumer where
    getSeverityAnnotation = \case
        ConsumerWaitingForNextBatch{} ->
            Debug
        ConsumerRollForward{} ->
            Info
        ConsumerRollBackward{} ->
            Notice
        ConsumerChainSync{chainSync} ->
            getSeverityAnnotation chainSync

data TraceGardener where
    GardenerBeginGarbageCollection
        :: { pruning :: Text }
        -> TraceGardener
    GardenerPrunedIncrement
        :: { prunedRows :: Int }
        -> TraceGardener
    GardenerExitGarbageCollection
        :: { totalPrunedRows :: Int }
        -> TraceGardener
    deriving stock (Generic, Show)

instance ToJSON TraceGardener where
    toEncoding =
        defaultGenericToEncoding

instance HasSeverityAnnotation TraceGardener where
    getSeverityAnnotation = \case
        GardenerBeginGarbageCollection{} -> Notice
        GardenerPrunedIncrement{} -> Debug
        GardenerExitGarbageCollection{}  -> Notice

data TraceKupo where
    KupoExit
        :: TraceKupo
    KupoRestartingWithIndexes
        :: { distance :: Word64 }
        -> TraceKupo
    KupoUnexpectedError
        :: { exception :: Text }
        -> TraceKupo
    deriving stock (Generic, Show)

instance ToJSON TraceKupo where
    toEncoding =
        defaultGenericToEncoding

instance HasSeverityAnnotation TraceKupo where
    getSeverityAnnotation = \case
        KupoExit{} -> Debug
        KupoRestartingWithIndexes{} -> Notice
        KupoUnexpectedError{} -> Error
