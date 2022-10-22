--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module Kupo.App
    ( -- * Producer(s)
      ChainSyncClient
    , newProducer
    , withFetchBlockClient

      -- * Consumer
    , consumer

      -- * Gardener
    , gardener

      -- * Tracers
    , TraceConsumer (..)
    , TraceGardener (..)
    ) where

import Kupo.Prelude

import Control.Monad.Class.MonadTimer
    ( MonadDelay (..)
    )
import Kupo.App.ChainSync
    ( TraceChainSync (..)
    , withChainSyncExceptionHandler
    )
import Kupo.App.Configuration
    ( TraceConfiguration (..)
    , parseNetworkParameters
    )
import Kupo.App.Database
    ( DBTransaction
    , Database (..)
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
    )
import Kupo.Data.ChainSync
    ( ForcedRollbackHandler (..)
    , IntersectionNotFoundException (..)
    )
import Kupo.Data.Configuration
    ( ChainProducer (..)
    , Configuration (..)
    , InputManagement (..)
    , LongestRollback (..)
    , NetworkParameters (..)
    , mailboxCapacity
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
import qualified Kupo.App.ChainSync.Node as Node
import qualified Kupo.App.ChainSync.Ogmios as Ogmios
import qualified Kupo.App.FetchBlock.Node as Node
import qualified Kupo.App.FetchBlock.Ogmios as Ogmios

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
    let forcedRollbackCallback = \point handler ->
            atomically (putTMVar forcedRollbackVar (point, handler))

    case chainProducer of
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
        Ogmios{ogmiosHost, ogmiosPort} ->
            Ogmios.withFetchBlockClient ogmiosHost ogmiosPort callback
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
    -> (Tip -> Maybe SlotNo -> DBTransaction m ())
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
            notifyTip lastKnownTip (Just lastKnownSlot)
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
        , Monad (DBTransaction m)
        )
    => Tracer IO TraceGardener
    -> Configuration
    -> TVar m (Set Pattern)
    -> (forall a. (Database m -> m a) -> m a)
    -> m Void
gardener tr config patterns withDatabase = forever $ do
    threadDelay garbageCollectionInterval
    logWith tr GardenerBeginGarbageCollection
    xs <- readTVarIO patterns
    withDatabase $ \Database{..} -> do
        (prunedInputs, prunedBinaryData) <-
            runTransaction $ do
                let
                    pruneInputsWhenApplicable =
                        case inputManagement of
                            RemoveSpentInputs -> pruneInputs
                            MarkSpentInputs -> pure 0
                    pruneBinaryDataWhenApplicable = do
                        case (inputManagement, MatchAny OnlyShelley `included` xs) of
                            (MarkSpentInputs, s) | not (Set.null s) -> pure 0
                            _needPruning -> pruneBinaryData
                 in
                    (,) <$> pruneInputsWhenApplicable
                        <*> pruneBinaryDataWhenApplicable
        runTransaction optimize
        logWith tr $ GardenerExitGarbageCollection
            { prunedInputs
            , prunedBinaryData
            }
  where
    Configuration
        { garbageCollectionInterval
        , inputManagement
        } = config

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
        :: TraceGardener
    GardenerExitGarbageCollection
        :: { prunedInputs :: Int, prunedBinaryData :: Int }
        -> TraceGardener
    deriving stock (Generic, Show)

instance ToJSON TraceGardener where
    toEncoding =
        defaultGenericToEncoding

instance HasSeverityAnnotation TraceGardener where
    getSeverityAnnotation = \case
        GardenerBeginGarbageCollection{} -> Info
        GardenerExitGarbageCollection{}  -> Info
