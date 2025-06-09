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

      -- * Rollforward variants
    , rollForwardAll
    , rollForwardUntil

      -- * Gardener
    , gardener

      -- * Exception handler
    , withExceptionHandler

      -- * Utils
    , idle
    , mkNotifyTip

      -- * Tracers
    , TraceConsumer (..)
    , TraceGardener (..)
    , TraceKupo (..)
    ) where

import Kupo.Prelude

import Control.Exception.Safe
    ( IOException
    )
import Data.Char
    ( isDigit
    )
import Data.List
    ( isInfixOf
    )
import Data.Severity
    ( HasSeverityAnnotation (..)
    , Severity (..)
    )
import Data.Time.Clock
    ( DiffTime
    )
import Kupo.App.ChainSync
    ( TraceChainSync (..)
    )
import Kupo.App.Configuration
    ( TraceConfiguration (..)
    , resolveNetworkParameters
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
    , foreverCalmly
    , threadDelay
    )
import Kupo.Control.MonadLog
    ( MonadLog (..)
    , Tracer
    , nullTracer
    , traceWith
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
import Kupo.Control.MonadTime
    ( MonadTime (..)
    )
import Kupo.Data.Cardano
    ( BinaryData
    , InputIndex
    , IsBlock
    , Point
    , SlotNo (..)
    , Tip
    , TransactionId
    , distanceToTip
    , getPoint
    , getPointSlotNo
    , getTipSlotNo
    , pattern GenesisPoint
    )
import Kupo.Data.ChainSync
    ( ForcedRollbackHandler (..)
    , HandshakeException (..)
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
import Network.Mux
    ( MuxError (..)
    , MuxErrorType (..)
    )
import Network.WebSockets
    ( ConnectionException (..)
    )
import Ouroboros.Network.Protocol.Handshake
    ( HandshakeProtocolError (..)
    )
import System.IO.Error
    ( isDoesNotExistError
    )

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Kupo.App.ChainSync.Hydra as Hydra
import qualified Kupo.App.ChainSync.Node as Node
import qualified Kupo.App.ChainSync.Ogmios as Ogmios
import qualified Kupo.App.FetchBlock.Node as Node
import qualified Kupo.App.FetchBlock.Ogmios as Ogmios
import qualified Kupo.App.FetchTip.Node as Node
import qualified Kupo.App.FetchTip.Ogmios as Ogmios
import Kupo.Data.Configuration
    ( EpochSlots
    , NetworkMagic
    )
import qualified Ouroboros.Network.Protocol.Handshake as Handshake


withExceptionHandler
    :: Tracer IO TraceKupo
    -> ConnectionStatusToggle IO
    -> IO ()
    -> IO Void
withExceptionHandler tr ConnectionStatusToggle{toggleDisconnected} io =
    foreverCalmly (handleExceptions (io $> 42))
  where
    handleExceptions :: IO DiffTime -> IO DiffTime
    handleExceptions
        = handle onHandshakeException
        . handle (onRetryableException 5 isRetryableIOException)
        . handle (onRetryableException 5 isRetryableMuxError)
        . handle (onRetryableException 5 isRetryableConnectionException)
        . handle (onRetryableException 0 isRetryableIntersectionNotFoundException)
        . (`onException` toggleDisconnected)

    onRetryableException :: Exception e => DiffTime -> (e -> Bool) -> e -> IO DiffTime
    onRetryableException retryingIn isRetryable e
        | isRetryable e = do
            traceWith tr $ KupoFailedToConnectOrConnectionLost{retryingIn}
            pure retryingIn
        | otherwise = do
            throwIO e

    isRetryableIOException :: IOException -> Bool
    isRetryableIOException e
        | isResourceVanishedError e = True
        | isDoesNotExistError e = True
        | isResourceExhaustedError e = True
        | isInvalidArgumentOnSocket e = True
        | otherwise = False

    isRetryableMuxError :: MuxError -> Bool
    isRetryableMuxError MuxError{errorType} =
        case errorType of
            MuxBearerClosed -> True
            MuxSDUReadTimeout -> True
            MuxSDUWriteTimeout -> True
            MuxIOException e -> isRetryableIOException e
            _notRetryable -> False

    isRetryableConnectionException :: ConnectionException -> Bool
    isRetryableConnectionException = \case
        CloseRequest{} -> True
        ConnectionClosed{} -> True
        ParseException{} -> False
        UnicodeException{} -> False

    isRetryableIntersectionNotFoundException :: IntersectionNotFoundException -> Bool
    isRetryableIntersectionNotFoundException = \case
        ForcedIntersectionNotFound{} -> True
        IntersectionNotFound{} -> False

-- | Show better errors when failing to handshake with the cardano-node. This is generally
-- because users have misconfigured their instance.
onHandshakeException :: HandshakeProtocolError NodeToClientVersion -> IO a
onHandshakeException = \case
    HandshakeError (Handshake.Refused _version reason) -> do
        let hint = case T.splitOn "/=" reason of
                [T.filter isDigit -> remoteConfig, T.filter isDigit -> localConfig] ->
                    unwords
                        [ "Kupo is configured for", prettyNetwork localConfig
                        , "but cardano-node is running on", prettyNetwork remoteConfig <> "."
                        , "You probably want to use a different configuration."
                        ]
                _unexpectedReasonMessage ->
                    ""
        throwIO $ HandshakeException $ unwords
            [ "Unable to establish the connection with the cardano-node:"
            , "it runs on a different network!"
            , hint
            ]
    e ->
        throwIO e

-- | Show a named version of the network magic when we recognize it for better UX.
prettyNetwork :: Text -> Text
prettyNetwork = \case
    "764824073" -> "'mainnet'"
    "1097911063" -> "'testnet'"
    "1" -> "'preview'"
    "2" -> "'preprod'"
    unknownMagic -> "'network-magic=" <> unknownMagic <> "'"

isResourceExhaustedError :: IOException -> Bool
isResourceExhaustedError =
    isInfixOf "resource exhausted" . show

isResourceVanishedError :: IOException -> Bool
isResourceVanishedError =
    isInfixOf "resource vanished" . show

-- NOTE: MacOS
isInvalidArgumentOnSocket :: IOException -> Bool
isInvalidArgumentOnSocket =
    isInfixOf "invalid argument (Socket operation on non-socket)" . show


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
    -> ChainProducer (TMVar IO NetworkParameters)
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

    networkParameters <- resolveNetworkParameters chainProducer

    case chainProducer of
        ReadOnlyReplica -> do
            mailbox <- atomically (newMailbox mailboxCapacity)
            callback @Void forcedRollbackCallback mailbox $ \_ _ _ -> pure ()

        Ogmios{ogmiosHost, ogmiosPort} -> do
            logWith tr ConfigurationOgmios { ogmiosHost, ogmiosPort }
            whenJust networkParameters $ logWith tr . ConfigurationNetwork
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
            logWith tr ConfigurationHydra { hydraHost, hydraPort }
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
            logWith tr ConfigurationCardanoNode { nodeSocket, nodeConfig }
            whenJust networkParameters $ logWith tr . ConfigurationNetwork
            mailbox <- atomically (newMailbox mailboxCapacity)

            magic <- networkMagicOrThrow networkParameters
            slots <- slotsPerEpochOrThrow networkParameters

            callback forcedRollbackCallback mailbox $ \tracerChainSync checkpoints statusToggle -> do
                withChainSyncServer
                  statusToggle
                  [ NodeToClientV_9 .. maxBound ]
                  magic
                  slots
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
    :: ChainProducer (TMVar IO NetworkParameters)
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
        CardanoNode{nodeSocket} -> do
            (chainSyncClient, fetchBlockClient) <- Node.newFetchBlockClient
            race_
                (callback fetchBlockClient)
                (withExceptionHandler nullTracer noConnectionStatusToggle $ do
                    networkParameters <- resolveNetworkParameters chainProducer
                    magic <- networkMagicOrThrow networkParameters
                    slots <- slotsPerEpochOrThrow networkParameters
                    withChainSyncServer
                        noConnectionStatusToggle
                        [ NodeToClientV_9 .. maxBound ]
                        magic
                        slots
                        nodeSocket
                        chainSyncClient
                )

newFetchTipClient
    :: ChainProducer (TMVar IO NetworkParameters)
    -> FetchTipClient IO
newFetchTipClient = \case
    ReadOnlyReplica ->
        throwIO UnableToFetchTipFromReadOnlyReplica
    Hydra{} ->
        throwIO UnableToFetchTipFromHydra
    Ogmios{ogmiosHost, ogmiosPort} ->
        Ogmios.newFetchTipClient ogmiosHost ogmiosPort
    chainProducer@CardanoNode{nodeSocket} -> do
        networkParameters <- resolveNetworkParameters chainProducer
        magic <- networkMagicOrThrow networkParameters
        slots <- slotsPerEpochOrThrow networkParameters

        response <- newEmptyTMVarIO

        withChainSyncServer
            noConnectionStatusToggle
            [ NodeToClientV_9 .. maxBound ]
            magic
            slots
            nodeSocket
            (Node.newFetchTipClient response)

        atomically $ takeTMVar response

type RollForward m block =
    Tracer IO TraceConsumer
    -> InputManagement
    -> (Tip -> Maybe Point -> DBTransaction m ())
    -> Database m
    -> Set Pattern
    -> NonEmpty (Tip, block)
    -> m ()

-- | Consumer process that is reading messages from the 'Mailbox'. Messages are
-- enqueued by another process (the producer).
consumer
    :: forall m block.
        ( MonadSTM m
        , MonadLog m
        , Monad (DBTransaction m)
        )
    => Tracer IO TraceConsumer
    -> InputManagement
    -> (Tip -> Maybe Point -> DBTransaction m ())
    -> Mailbox m (Tip, block) (Tip, Point)
    -> TVar m (Set Pattern)
    -> Database m
    -> RollForward m block
    -> m Void
consumer tr inputManagement notifyTip mailbox patternsVar database@Database{..} rollForward =
    forever $ do
        logWith tr ConsumerWaitingForNextBatch
        atomically ((,) <$> flushMailbox mailbox <*> readTVar patternsVar) >>= \case
            (Left blks, patterns) ->
                rollForward tr inputManagement notifyTip database patterns blks
            (Right pt, _) ->
                rollBackward pt
  where
    rollBackward :: (Tip, Point) -> m ()
    rollBackward (tip, pt) = do
        logWith tr (ConsumerRollBackward { point = getPointSlotNo pt })
        runTransaction $ do
            lastKnownSlot <- rollbackTo (getPointSlotNo pt)
            notifyTip tip lastKnownSlot

rollForwardAll
    :: forall m block.
        ( MonadSTM m
        , MonadLog m
        , Monad (DBTransaction m)
        , IsBlock block
        )
    => RollForward m block
rollForwardAll tr inputManagement notifyTip Database{..} patterns blks = do
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
  where
    codecs = Codecs
        { toResult = resultToRow
        , toBinaryData = binaryDataToRow
        , toScript = scriptToRow
        , toPolicy = policyToRow
        }
    onSpentInputs
        :: Tip
        -> SlotNo
        -> Map (TransactionId, SlotNo) [(Pattern, InputIndex, Maybe BinaryData)]
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
                    fmap sum . traverse (deleteInputs . fmap (\(a, _, _) -> a))
                else
                    fmap sum . Map.traverseWithKey markInputs
      where
        unstableWindow =
            getLongestRollback longestRollback


rollForwardUntil
    :: forall m block.
        ( MonadSTM m
        , MonadLog m
        , Monad (DBTransaction m)
        , IsBlock block
        )
    => (Point -> Bool)
    -> RollForward m block
rollForwardUntil until tr inputManagement notifyTip database patterns blks = do
    let blksBefore = NE.takeWhile (until . getPoint . snd) blks
    whenJust (nonEmpty blksBefore) $
        rollForwardAll tr inputManagement notifyTip database patterns

readOnlyConsumer
    :: forall m.
        ( MonadSTM m
        , MonadDelay m
        , MonadTime m
        )
    => TVar m Health
    -> Database m
    -> m Void
readOnlyConsumer health Database{..} = do
    forever $ do
        mostRecentCheckpoint <- runTransaction listCheckpointsDesc <&> \case
            []  -> Nothing
            h:_ -> Just h
        now <- getCurrentTime
        recordCheckpoint health now (maybe 0 getPointSlotNo mostRecentCheckpoint) mostRecentCheckpoint
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

networkMagicOrThrow :: MonadThrow m => Maybe NetworkParameters -> m NetworkMagic
networkMagicOrThrow =
    maybe (throwIO FailedToRetrieveNetworkParameters) (pure . networkMagic)

slotsPerEpochOrThrow :: MonadThrow m => Maybe NetworkParameters -> m EpochSlots
slotsPerEpochOrThrow =
    maybe (throwIO FailedToRetrieveNetworkParameters) (pure . slotsPerEpoch)

mkNotifyTip
    :: (MonadThrow m, MonadSTM m, MonadTime m)
    => DeferIndexesInstallation
    -> TVar m Health
    -> Tip
    -> Maybe Point
    -> m ()
mkNotifyTip indexMode health tip point = do
    now <- getCurrentTime
    case indexMode of
        InstallIndexesIfNotExist -> pure ()
        SkipNonEssentialIndexes  -> do
            let distance = maybe maxBound (distanceToTip tip . getPointSlotNo) point
            when (distance <= 60) $ throwIO NodeTipHasBeenReached{ distance }
    recordCheckpoint health now (getTipSlotNo tip) point

data FailedToRetrieveNetworkParameters = FailedToRetrieveNetworkParameters deriving (Show, Generic)
instance Exception FailedToRetrieveNetworkParameters

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
    KupoFailedToConnectOrConnectionLost
        :: { retryingIn :: DiffTime }
        -> TraceKupo
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
        KupoFailedToConnectOrConnectionLost{} -> Warning
        KupoRestartingWithIndexes{} -> Notice
        KupoUnexpectedError{} -> Error
