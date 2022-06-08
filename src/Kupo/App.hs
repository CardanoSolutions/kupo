--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module Kupo.App
    ( -- * Application Startup
      startOrResume
    , newPatternsCache
    , ConflictingOptionsException (..)
    , NoStartingPointException (..)

      -- * ChainProducer
    , withChainProducer

      -- * Producer/Consumer
    , producer
    , consumer

      -- * Tracers
    , Tracers' (..)
    , Tracers
    ) where

import Kupo.Prelude

import Kupo.App.ChainSync
    ( TraceChainSync (..) )
import Kupo.App.Http
    ( TraceHttpServer )
import Kupo.App.Mailbox
    ( Mailbox, flushMailbox, newMailbox, putMailbox )
import Kupo.Configuration
    ( ChainProducer (..)
    , Configuration (..)
    , InputManagement (..)
    , NetworkParameters (..)
    , TraceConfiguration (..)
    , parseNetworkParameters
    )
import Kupo.Control.MonadCatch
    ( MonadCatch (..) )
import Kupo.Control.MonadDatabase
    ( Database (..), MonadDatabase (..), TraceDatabase (..) )
import Kupo.Control.MonadLog
    ( MonadLog (..), Tracer, TracerDefinition (..), TracerHKD )
import Kupo.Control.MonadOuroboros
    ( MonadOuroboros (..), NodeToClientVersion (..) )
import Kupo.Control.MonadSTM
    ( MonadSTM (..) )
import Kupo.Control.MonadThrow
    ( MonadThrow (..) )
import Kupo.Data.Cardano
    ( Block, IsBlock, Point, SlotNo (..), Tip, getPoint, getPointSlotNo )
import Kupo.Data.ChainSync
    ( ChainSyncHandler (..), IntersectionNotFoundException (..) )
import Kupo.Data.Database
    ( patternFromRow, patternToRow, pointFromRow, pointToRow, resultToRow )
import Kupo.Data.Pattern
    ( Pattern, matchBlock, patternToText )

import qualified Kupo.App.ChainSync.Direct as Direct
import qualified Kupo.App.ChainSync.Ogmios as Ogmios

--
-- Application Bootstrapping
--

startOrResume
    :: forall m.
        ( MonadThrow m
        , MonadLog m
        )
    => Tracers
    -> Configuration
    -> Database m
    -> m [Point Block]
startOrResume tracers configuration Database{..} = do
    -- TODO: include sanity check and fail if '--prune-utxo' is included but
    -- there exists marked inputs in the database. This would indicate that the
    -- index was restarted with a different configuration.
    checkpoints <- runTransaction (listCheckpointsDesc pointFromRow)
    case nonEmpty (sortOn Down (unSlotNo . getPointSlotNo <$> checkpoints)) of
        Nothing -> pure ()
        Just slots -> do
            let mostRecentCheckpoint = head slots
            let oldestCheckpoint = last slots
            let totalCheckpoints = length slots
            logWith tracerDatabase $ DatabaseFoundCheckpoints
                { totalCheckpoints
                , mostRecentCheckpoint
                , oldestCheckpoint
                }
    case (since, checkpoints) of
        (Nothing, []) -> do
            logWith tracerConfiguration errNoStartingPoint
            throwIO NoStartingPointException
        (Just point, mostRecentCheckpoint:_) -> do
            if getPointSlotNo point > getPointSlotNo mostRecentCheckpoint then do
                logWith tracerConfiguration errConflictingOptions
                throwIO ConflictingOptionsException
            else do
                pure (sortOn (Down . getPointSlotNo) (point : checkpoints))
        (Nothing, pts) -> do
            pure pts
        (Just pt, []) ->
            pure [pt]
  where
    Tracers{tracerDatabase, tracerConfiguration} = tracers
    Configuration{since} = configuration

    errNoStartingPoint = ConfigurationInvalidOrMissingOption
        "No '--since' provided and no checkpoints found in the \
        \database. An explicit starting point (e.g. 'origin') is \
        \required the first time launching the application."

    errConflictingOptions = ConfigurationInvalidOrMissingOption
        "The point provided through '--since' is more recent than \
        \any of the known checkpoints and it isn't possible to make \
        \a choice for resuming the application: should synchronization \
        \restart from the latest checkpoint or from the provided \
        \--since point? Please dispel the confusion by either choosing \
        \a different starting point (or none at all) or by using a \
        \fresh new database."


newPatternsCache
    :: forall m.
        ( MonadThrow m
        , MonadSTM m
        , MonadLog m
        )
    => Tracers
    -> Configuration
    -> Database m
    -> m (TVar m [Pattern])
newPatternsCache tracers configuration Database{..} = do
    alreadyKnownPatterns <- runTransaction (listPatterns patternFromRow)
    patterns <- case (alreadyKnownPatterns, configuredPatterns) of
        (x:xs, []) ->
            pure (x:xs)
        ([], y:ys) -> do
            runTransaction (insertPatterns (patternToRow <$> (y:ys)))
            pure (y:ys)
        ([], []) ->
            pure []
        (xs, ys) | sort xs /= sort ys -> do
            logWith tracerConfiguration errConflictingOptions
            throwIO ConflictingOptionsException
        (xs, _) ->
            pure xs
    logWith tracerConfiguration $ ConfigurationPatterns (patternToText <$> patterns)
    newTVarIO patterns
  where
    Tracers{tracerConfiguration} = tracers
    Configuration{patterns = configuredPatterns} = configuration

    errConflictingOptions = ConfigurationInvalidOrMissingOption
        "Configuration patterns are different from previously known \
        \patterns. Restarting a running server using different \
        \command-line patterns is not allowed for it may likely be \
        \an error. If you do intent do dynamically manage patterns, \
        \please use the HTTP API instead of the command-line options."

data NoStartingPointException = NoStartingPointException deriving (Show)
instance Exception NoStartingPointException

data ConflictingOptionsException = ConflictingOptionsException  deriving (Show)
instance Exception ConflictingOptionsException

--
-- Chain Producer
--

type ChainSyncClient m block =
       Tracer IO TraceChainSync
    -> [Point Block] -- Checkpoints
    -> (Tip Block -> Maybe SlotNo -> m ()) -- Tip notifier
    -> ConnectionStatusToggle m
    -> Database m
    -> m ()

withChainProducer
    :: Tracer IO TraceConfiguration
    -> ChainProducer
    -> (forall block. IsBlock block => Mailbox IO (Tip Block, block) -> ChainSyncClient IO block -> IO ())
    -> IO ()
withChainProducer tracerConfiguration chainProducer callback = do
    case chainProducer of
        Ogmios{ogmiosHost, ogmiosPort} -> do
            logWith tracerConfiguration ConfigurationOgmios{ogmiosHost, ogmiosPort}
            mailbox <- atomically (newMailbox mailboxSize)
            callback mailbox $ \tracerChainSync checkpoints notifyTip statusToggle db -> do
                let chainSyncHandler = producer tracerChainSync notifyTip mailbox db
                Ogmios.connect statusToggle ogmiosHost ogmiosPort $
                    Ogmios.runChainSyncClient chainSyncHandler checkpoints

        CardanoNode{nodeSocket, nodeConfig} -> do
            logWith tracerConfiguration ConfigurationCardanoNode{nodeSocket,nodeConfig}
            mailbox <- atomically (newMailbox mailboxSize)
            network@NetworkParameters
                { networkMagic
                , slotsPerEpoch
                } <- liftIO (parseNetworkParameters nodeConfig)
            logWith tracerConfiguration (ConfigurationNetwork network)
            callback mailbox $ \tracerChainSync checkpoints notifyTip statusToggle db -> do
                let chainSyncHandler = producer tracerChainSync notifyTip mailbox db
                withChainSyncServer
                  statusToggle
                  [ NodeToClientV_9 .. NodeToClientV_12 ]
                  networkMagic
                  slotsPerEpoch
                  nodeSocket
                  (Direct.mkChainSyncClient chainSyncHandler checkpoints)
                  & handle (\e@IntersectionNotFound{requestedPoints = points} -> do
                      logWith tracerChainSync ChainSyncIntersectionNotFound{points}
                      throwIO e
                    )

-- TODO: Make the mailbox's size configurable? Larger sizes means larger
-- memory usage at the benefits of slightly faster sync time... Though I
-- am not sure it's worth enough to bother users with this. Between 100
-- and 1000, the heap increases from ~150MB to ~1GB, for a 5-10%
-- synchronization time decrease.
mailboxSize :: Natural
mailboxSize = 100

--
-- Producer / Consumer
--

producer
    :: forall m block.
        ( MonadSTM m
        , MonadLog m
        )
    => Tracer IO TraceChainSync
    -> (Tip Block -> Maybe SlotNo -> m ())
    -> Mailbox m (Tip Block, block)
    -> Database m
    -> ChainSyncHandler m (Tip Block) (Point Block) block
producer tr notifyTip mailbox Database{..} = ChainSyncHandler
    { onRollBackward = \tip pt -> do
        logWith tr (ChainSyncRollBackward (getPointSlotNo pt))
        lastKnownSlot <- runTransaction $ rollbackTo (unSlotNo (getPointSlotNo pt))
        notifyTip tip (SlotNo <$> lastKnownSlot)
    , onRollForward = \tip blk ->
        atomically (putMailbox mailbox (tip, blk))
    }

consumer
    :: forall m block.
        ( MonadSTM m
        , MonadLog m
        , MonadFail m
        , Monad (DBTransaction m)
        , IsBlock block
        )
    => Tracer IO TraceChainSync
    -> InputManagement
    -> (Tip Block -> Maybe SlotNo -> m ())
    -> Mailbox m (Tip Block, block)
    -> TVar m [Pattern]
    -> Database m
    -> m ()
consumer tr inputManagement notifyTip mailbox patternsVar Database{..} = forever $ do
    (blks, patterns) <- atomically $ (,) <$> flushMailbox mailbox <*> readTVar patternsVar
    let (lastKnownTip, lastKnownBlk) = last blks
    let lastKnownPoint = getPoint lastKnownBlk
    let lastKnownSlot = getPointSlotNo lastKnownPoint
    when (lastKnownSlot >= 58836325) $ fail "done" -- TODO: TMP
    let (spentInputs, newInputs) = foldMap (matchBlock resultToRow serialize' patterns . snd) blks
    logWith tr (ChainSyncRollForward lastKnownSlot (length newInputs))
    notifyTip lastKnownTip (Just lastKnownSlot)
    runTransaction $ do
        insertCheckpoint (pointToRow lastKnownPoint)
        insertInputs newInputs
        onSpentInputs spentInputs
  where
    onSpentInputs = case inputManagement of
        MarkSpentInputs ->
            markInputsByReference
        RemoveSpentInputs ->
            deleteInputsByReference

--
-- Tracers
--

type Tracers = Tracers' IO Concrete

data Tracers' m (kind :: TracerDefinition) = Tracers
    { tracerHttp
        :: TracerHKD kind (Tracer m TraceHttpServer)
    , tracerDatabase
        :: TracerHKD kind (Tracer m TraceDatabase)
    , tracerChainSync
        :: TracerHKD kind (Tracer m TraceChainSync)
    , tracerConfiguration
        :: TracerHKD kind (Tracer m TraceConfiguration)
    } deriving (Generic)

deriving instance Show (Tracers' m MinSeverities)
deriving instance Eq (Tracers' m MinSeverities)
