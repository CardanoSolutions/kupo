--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module Kupo.App
    ( -- * Application Startup
      startOrResume
    , ConflictingOptionException (..)
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

import Kupo.App.Http
    ( TraceHttpServer )
import Kupo.App.Mailbox
    ( Mailbox, flushMailbox, newMailbox, putMailbox )
import Kupo.Configuration
    ( ChainProducer (..)
    , Configuration (..)
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
    ( IntersectionNotFoundException (..)
    , MonadOuroboros (..)
    , NodeToClientVersion (..)
    , TraceChainSync (..)
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..) )
import Kupo.Control.MonadThrow
    ( MonadThrow (..) )
import Kupo.Data.Cardano
    ( Block, IsBlock, Point, SlotNo (..), Tip, getPoint, getPointSlotNo )
import Kupo.Data.ChainSync
    ( ChainSyncHandler (..) )
import Kupo.Data.Database
    ( patternFromRow, patternToRow, pointFromRow, pointToRow, resultToRow )
import Kupo.Data.Pattern
    ( Pattern, matchBlock )

import qualified Kupo.App.ChainSync.Direct as Direct
import qualified Kupo.App.ChainSync.Ogmios as Ogmios

--
-- Application Bootstrapping
--

startOrResume
    :: forall m.
        ( MonadThrow m
        , MonadSTM m
        , MonadLog m
        )
    => Tracers
    -> Configuration
    -> Database m
    -> m (TVar m [Pattern], [Point Block])
startOrResume tracers configuration Database{..} = do
    checkpoints <- initCheckpoints
    patterns <- newTVarIO =<< initPatterns
    pure (patterns, checkpoints)
  where
    Tracers{tracerDatabase, tracerConfiguration} = tracers
    Configuration{since, patterns = configuredPatterns} = configuration

    initCheckpoints = do
        checkpoints <- runTransaction (listCheckpointsDesc pointFromRow)
        unless (null checkpoints) $ do
            let ws = unSlotNo . getPointSlotNo <$> checkpoints
            logWith tracerDatabase (DatabaseFoundCheckpoints ws)

        case (since, checkpoints) of
            (Nothing, []) -> do
                logWith tracerConfiguration $ ConfigurationInvalidOrMissingOption
                    "No '--since' provided and no checkpoints found in the \
                    \database. An explicit starting point (e.g. 'origin') is \
                    \required the first time launching the application."
                throwIO NoStartingPointException
            (Just point, mostRecentCheckpoint:_) -> do
                if getPointSlotNo point > getPointSlotNo mostRecentCheckpoint then do
                    logWith tracerConfiguration $ ConfigurationInvalidOrMissingOption
                        "The point provided through '--since' is more recent than \
                        \any of the known checkpoints and it isn't possible to make \
                        \a choice for resuming the application: should synchronization \
                        \restart from the latest checkpoint or from the provided \
                        \--since point? Please dispel the confusion by either choosing \
                        \a different starting point (or none at all) or by using a \
                        \fresh new database."
                    throwIO ConflictingOptionException
                else do
                    pure (sortOn (Down . getPointSlotNo) (point : checkpoints))
            (Nothing, pts) -> do
                pure pts
            (Just pt, []) ->
                pure [pt]

    initPatterns = do
        patterns <- runTransaction (listPatterns patternFromRow)
        case (patterns, configuredPatterns) of
            (x:xs, []) ->
                pure (x:xs)
            ([], y:ys) -> do
                runTransaction (insertPatterns (patternToRow <$> (y:ys)))
                pure (y:ys)
            ([], []) ->
                pure []
            (xs, ys) | sort xs /= sort ys -> do
                logWith tracerConfiguration $ ConfigurationInvalidOrMissingOption
                    "Configuration patterns are different from previously known \
                    \patterns. Restarting a running server using different \
                    \command-line patterns is not allowed for it may likely be \
                    \an error. If you do intent do dynamically manage patterns, \
                    \please use the HTTP API instead of the command-line options."
                throwIO ConflictingOptionException
            (xs, _) ->
                pure xs

data NoStartingPointException = NoStartingPointException deriving (Show)
instance Exception NoStartingPointException

data ConflictingOptionException = ConflictingOptionException  deriving (Show)
instance Exception ConflictingOptionException

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
                  tracerChainSync
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
        , Monad (DBTransaction m)
        , IsBlock block
        )
    => Tracer IO TraceChainSync
    -> (Tip Block -> Maybe SlotNo -> m ())
    -> Mailbox m (Tip Block, block)
    -> TVar m [Pattern]
    -> Database m
    -> m ()
consumer tr notifyTip mailbox patternsVar Database{..} = forever $ do
    (blks, patterns) <- atomically $ (,) <$> flushMailbox mailbox <*> readTVar patternsVar
    let (lastKnownTip, lastKnownBlk) = last blks
    let lastKnownPoint = getPoint lastKnownBlk
    let lastKnownSlot = getPointSlotNo lastKnownPoint
    let inputs = concatMap (matchBlock resultToRow patterns . snd) blks
    logWith tr (ChainSyncRollForward lastKnownSlot (length inputs))
    notifyTip lastKnownTip (Just lastKnownSlot)
    runTransaction $ do
        insertCheckpoint (pointToRow lastKnownPoint)
        insertInputs inputs

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
