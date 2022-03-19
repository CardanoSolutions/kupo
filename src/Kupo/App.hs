--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module Kupo.App
    ( -- * Application Startup
      startOrResume

      -- * Producer/Consumer
    , producer
    , consumer

      -- * Tracers
    , Tracers' (..)
    , Tracers
    ) where

import Kupo.Prelude

import Kupo.App.ChainSync
    ( ChainSyncHandler (..) )
import Kupo.App.Http
    ( TraceHttpServer )
import Kupo.App.Mailbox
    ( Mailbox, flushMailbox, putMailbox )
import Kupo.Configuration
    ( StandardCrypto, TraceConfiguration (..) )
import Kupo.Control.MonadDatabase
    ( Database (..), MonadDatabase (..), TraceDatabase (..) )
import Kupo.Control.MonadLog
    ( MonadLog (..), Tracer, TracerDefinition (..), TracerHKD )
import Kupo.Control.MonadOuroboros
    ( TraceChainSync (..) )
import Kupo.Control.MonadSTM
    ( MonadSTM (..) )
import Kupo.Control.MonadThrow
    ( MonadThrow (..) )
import Kupo.Data.ChainSync
    ( Block, Point, SlotNo (..), Tip, getPoint, getPointSlotNo )
import Kupo.Data.Database
    ( pointFromRow, pointToRow, resultToRow )
import Kupo.Data.Pattern
    ( Pattern, matchBlock )

--
-- Application Bootstrapping
--

startOrResume
    :: forall m.
        ( MonadThrow m
        , MonadLog m
        )
    => Tracers
    -> Database m
    -> Maybe (Point (Block StandardCrypto))
    -> m [Point (Block StandardCrypto)]
startOrResume Tracers{tracerDatabase, tracerConfiguration} Database{..} since = do
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

data NoStartingPointException = NoStartingPointException deriving (Show)
instance Exception NoStartingPointException

data ConflictingOptionException = ConflictingOptionException  deriving (Show)
instance Exception ConflictingOptionException

--
-- Producer / Consumer
--

producer
    :: forall m block.
        ( MonadSTM m
        , MonadLog m
        , block ~ Block StandardCrypto
        )
    => Tracer IO TraceChainSync
    -> (Tip block -> Maybe SlotNo -> m ())
    -> Mailbox m (Tip block, block)
    -> Database m
    -> ChainSyncHandler m (Block StandardCrypto)
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
        , block ~ Block StandardCrypto
        )
    => Tracer IO TraceChainSync
    -> (Tip block -> Maybe SlotNo -> m ())
    -> Mailbox m (Tip block, block)
    -> [Pattern StandardCrypto]
    -> Database m
    -> m ()
consumer tr notifyTip mailbox patterns Database{..} = forever $ do
    blks <- atomically (flushMailbox mailbox)
    let (lastKnownTip, lastKnownBlk) = last blks
    let lastKnownPoint = getPoint lastKnownBlk
    let lastKnownSlot = getPointSlotNo lastKnownPoint
    let inputs = concatMap (matchBlock resultToRow patterns . snd) blks
    logWith tr (ChainSyncRollForward lastKnownSlot (length inputs))
    notifyTip lastKnownTip (Just lastKnownSlot)
    runTransaction $ do
        -- TODO: inserting checkpoints could / should actually be done by
        -- 'insertInputs', instead of leaking here in the application logic.
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
