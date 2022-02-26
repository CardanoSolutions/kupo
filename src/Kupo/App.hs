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
    ( ChainSyncHandler (..), TraceChainSync (..) )
import Kupo.App.Mailbox
    ( Mailbox, flushMailbox, putMailbox )
import Kupo.Configuration
    ( StandardCrypto, TraceConfiguration (..) )
import Kupo.Control.MonadDatabase
    ( Database (..), MonadDatabase (..), TraceDatabase (..) )
import Kupo.Control.MonadLog
    ( MonadLog (..), Tracer, TracerDefinition (..), TracerHKD )
import Kupo.Control.MonadSTM
    ( MonadSTM (..) )
import Kupo.Control.MonadThrow
    ( MonadThrow (..) )
import Kupo.Data.ChainSync
    ( Block
    , Point
    , SlotNo (..)
    , getHeaderHash
    , getPointSlotNo
    , getSlotNo
    , unsafeMkPoint
    )
import Kupo.Data.Pattern
    ( Pattern, matchBlock, resultToRow )

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
    checkpoints <- runTransaction (listCheckpointsDesc unsafeMkPoint)
    case (since, checkpoints) of
        (Nothing, []) -> do
            logWith tracerConfiguration $ ConfigurationInvalidOrMissingOption
                "No '--since' provided but no checkpoints were found in the \
                \database. An explicit starting point (e.g. 'origin') is \
                \required the first time launching the application."
            throwIO NoStartingPointException
        (Just{}, _:_) -> do
            logWith tracerConfiguration $ ConfigurationInvalidOrMissingOption
                "The option '--since' was provided but checkpoints were found \
                \in the database. It is not possible to decide between these \
                \two paths. Either remove the '--since' option, or point the \
                \--workdir to a fresh location."
            throwIO ConflictingOptionException
        (Nothing, pts) -> do
            let ws = unSlotNo . getPointSlotNo <$> checkpoints
            logWith tracerDatabase (DatabaseFoundCheckpoints ws)
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
    :: forall m.
        ( MonadSTM m
        , MonadLog m
        )
    => Tracers
    -> Mailbox m (Block StandardCrypto)
    -> Database m
    -> ChainSyncHandler m (Block StandardCrypto)
producer Tracers{tracerChainSync} mailbox Database{..} = ChainSyncHandler
    { onRollBackward = \pt -> do
        logWith tracerChainSync (ChainSyncRollBackward (getPointSlotNo pt))
        runTransaction $ rollbackTo (unSlotNo (getPointSlotNo pt))
    , onRollForward =
        atomically . putMailbox mailbox
    }

consumer
    :: forall m.
        ( MonadSTM m
        , MonadLog m
        , Monad (DBTransaction m)
        )
    => Tracers
    -> Mailbox m (Block StandardCrypto)
    -> [Pattern StandardCrypto]
    -> Database m
    -> m ()
consumer Tracers{tracerChainSync} mailbox patterns Database{..} = forever $ do
    blks <- atomically (flushMailbox mailbox)
    let lastKnownBlk = last blks
    let lastKnownSlot = getSlotNo lastKnownBlk
    let inputs = concatMap (matchBlock resultToRow patterns) blks
    logWith tracerChainSync (ChainSyncRollForward lastKnownSlot (length inputs))
    runTransaction $ do
        insertCheckpoint (getHeaderHash lastKnownBlk) (unSlotNo lastKnownSlot)
        insertInputs inputs

--
-- Tracers
--

type Tracers = Tracers' IO Concrete

data Tracers' m (kind :: TracerDefinition) = Tracers
    { tracerDatabase
        :: TracerHKD kind (Tracer m TraceDatabase)
    , tracerChainSync
        :: TracerHKD kind (Tracer m TraceChainSync)
    , tracerConfiguration
        :: TracerHKD kind (Tracer m TraceConfiguration)
    } deriving (Generic)

deriving instance Show (Tracers' m MinSeverities)
deriving instance Eq (Tracers' m MinSeverities)
