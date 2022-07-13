--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module Kupo.App
    ( -- * ChainProducer
      withChainProducer

      -- * Producer/Consumer
    , producer
    , consumer
    ) where

import Kupo.Prelude

import Kupo.App.ChainSync
    ( TraceChainSync (..) )
import Kupo.App.Mailbox
    ( Mailbox, flushMailbox, newMailbox, putMailbox )
import Kupo.Configuration
    ( ChainProducer (..)
    , InputManagement (..)
    , NetworkParameters (..)
    , TraceConfiguration (..)
    , parseNetworkParameters
    )
import Kupo.Control.MonadCatch
    ( MonadCatch (..) )
import Kupo.Control.MonadDatabase
    ( Database (..), MonadDatabase (..) )
import Kupo.Control.MonadLog
    ( MonadLog (..), Tracer )
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
    ( binaryDataToRow, pointToRow, resultToRow )
import Kupo.Data.Pattern
    ( Codecs (..), Pattern, matchBlock )

import qualified Data.Map as Map
import qualified Kupo.App.ChainSync.Direct as Direct
import qualified Kupo.App.ChainSync.Ogmios as Ogmios

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
    -> ( forall block. IsBlock block
            => Mailbox IO (Tip Block, block)
            -> ChainSyncClient IO block
            -> IO ()
       )
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
                  [ NodeToClientV_9 .. maxBound ]
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
    let (spentInputs, newInputs, bins) = foldMap (matchBlock codecs patterns . snd) blks
    logWith tr (ChainSyncRollForward lastKnownSlot (length newInputs))
    notifyTip lastKnownTip (Just lastKnownSlot)
    runTransaction $ do
        insertCheckpoints (foldr ((:) . pointToRow . getPoint . snd) [] blks)
        insertInputs newInputs
        onSpentInputs spentInputs
        insertBinaryData bins
  where
    codecs = Codecs
        { toResult = resultToRow
        , toSlotNo = unSlotNo
        , toInput = serialize'
        , toBinaryData = binaryDataToRow
        }

    onSpentInputs = case inputManagement of
        MarkSpentInputs ->
            void . Map.traverseWithKey markInputsByReference
        RemoveSpentInputs ->
            traverse_ deleteInputsByReference
