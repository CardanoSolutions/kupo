--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.ChainSync.Hydra
    ( connect
    , runChainSyncClient
    ) where

import Kupo.Prelude

import Kupo.App.Mailbox
    ( Mailbox
    , putHighFrequencyMessage
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Control.MonadThrow
    ( MonadThrow (..)
    )
import Kupo.Data.Cardano
    ( Point
    , Tip
    , WithOrigin (..)
    , getPointSlotNo
    , getTipSlotNo
    )
import Kupo.Data.ChainSync
    ( IntersectionNotFoundException (..)
    )
import Kupo.Data.Hydra
    ( HydraMessage (..)
    , Snapshot (..)
    , decodeHydraMessage
    , mkHydraBlock
    )
import Kupo.Data.PartialBlock
    ( PartialBlock (..)
    )

import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Json as WS

runChainSyncClient
    :: forall m.
        ( MonadIO m
        , MonadSTM m
        , MonadThrow m
        )
    => Mailbox m (Tip, PartialBlock) (Tip, Point)
    -> m () -- An action to run before the main loop starts.
    -> [Point]
    -> WS.Connection
    -> m IntersectionNotFoundException
runChainSyncClient mailbox beforeMainLoop pts ws = do
    beforeMainLoop
    flip evalStateT (sortOn getPointSlotNo pts) $ forever $ do
       lift (WS.receiveJson ws decodeHydraMessage) >>= \case
            HeadIsOpen{genesisTxs} -> do
                handleBlock $ mkHydraBlock 0 genesisTxs
            TxValid{} ->
                pure ()
            SnapshotConfirmed{ snapshot = Snapshot { number, confirmedTransactions }} -> do
                handleBlock $ mkHydraBlock number confirmedTransactions
            SomethingElse -> do
                pure ()
  where
    -- Hydra doesn't provide any mechanism to negotiate an intersection point and receive snapshots
    -- only from a certain point. So we 'fake' it by skippping blocks until we have caught up with our
    -- state.
    --
    -- - We start from a (possibly empty) ascending list of checkpoints that we have already processed.
    -- - For each 'block' received from the Hydra head, we have one of three situation.
    --
    --   1. Its slot number is lower than the current cursor (i.e. smallest checkpoint)
    --   2. Its slot number is equal to the current cursor
    --   3. Its slot number is higher than the current cursor.
    --
    -- - In the case of (1), it's simple[NOTE^A], we have already seen that block and we can just skip it. The
    -- cursor doesn't move as we haven't caught up with that checkpoint yet.
    --
    -- - In the case of (2), we have two possibilities:
    --
    --   1. The block header hash and the checkpoint header hash are identical
    --      -> This means we've found a block that's on both chains. We can skip it as we already know
    --      of it. We can move to the next checkpoint too.
    --
    --   2. The block header hash and the checkpoint header hash are different
    --      -> This means we (the indexer) is on a fork. But we don't have enough information to know
    --      where to rollback to. So we throw an exception.
    --      Note that this truly is *exceptional* because Hydra heads cannot fork in principle. The
    --      only way we end up here is if someone tries to connect an already synchronized index to a
    --      new head which happen to have different chain histories. And that's a very valid reason to
    --      crash.
    --
    -- - The case of (3) is in principle _impossible_ because a Hydra snapshots are monotically
    -- increasing with no gaps. So we can't possibly hit that scenario without first hitting (2).
    --
    -- - When the list of checkpoints is empty, it means we have caught up with the chain and any new
    -- block is relevant to the indexer.
    --
    --
    -- NOTE^A: It is "simple" because we assume that there is not two different Hydra "chains" that
    -- might have two equal blocks. However, in principle, we should verify that the discarded blocks
    -- still form a chain up to the last known block we stored. Otherwise, there could be a scenario
    -- where we have stored: b1, b2, b3 but receive b1', b2', b3. If are waiting for a snapshot number
    -- at 3, then we will discard b1' and b2' even though they are actually different from what we
    -- stored.
    --
    -- This can really only happen if we have concomittantly:
    --   (a) Someone restarting the indexer on a *different* Hydra head.
    --   (b) ALL of our selected checkpoint being identical on both heads.
    --
    -- Without (b), the condition 2.2 would already throw an exception. Given the scenario we're in,
    -- we consider this quite unlikely.
    handleBlock
        :: (Tip, PartialBlock)
        -> StateT [Point] m ()
    handleBlock (tip, block) = do
        get >>= \case
            -- No checkpoints, all blocks are relevant.
            [] -> do
                lift $ atomically (putHighFrequencyMessage mailbox (tip, block))

            requestedPoints@(cursor:_) -> do
                   -- Block is before a known checkpoint, ignoring.
                if | slot < getPointSlotNo cursor -> do
                       return ()

                   -- Block is at checkpoint
                   | slot == getPointSlotNo cursor -> do
                       if point == cursor then do
                           -- Checkpoint matches, we skip the block and move the cursor
                           modify' (drop 1)
                       else
                            lift $ throwIO $ IntersectionNotFound
                               { requestedPoints = At . getPointSlotNo <$> requestedPoints
                               , tip = At . getTipSlotNo $ tip
                               }

                   -- Block is after a known checkpoint, absurd
                   | otherwise -> do
                       error $ "impossible: received a block *after* a known checkpoint? \
                               \There isn't supposed to be any gaps in Hydra snapshots \
                               \however this suggests that there was one.\
                               \We choked on: " <> show point <> ", while expecting a \
                               \snapshot at: " <> show cursor <> "."
      where
        point = blockPoint block
        slot = getPointSlotNo point

connect
    :: ConnectionStatusToggle IO
    -> String
    -> Int
    -> (WS.Connection -> IO a)
    -> IO a
connect ConnectionStatusToggle{toggleConnected} host port action =
    WS.runClientWith host port "/?history=yes"
        WS.defaultConnectionOptions [] (\ws -> toggleConnected >> action ws)
