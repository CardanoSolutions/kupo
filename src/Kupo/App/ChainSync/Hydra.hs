--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.ChainSync.Hydra
    ( connect
    , runChainSyncClient
    , newTransactionStore
    , TransactionStore (..)
    , TransactionStoreException (..)
    ) where

import Kupo.Prelude

import Control.Exception.Safe
    ( MonadThrow
    , throwM
    )
import qualified Data.Map as Map
import Kupo.App.Mailbox
    ( Mailbox
    , putHighFrequencyMessage
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Data.Cardano
    ( Point
    , Tip
    , TransactionId
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
    ( PartialBlock
    , PartialTransaction (..)
    )
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Json as WS

runChainSyncClient
    :: forall m.
        ( MonadIO m
        , MonadSTM m
        , MonadThrow m, MonadThrow (STM m))
    => Mailbox m (Tip, PartialBlock) (Tip, Point)
    -> m () -- An action to run before the main loop starts.
    -> [Point]
    -> WS.Connection
    -> m IntersectionNotFoundException
runChainSyncClient mailbox beforeMainLoop _pts ws = do
    beforeMainLoop
    TransactionStore{pushTx, popTxById} <- newTransactionStore
    forever $ do
        WS.receiveJson ws decodeHydraMessage >>= \case
            HeadIsOpen ->
                atomically (putHighFrequencyMessage mailbox (mkHydraBlock 0 []))
            TxValid{tx} ->
                pushTx tx
            SnapshotConfirmed{ snapshot = Snapshot { number, confirmedTransactionIds }} -> do
                txs <- mapM popTxById confirmedTransactionIds
                atomically (putHighFrequencyMessage mailbox (mkHydraBlock number txs))
            SomethingElse -> pure ()

connect
    :: ConnectionStatusToggle IO
    -> String
    -> Int
    -> (WS.Connection -> IO a)
    -> IO a
connect ConnectionStatusToggle{toggleConnected} host port action =
    WS.runClientWith host port "/"
        WS.defaultConnectionOptions [] (\ws -> toggleConnected >> action ws)

newtype TransactionStoreException = TransactionNotInStore { transactionId :: TransactionId }
  deriving (Eq, Show)

instance Exception TransactionStoreException

-- | Handle to store and later retrieve transaction.
data TransactionStore m = TransactionStore
    { -- | Store a transaction for later retrieval.
      pushTx :: PartialTransaction -> m ()
    , -- | Resolves a transaction id and removes it. Throws
      -- 'TransactionNotInStore' when not found.
      popTxById :: MonadThrow m => TransactionId -> m PartialTransaction
    }

newTransactionStore :: (Monad m, MonadSTM m, MonadThrow (STM m)) => m (TransactionStore m)
newTransactionStore = do
  txStore <- atomically $ newTVar mempty
  pure
    TransactionStore
        { pushTx = \tx@PartialTransaction{id} -> atomically $ modifyTVar' txStore (Map.insert id tx)
        , popTxById = \txId ->
            atomically $ do
                txMap <- readTVar txStore
                case Map.lookup txId txMap of
                  Nothing -> throwM $ TransactionNotInStore txId
                  Just tx -> do
                      writeTVar txStore (Map.delete txId txMap)
                      pure tx
        }



