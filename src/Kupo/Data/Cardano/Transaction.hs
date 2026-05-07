module Kupo.Data.Cardano.Transaction where

import Kupo.Prelude

import Kupo.Data.Cardano.TransactionId
    ( HasTransactionId (..)
    , transactionIdFromByron
    )

import qualified Cardano.Chain.UTxO as Ledger.Byron
import qualified Cardano.Ledger.Core as Ledger


-- Transaction

data Transaction
    = TransactionByron
        !Ledger.Byron.Tx
        !Ledger.Byron.TxId
    | TransactionShelley
        !(Ledger.Tx Ledger.TopTx ShelleyEra)
    | TransactionAllegra
        !(Ledger.Tx Ledger.TopTx AllegraEra)
    | TransactionMary
        !(Ledger.Tx Ledger.TopTx MaryEra)
    | TransactionAlonzo
        !(Ledger.Tx Ledger.TopTx AlonzoEra)
    | TransactionBabbage
        !(Ledger.Tx Ledger.TopTx BabbageEra)
    | TransactionConway
        !(Ledger.Tx Ledger.TopTx ConwayEra)

instance HasTransactionId Transaction where
    getTransactionId = \case
        TransactionByron _ i ->
            transactionIdFromByron i
        TransactionShelley tx ->
            Ledger.txIdTxBody @ShelleyEra (tx ^. Ledger.bodyTxL)
        TransactionAllegra tx ->
            Ledger.txIdTxBody @AllegraEra (tx ^. Ledger.bodyTxL)
        TransactionMary tx ->
            Ledger.txIdTxBody @MaryEra (tx ^. Ledger.bodyTxL)
        TransactionAlonzo tx ->
            Ledger.txIdTxBody @AlonzoEra (tx ^. Ledger.bodyTxL)
        TransactionBabbage tx ->
            Ledger.txIdTxBody @BabbageEra (tx ^. Ledger.bodyTxL)
        TransactionConway tx ->
            Ledger.txIdTxBody @ConwayEra (tx ^. Ledger.bodyTxL)
