module Kupo.Data.Cardano.Transaction where

import Kupo.Prelude

import Kupo.Data.Cardano.TransactionId
    ( HasTransactionId (..)
    , transactionIdFromByron
    )

import qualified Cardano.Chain.UTxO as Ledger.Byron
import qualified Cardano.Ledger.Alonzo.Tx as Ledger.Alonzo
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Shelley.Tx as Ledger.Shelley


-- Transaction

data Transaction
    = TransactionByron
        !Ledger.Byron.Tx
        !Ledger.Byron.TxId
    | TransactionShelley
        !(Ledger.Shelley.ShelleyTx ShelleyEra)
    | TransactionAllegra
        !(Ledger.Shelley.ShelleyTx AllegraEra)
    | TransactionMary
        !(Ledger.Shelley.ShelleyTx MaryEra)
    | TransactionAlonzo
        !(Ledger.Alonzo.AlonzoTx AlonzoEra)
    | TransactionBabbage
        !(Ledger.Alonzo.AlonzoTx BabbageEra)
    | TransactionConway
        !(Ledger.Alonzo.AlonzoTx ConwayEra)

instance HasTransactionId Transaction where
    getTransactionId = \case
        TransactionByron _ i ->
            transactionIdFromByron i
        TransactionShelley tx ->
            let body = Ledger.Shelley.body tx
             in Ledger.txIdTxBody @ShelleyEra body
        TransactionAllegra tx ->
            let body = Ledger.Shelley.body tx
             in Ledger.txIdTxBody @AllegraEra body
        TransactionMary tx ->
            let body = Ledger.Shelley.body tx
             in Ledger.txIdTxBody @MaryEra body
        TransactionAlonzo tx ->
            let body = Ledger.Alonzo.body tx
             in Ledger.txIdTxBody @AlonzoEra body
        TransactionBabbage tx ->
            let body = Ledger.Alonzo.body tx
             in Ledger.txIdTxBody @BabbageEra body
        TransactionConway tx ->
            let body = Ledger.Alonzo.body tx
             in Ledger.txIdTxBody @ConwayEra body
