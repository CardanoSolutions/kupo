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
    | TransactionDijkstra
        !(Ledger.Tx Ledger.TopTx DijkstraEra)

instance HasTransactionId Transaction where
    getTransactionId = \case
        TransactionByron _ i ->
            transactionIdFromByron i
        TransactionShelley tx ->
            Ledger.txIdTx @ShelleyEra tx
        TransactionAllegra tx ->
            Ledger.txIdTx @AllegraEra tx
        TransactionMary tx ->
            Ledger.txIdTx @MaryEra tx
        TransactionAlonzo tx ->
            Ledger.txIdTx @AlonzoEra tx
        TransactionBabbage tx ->
            Ledger.txIdTx @BabbageEra tx
        TransactionConway tx ->
            Ledger.txIdTx @ConwayEra tx
        TransactionDijkstra tx ->
            Ledger.txIdTx @DijkstraEra tx
