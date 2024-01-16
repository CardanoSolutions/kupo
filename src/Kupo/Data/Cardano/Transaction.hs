module Kupo.Data.Cardano.Transaction where

import Kupo.Prelude

import Kupo.Data.Cardano.TransactionId
    ( HasTransactionId (..)
    , transactionIdFromByron
    )

import qualified Cardano.Chain.UTxO as Ledger.Byron
import qualified Cardano.Ledger.Alonzo.Tx as Ledger.Alonzo
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Shelley.Tx as Ledger.Shelley


-- Transaction

type Transaction = Transaction' StandardCrypto

data Transaction' crypto
    = TransactionByron
        !Ledger.Byron.Tx
        !Ledger.Byron.TxId
    | TransactionShelley
        !(Ledger.Shelley.ShelleyTx (ShelleyEra crypto))
    | TransactionAllegra
        !(Ledger.Shelley.ShelleyTx (AllegraEra crypto))
    | TransactionMary
        !(Ledger.Shelley.ShelleyTx (MaryEra crypto))
    | TransactionAlonzo
        !(Ledger.Alonzo.AlonzoTx (AlonzoEra crypto))
    | TransactionBabbage
        !(Ledger.Alonzo.AlonzoTx (BabbageEra crypto))
    | TransactionConway
        !(Ledger.Alonzo.AlonzoTx (ConwayEra crypto))

instance HasTransactionId Transaction StandardCrypto where
    getTransactionId = \case
        TransactionByron _ i ->
            transactionIdFromByron i
        TransactionShelley tx ->
            let body = Ledger.Shelley.body tx
             in Ledger.txid @(ShelleyEra StandardCrypto) body
        TransactionAllegra tx ->
            let body = Ledger.Shelley.body tx
             in Ledger.txid @(AllegraEra StandardCrypto) body
        TransactionMary tx ->
            let body = Ledger.Shelley.body tx
             in Ledger.txid @(MaryEra StandardCrypto) body
        TransactionAlonzo tx ->
            let body = Ledger.Alonzo.body tx
             in Ledger.txid @(AlonzoEra StandardCrypto) body
        TransactionBabbage tx ->
            let body = Ledger.Alonzo.body tx
             in Ledger.txid @(BabbageEra StandardCrypto) body
        TransactionConway tx ->
            let body = Ledger.Alonzo.body tx
             in Ledger.txid @(ConwayEra StandardCrypto) body
