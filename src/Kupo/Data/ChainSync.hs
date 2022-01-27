--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Kupo.Data.ChainSync
    ( -- * Constraints
      Crypto

      -- * Block
    , Block (..)
    , IsBlock
    , foldBlock

      -- * Transaction
    , Transaction
    , TransactionId
    , getTransactionId
    , mapMaybeOutputs

      -- * Input
    , Input
    , OutputReference

      -- * Output
    , Output
    , Address
    , getAddress
    , Value
    , getValue
    , DatumHash
    , getDatumHash

      -- * Address
    , getPaymentPartBytes
    , getDelegationPartBytes
    , serialiseAddress
    , deserialiseAddress

      -- * Point
    , Point (..)
    , pattern GenesisPoint

      -- * Tip
    , Tip (..)
    ) where

import Kupo.Prelude

import Cardano.Ledger.Allegra
    ( AllegraEra )
import Cardano.Ledger.Alonzo
    ( AlonzoEra )
import Cardano.Ledger.Crypto
    ( Crypto )
import Cardano.Ledger.Mary
    ( MaryEra )
import Cardano.Ledger.Shelley
    ( ShelleyEra )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock, CardanoEras, GenTx (..), HardForkBlock (..) )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..) )
import Ouroboros.Network.Block
    ( pattern GenesisPoint, Point (..), StandardHash, Tip (..) )

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.Tx as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxSeq as Ledger.Alonzo
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Cardano.Ledger.Shelley.BlockChain as Ledger.Shelley
import qualified Cardano.Ledger.Shelley.Tx as Ledger.Shelley
import qualified Cardano.Ledger.TxIn as Ledger

-- Block

type Block crypto =
    CardanoBlock crypto

type IsBlock block =
    ( StandardHash block
    , Typeable block
    )

foldBlock
    :: forall crypto b. (Crypto crypto)
    => (Transaction crypto -> b -> b)
    -> b
    -> Block crypto
    -> b
foldBlock fn b = \case
    BlockByron{} ->
        b
    BlockShelley (ShelleyBlock (Ledger.Block _ txs) _) ->
        foldr (fn . TransactionShelley) b (Ledger.Shelley.txSeqTxns' txs)
    BlockAllegra (ShelleyBlock (Ledger.Block _ txs) _) ->
        foldr (fn . TransactionAllegra) b (Ledger.Shelley.txSeqTxns' txs)
    BlockMary (ShelleyBlock (Ledger.Block _ txs) _) ->
        foldr (fn . TransactionMary) b (Ledger.Shelley.txSeqTxns' txs)
    BlockAlonzo (ShelleyBlock (Ledger.Block _ txs) _) ->
        foldr (fn . TransactionAlonzo) b (Ledger.Alonzo.txSeqTxns txs)

-- Transaction

data Transaction crypto
    = TransactionShelley
        (Ledger.Shelley.Tx (ShelleyEra crypto))
    | TransactionAllegra
        (Ledger.Shelley.Tx (AllegraEra crypto))
    | TransactionMary
        (Ledger.Shelley.Tx (MaryEra crypto))
    | TransactionAlonzo
        (Ledger.Alonzo.ValidatedTx (AlonzoEra crypto))

type TransactionId crypto =
    Ledger.TxId crypto

getTransactionId
    :: forall crypto. (Crypto crypto)
    => Transaction crypto
    -> TransactionId crypto
getTransactionId = \case
    TransactionShelley tx ->
        Ledger.txid @(ShelleyEra crypto) (Ledger.Shelley.body tx)
    TransactionAllegra tx ->
        Ledger.txid @(AllegraEra crypto) (Ledger.Shelley.body tx)
    TransactionMary tx ->
        Ledger.txid @(MaryEra crypto)    (Ledger.Shelley.body tx)
    TransactionAlonzo tx ->
        Ledger.txid @(AlonzoEra crypto)  (Ledger.Alonzo.body tx)

mapMaybeOutputs
    :: (OutputReference crypto -> Output crypto -> Maybe a)
    -> Transaction crypto
    -> [a]
mapMaybeOutputs fn tx =
    undefined

-- Input

type Input crypto =
    Ledger.TxIn crypto

type OutputReference crypto =
    Input crypto

-- Output

type Output crypto =
    Ledger.Alonzo.TxOut (AlonzoEra crypto)

type DatumHash crypto =
    Ledger.DataHash crypto

type Value crypto =
    Ledger.Value crypto

getAddress
    :: Output crypto
    -> Address crypto
getAddress =
    undefined

getValue
    :: Output crypto
    -> Value crypto
getValue =
    undefined

getDatumHash
    :: Output crypto
    -> Maybe (DatumHash crypto)
getDatumHash =
    undefined

-- Address

type Address crypto = Ledger.Addr crypto

getPaymentPartBytes
    :: Address crypto
    -> ByteString
getPaymentPartBytes =
    undefined

getDelegationPartBytes
    :: Address crypto
    -> Maybe ByteString
getDelegationPartBytes =
    undefined

serialiseAddress
    :: Address crypto
    -> ByteString
serialiseAddress =
    undefined

deserialiseAddress
    :: ByteString
    -> Maybe (Address crypto)
deserialiseAddress =
    undefined
