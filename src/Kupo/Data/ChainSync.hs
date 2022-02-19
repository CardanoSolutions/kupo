--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}

module Kupo.Data.ChainSync
    ( -- * Constraints
      Crypto

      -- * Block
    , Block
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
    , getAddress
    , Value
    , getValue
    , DatumHash
    , getDatumHash

      -- * Address
    , Address
    , addressFromBytes
    , isBootstrap
    , getPaymentPartBytes
    , getDelegationPartBytes

      -- * SlotNo
    , SlotNo (..)

      -- * Hash
    , digest
    , digestSize
    , Blake2b_224
    , Blake2b_256

      -- * HeaderHash
    , HeaderHash

      -- * Point
    , Point (..)
    , pattern GenesisPoint
    , pattern BlockPoint

      -- * Tip
    , Tip (..)
    ) where

import Kupo.Prelude

import Cardano.Crypto.Hash
    ( Blake2b_224, Blake2b_256, HashAlgorithm (..), sizeHash )
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
import Cardano.Ledger.Val
    ( Val (inject) )
import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Data.Binary.Put
    ( runPut )
import Data.Maybe.Strict
    ( StrictMaybe (..), strictMaybeToMaybe )
import Data.Sequence.Strict
    ( pattern (:<|), pattern Empty, StrictSeq )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock, HardForkBlock (..) )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..) )
import Ouroboros.Network.Block
    ( pattern BlockPoint
    , pattern GenesisPoint
    , HeaderHash
    , Point (..)
    , StandardHash
    , Tip (..)
    )

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.Tx as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxSeq as Ledger.Alonzo
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger.Core
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Era as Ledger.Era
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Cardano.Ledger.Shelley.BlockChain as Ledger.Shelley
import qualified Cardano.Ledger.Shelley.Tx as Ledger.Shelley
import qualified Cardano.Ledger.ShelleyMA.TxBody as Ledger.MaryAllegra
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
    :: forall a crypto. (Crypto crypto)
    => (OutputReference crypto -> Output crypto -> Maybe a)
    -> Transaction crypto
    -> [a]
mapMaybeOutputs fn = \case
    TransactionShelley tx ->
        let
            body = Ledger.Shelley.body tx
            txId = Ledger.txid @(ShelleyEra crypto) body
            outs = Ledger.Shelley._outputs body
         in
            traverseAndTransform (asAlonzoOutput inject) txId 0 outs
    TransactionAllegra tx ->
        let
            body = Ledger.Shelley.body tx
            txId = Ledger.txid @(AllegraEra crypto) body
            outs = Ledger.MaryAllegra.outputs' body
         in
            traverseAndTransform (asAlonzoOutput inject) txId 0 outs
    TransactionMary tx ->
        let
            body = Ledger.Shelley.body tx
            txId = Ledger.txid @(MaryEra crypto) body
            outs = Ledger.MaryAllegra.outputs' body
         in
            traverseAndTransform (asAlonzoOutput identity) txId 0 outs
    TransactionAlonzo tx ->
        let
            body = Ledger.Alonzo.body tx
            txId = Ledger.txid @(AlonzoEra crypto) body
            outs = Ledger.Alonzo.outputs' body
         in
            traverseAndTransform identity txId 0 outs
  where
    traverseAndTransform
        :: forall output. ()
        => (output -> Output crypto)
        -> TransactionId crypto
        -> Natural
        -> StrictSeq output
        -> [a]
    traverseAndTransform transform txId ix = \case
        Empty -> []
        output :<| rest ->
            let
                outputRef = Ledger.TxIn txId ix
                results   = traverseAndTransform transform txId (succ ix) rest
             in
                case fn outputRef (transform output) of
                    Nothing ->
                        results
                    Just result ->
                        result : results

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
    :: (Crypto crypto)
    => Output crypto
    -> Address crypto
getAddress (Ledger.Alonzo.TxOut address _value _datumHash) =
    address

getValue
    :: (Crypto crypto)
    => Output crypto
    -> Value crypto
getValue (Ledger.Alonzo.TxOut _address value _datumHash) =
    value

getDatumHash
    :: (Crypto crypto)
    => Output crypto
    -> Maybe (DatumHash crypto)
getDatumHash (Ledger.Alonzo.TxOut _address _value datumHash) =
    (strictMaybeToMaybe datumHash)

asAlonzoOutput
    :: forall (era :: Type -> Type) crypto.
        ( Ledger.Era.Era (era crypto)
        , Ledger.Era.Crypto (era crypto) ~ crypto
        , Ledger.Core.TxOut (era crypto) ~ Ledger.Shelley.TxOut (era crypto)
        , Show (Ledger.Core.Value (era crypto))
        )
    => (Ledger.Core.Value (era crypto) -> Ledger.Value crypto)
    -> Ledger.Core.TxOut (era crypto)
    -> Ledger.Core.TxOut (AlonzoEra crypto)
asAlonzoOutput liftValue (Ledger.Shelley.TxOut addr value) =
    Ledger.Alonzo.TxOut addr (liftValue value) SNothing

-- Address

type Address crypto = Ledger.Addr crypto

addressFromBytes :: Crypto crypto => ByteString -> Maybe (Address crypto)
addressFromBytes = Ledger.deserialiseAddr
{-# INLINEABLE addressFromBytes #-}

isBootstrap :: Address crypto -> Bool
isBootstrap = \case
    Ledger.AddrBootstrap{} -> True
    Ledger.Addr{} -> False
{-# INLINEABLE isBootstrap #-}

getPaymentPartBytes
    :: Address crypto
    -> Maybe ByteString
getPaymentPartBytes = \case
    Ledger.Addr _ payment _ ->
        Just $ toStrict $ runPut $ Ledger.putCredential payment
    Ledger.AddrBootstrap{} ->
        Nothing

getDelegationPartBytes
    :: Address crypto
    -> Maybe ByteString
getDelegationPartBytes = \case
    Ledger.Addr _ _ (Ledger.StakeRefBase delegation) ->
        Just $ toStrict $ runPut $ Ledger.putCredential delegation
    Ledger.Addr{} ->
        Nothing
    Ledger.AddrBootstrap{} ->
        Nothing

-- Hash

digestSize :: forall alg. HashAlgorithm alg => Int
digestSize =
    fromIntegral (sizeHash (Proxy @alg))
