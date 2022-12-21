--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kupo.Data.Cardano
    ( IsBlock (..)
    , module Kupo.Data.Cardano.Address
    , module Kupo.Data.Cardano.AssetId
    , module Kupo.Data.Cardano.AssetName
    , module Kupo.Data.Cardano.BinaryData
    , module Kupo.Data.Cardano.Block
    , module Kupo.Data.Cardano.BlockNo
    , module Kupo.Data.Cardano.Datum
    , module Kupo.Data.Cardano.DatumHash
    , module Kupo.Data.Cardano.HeaderHash
    , module Kupo.Data.Cardano.Metadata
    , module Kupo.Data.Cardano.MetadataHash
    , module Kupo.Data.Cardano.NativeScript
    , module Kupo.Data.Cardano.Output
    , module Kupo.Data.Cardano.OutputReference
    , module Kupo.Data.Cardano.OutputIndex
    , module Kupo.Data.Cardano.Point
    , module Kupo.Data.Cardano.PolicyId
    , module Kupo.Data.Cardano.Script
    , module Kupo.Data.Cardano.ScriptHash
    , module Kupo.Data.Cardano.ScriptReference
    , module Kupo.Data.Cardano.SlotNo
    , module Kupo.Data.Cardano.Tip
    , module Kupo.Data.Cardano.Transaction
    , module Kupo.Data.Cardano.TransactionId
    , module Kupo.Data.Cardano.TransactionIndex
    , module Kupo.Data.Cardano.Value
    ) where

import Kupo.Prelude

import Cardano.Ledger.Val
    ( Val (inject)
    )
import Data.Maybe.Strict
    ( StrictMaybe (..)
    , strictMaybe
    )
import Data.Sequence.Strict
    ( StrictSeq
    , pattern (:<|)
    , pattern Empty
    )
import GHC.Records
    ( HasField (..)
    )
import Ouroboros.Consensus.Byron.Ledger.Mempool
    ( GenTx (..)
    )
import Ouroboros.Consensus.Cardano.Block
    ( HardForkBlock (..)
    )
import Ouroboros.Consensus.Ledger.SupportsMempool
    ( HasTxs (..)
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    )
import Ouroboros.Network.Block
    ( blockPoint
    )

import Kupo.Data.Cardano.Address
import Kupo.Data.Cardano.AssetId
import Kupo.Data.Cardano.AssetName
import Kupo.Data.Cardano.BinaryData
import Kupo.Data.Cardano.Block
import Kupo.Data.Cardano.BlockNo
import Kupo.Data.Cardano.Datum
import Kupo.Data.Cardano.DatumHash
import Kupo.Data.Cardano.HeaderHash
import Kupo.Data.Cardano.Metadata
import Kupo.Data.Cardano.MetadataHash
import Kupo.Data.Cardano.NativeScript
import Kupo.Data.Cardano.Output
import Kupo.Data.Cardano.OutputIndex
import Kupo.Data.Cardano.OutputReference
import Kupo.Data.Cardano.Point
import Kupo.Data.Cardano.PolicyId
import Kupo.Data.Cardano.Script
import Kupo.Data.Cardano.ScriptHash
import Kupo.Data.Cardano.ScriptReference
import Kupo.Data.Cardano.SlotNo
import Kupo.Data.Cardano.Tip
import Kupo.Data.Cardano.Transaction
import Kupo.Data.Cardano.TransactionId
import Kupo.Data.Cardano.TransactionIndex
import Kupo.Data.Cardano.Value

import qualified Cardano.Chain.UTxO as Ledger.Byron
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.Tx as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxSeq as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Cardano.Ledger.Babbage.TxBody as Ledger.Babbage
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Shelley.BlockChain as Ledger.Shelley
import qualified Cardano.Ledger.Shelley.Metadata as Ledger
import qualified Cardano.Ledger.Shelley.Tx as Ledger.Shelley
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as Ledger.MaryAllegra
import qualified Cardano.Ledger.ShelleyMA.TxBody as Ledger.MaryAllegra
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Data.Set as Set

-- IsBlock

class HasTransactionId (BlockBody block) StandardCrypto => IsBlock (block :: Type) where
    type BlockBody block :: Type

    getPoint
        :: block
        -> Point

    foldBlock
        :: (TransactionIndex -> BlockBody block -> result -> result)
        -> result
        -> block
        -> result

    spentInputs
        :: BlockBody block
        -> Set Input

    mapMaybeOutputs
        :: (OutputReference -> Output -> Maybe result)
        -> BlockBody block
        -> [result]

    witnessedDatums
        :: BlockBody block
        -> Map DatumHash BinaryData

    witnessedScripts
        :: BlockBody block
        -> Map ScriptHash Script

    userDefinedMetadata
        :: BlockBody block
        -> Maybe (MetadataHash, Metadata)

-- Block

instance IsBlock Block where
    type BlockBody Block = Transaction

    getPoint
        :: Block
        -> Point
    getPoint =
        blockPoint

    foldBlock
        :: (TransactionIndex -> Transaction -> result -> result)
        -> result
        -> Block
        -> result
    foldBlock fn result = \case
        BlockByron blk ->
            let
                ignoreProtocolTxs ix = \case
                    ByronTx txId (Ledger.Byron.taTx -> tx) ->
                        fn ix (TransactionByron tx txId)
                    _protocolUpdateOrVote ->
                        identity
             in
                foldrWithIndex ignoreProtocolTxs result (extractTxs blk)
        BlockShelley (ShelleyBlock (Ledger.Block _ txs) _) ->
            foldrWithIndex (\ix -> fn ix . TransactionShelley) result (Ledger.Shelley.txSeqTxns' txs)
        BlockAllegra (ShelleyBlock (Ledger.Block _ txs) _) ->
            foldrWithIndex (\ix -> fn ix . TransactionAllegra) result (Ledger.Shelley.txSeqTxns' txs)
        BlockMary (ShelleyBlock (Ledger.Block _ txs) _) ->
            foldrWithIndex (\ix -> fn ix . TransactionMary) result (Ledger.Shelley.txSeqTxns' txs)
        BlockAlonzo (ShelleyBlock (Ledger.Block _ txs) _) ->
            foldrWithIndex (\ix -> fn ix . TransactionAlonzo) result (Ledger.Alonzo.txSeqTxns txs)
        BlockBabbage (ShelleyBlock (Ledger.Block _ txs) _) ->
            foldrWithIndex (\ix -> fn ix . TransactionBabbage) result (Ledger.Alonzo.txSeqTxns txs)

    spentInputs
        :: Transaction
        -> Set Input
    spentInputs = \case
        TransactionByron tx _ ->
            foldr (Set.insert . transformByron) Set.empty (Ledger.Byron.txInputs tx)
        TransactionShelley tx ->
            getField @"inputs" (getField @"body" tx)
        TransactionAllegra tx ->
            getField @"inputs" (getField @"body" tx)
        TransactionMary tx ->
            getField @"inputs" (getField @"body" tx)
        TransactionAlonzo tx ->
            case Ledger.Alonzo.isValid tx of
                Ledger.Alonzo.IsValid True ->
                    getField @"inputs" (getField @"body" tx)
                Ledger.Alonzo.IsValid False ->
                    getField @"collateral" (getField @"body" tx)
        TransactionBabbage tx ->
            case Ledger.Alonzo.isValid tx of
                Ledger.Alonzo.IsValid True ->
                    getField @"inputs" (getField @"body" tx)
                Ledger.Alonzo.IsValid False ->
                    getField @"collateral" (getField @"body" tx)
      where
        transformByron (Ledger.Byron.TxInUtxo txId ix) =
            mkOutputReference
                (transactionIdFromByron txId)
                ix

    mapMaybeOutputs
        :: forall result. ()
        => (OutputReference -> Output -> Maybe result)
        -> Transaction
        -> [result]
    mapMaybeOutputs fn = \case
        TransactionByron tx (transactionIdFromByron -> txId) ->
            let
                out :| outs = Ledger.Byron.txOutputs tx
             in
                traverseAndTransformByron fromByronOutput txId 0 (out : outs)
        TransactionShelley tx ->
            let
                body = Ledger.Shelley.body tx
                txId = Ledger.txid @(ShelleyEra StandardCrypto) body
                outs = Ledger.Shelley._outputs body
             in
                traverseAndTransform (fromShelleyOutput inject) txId 0 outs
        TransactionAllegra tx ->
            let
                body = Ledger.Shelley.body tx
                txId = Ledger.txid @(AllegraEra StandardCrypto) body
                outs = Ledger.MaryAllegra.outputs' body
             in
                traverseAndTransform (fromShelleyOutput inject) txId 0 outs
        TransactionMary tx ->
            let
                body = Ledger.Shelley.body tx
                txId = Ledger.txid @(MaryEra StandardCrypto) body
                outs = Ledger.MaryAllegra.outputs' body
             in
                traverseAndTransform (fromShelleyOutput identity) txId 0 outs
        TransactionAlonzo tx ->
            let
                body = Ledger.Alonzo.body tx
                txId = Ledger.txid @(AlonzoEra StandardCrypto) body
                outs = Ledger.Alonzo.outputs' body
             in
                case Ledger.Alonzo.isValid tx of
                    Ledger.Alonzo.IsValid True ->
                        traverseAndTransform fromAlonzoOutput txId 0 outs
                    Ledger.Alonzo.IsValid False ->
                        []
        TransactionBabbage tx ->
            let
                body = Ledger.Alonzo.body tx
                txId = Ledger.txid @(BabbageEra StandardCrypto) body
                outs = Ledger.Babbage.outputs' body
             in
                case Ledger.Alonzo.isValid tx of
                    Ledger.Alonzo.IsValid True ->
                        traverseAndTransform identity txId 0 outs
                    Ledger.Alonzo.IsValid False ->
                        case Ledger.Babbage.collateralReturn' body of
                            SNothing -> []
                            SJust r  -> traverseAndTransform identity txId 0 (r :<| mempty)
      where
        traverseAndTransformByron
            :: forall output. ()
            => (output -> Output)
            -> TransactionId
            -> OutputIndex
            -> [output]
            -> [result]
        traverseAndTransformByron transform txId ix = \case
            [] -> []
            (out:rest) ->
                let
                    outputRef = mkOutputReference txId ix
                    results   = traverseAndTransformByron transform txId (next ix) rest
                 in
                    case fn outputRef (transform out) of
                        Nothing ->
                            results
                        Just result ->
                            result : results

        traverseAndTransform
            :: forall output. ()
            => (output -> Output)
            -> TransactionId
            -> OutputIndex
            -> StrictSeq output
            -> [result]
        traverseAndTransform transform txId ix = \case
            Empty -> []
            output :<| rest ->
                let
                    outputRef = mkOutputReference txId ix
                    results   = traverseAndTransform transform txId (next ix) rest
                 in
                    case fn outputRef (transform output) of
                        Nothing ->
                            results
                        Just result ->
                            result : results

    witnessedDatums
        :: Transaction
        -> Map DatumHash BinaryData
    witnessedDatums = \case
        TransactionByron{} ->
            mempty
        TransactionShelley{} ->
            mempty
        TransactionAllegra{} ->
            mempty
        TransactionMary{} ->
            mempty
        TransactionAlonzo tx ->
            fromAlonzoData <$>
                Ledger.unTxDats (getField @"txdats" (getField @"wits" tx))
        TransactionBabbage tx ->
            fromBabbageData <$>
                Ledger.unTxDats (getField @"txdats" (getField @"wits" tx))

    witnessedScripts
        :: Transaction
        -> Map ScriptHash Script
    witnessedScripts = \case
        TransactionByron{} ->
            mempty
        TransactionShelley{} ->
            mempty
        TransactionAllegra tx ->
            ( fromAllegraScript
                <$> getField @"scriptWits" tx
            ) & strictMaybe identity (scriptFromAllegraAuxiliaryData fromAllegraScript)
                    (getField @"auxiliaryData" tx)
        TransactionMary tx ->
            ( fromMaryScript
                <$> getField @"scriptWits" tx
            ) & strictMaybe identity (scriptFromAllegraAuxiliaryData fromMaryScript)
                    (getField @"auxiliaryData" tx)
        TransactionAlonzo tx ->
            ( fromAlonzoScript
                <$> getField @"txscripts" (getField @"wits" tx)
            ) & strictMaybe identity (scriptFromAlonzoAuxiliaryData fromAlonzoScript)
                    (getField @"auxiliaryData" tx)
        TransactionBabbage tx ->
            ( fromBabbageScript
                <$> getField @"txscripts" (getField @"wits" tx)
            ) & strictMaybe identity (scriptFromAlonzoAuxiliaryData fromBabbageScript)
                    (getField @"auxiliaryData" tx)

    userDefinedMetadata
        :: Transaction
        -> Maybe (MetadataHash, Metadata)
    userDefinedMetadata = \case
        TransactionByron{} ->
            Nothing
        TransactionShelley tx ->
            case getField @"auxiliaryData" tx of
                SNothing ->
                    Nothing
                SJust (Ledger.Metadata meta) ->
                    Just (mkMetadata meta)
        TransactionAllegra tx ->
            case getField @"auxiliaryData" tx of
                SNothing ->
                    Nothing
                SJust (Ledger.MaryAllegra.AuxiliaryData meta _scripts) ->
                    Just (mkMetadata meta)
        TransactionMary tx ->
            case getField @"auxiliaryData" tx of
                SNothing ->
                    Nothing
                SJust (Ledger.MaryAllegra.AuxiliaryData meta _scripts) ->
                    Just (mkMetadata meta)
        TransactionAlonzo tx ->
            case getField @"auxiliaryData" tx of
                SNothing ->
                    Nothing
                SJust (Ledger.AuxiliaryData meta _scripts) ->
                    Just (mkMetadata meta)
        TransactionBabbage tx ->
            case getField @"auxiliaryData" tx of
                SNothing ->
                    Nothing
                SJust (Ledger.AuxiliaryData meta _scripts) ->
                    Just (mkMetadata meta)
