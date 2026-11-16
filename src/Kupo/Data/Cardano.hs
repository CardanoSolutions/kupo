--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kupo.Data.Cardano
    ( IsBlock (..)
    , UnsupportedSubTransactions (..)
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
    , module Kupo.Data.Cardano.Redeemers
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

import Cardano.Ledger.BaseTypes
    ( inject
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
import Kupo.Data.Cardano.Redeemers
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
import qualified Cardano.Ledger.Alonzo.Core as Ledger
import qualified Cardano.Ledger.Alonzo.TxWits as Ledger
import qualified Cardano.Ledger.Babbage.Core as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Dijkstra.Core as Ledger
import qualified Data.Set as Set

-- IsBlock

class HasTransactionId (BlockBody block) => IsBlock (block :: Type) where
    type BlockBody block :: Type

    getPoint
        :: block
        -> Point

    foldBlock
        :: (TransactionIndex -> BlockBody block -> result -> result)
        -> result
        -> block
        -> result

    -- | Fails on a block whose Dijkstra sub-transactions are not indexable.
    checkSubTransactions
        :: block
        -> Either UnsupportedSubTransactions ()

    -- | The sub-transactions of a top-level transaction, in the order the ledger applies them.
    -- None by default.
    subTransactions
        :: BlockBody block
        -> [BlockBody block]
    subTransactions =
        const []

    -- | The sub-transactions of a phase-2 invalid top-level transaction, which the ledger discards.
    -- None by default.
    discardedSubTransactions
        :: BlockBody block
        -> [BlockBody block]
    discardedSubTransactions =
        const []

    spentInputs
        :: BlockBody block
        -> Set Input

    mapMaybeOutputs
        :: (OutputReference -> Output -> Metadata -> Maybe result)
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

    spendRedeemer
        :: BlockBody block
        -> InputIndex
        -> Maybe BinaryData

-- Block

-- | Raised on a Dijkstra block whose sub-transactions Kupo cannot index.
data UnsupportedSubTransactions
    = UnverifiableSubTransactions !Point
    -- ^ A Dijkstra block whose transactions Kupo cannot inspect.
    deriving (Show)

instance Exception UnsupportedSubTransactions where
    displayException = \case
        UnverifiableSubTransactions point ->
            "Ogmios does not serialise Dijkstra transactions yet, so Kupo cannot index the block at slot "
                <> toString (slotNoToText (getPointSlotNo point))
                <> ". Connect Kupo directly to cardano-node to index Dijkstra blocks."

instance IsBlock Void where
    type BlockBody Void = Void
    getPoint = absurd
    foldBlock _ _ = absurd
    checkSubTransactions = absurd
    subTransactions = absurd
    discardedSubTransactions = absurd
    spentInputs = absurd
    mapMaybeOutputs _ = absurd
    witnessedDatums = absurd
    witnessedScripts = absurd
    userDefinedMetadata = absurd
    spendRedeemer = absurd

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
            foldrWithIndex (\ix -> fn ix . TransactionShelley) result (txs ^. Ledger.txSeqBlockBodyL)
        BlockAllegra (ShelleyBlock (Ledger.Block _ txs) _) ->
            foldrWithIndex (\ix -> fn ix . TransactionAllegra) result (txs ^. Ledger.txSeqBlockBodyL)
        BlockMary (ShelleyBlock (Ledger.Block _ txs) _) ->
            foldrWithIndex (\ix -> fn ix . TransactionMary) result (txs ^. Ledger.txSeqBlockBodyL)
        BlockAlonzo (ShelleyBlock (Ledger.Block _ txs) _) ->
            foldrWithIndex (\ix -> fn ix . TransactionAlonzo) result (txs ^. Ledger.txSeqBlockBodyL)
        BlockBabbage (ShelleyBlock (Ledger.Block _ txs) _) ->
            foldrWithIndex (\ix -> fn ix . TransactionBabbage) result (txs ^. Ledger.txSeqBlockBodyL)
        BlockConway (ShelleyBlock (Ledger.Block _ txs) _) ->
            foldrWithIndex (\ix -> fn ix . TransactionConway) result (txs ^. Ledger.txSeqBlockBodyL)
        BlockDijkstra (ShelleyBlock (Ledger.Block _ txs) _) ->
            foldrWithIndex (\ix -> fn ix . TransactionDijkstra) result (txs ^. Ledger.txSeqBlockBodyL)

    checkSubTransactions =
        const (Right ())

    subTransactions
        :: Transaction
        -> [Transaction]
    subTransactions = \case
        TransactionByron{} ->
            []
        TransactionShelley{} ->
            []
        TransactionAllegra{} ->
            []
        TransactionMary{} ->
            []
        TransactionAlonzo{} ->
            []
        TransactionBabbage{} ->
            []
        TransactionConway{} ->
            []
        TransactionDijkstra tx ->
            -- A phase-2 invalid transaction applies none of its sub-transactions.
            case tx ^. Ledger.isPhase2ValidTxL of
                Ledger.Phase2Valid ->
                    SubTransactionDijkstra <$> toList (tx ^. Ledger.bodyTxL . Ledger.subTransactionsTxBodyL)
                Ledger.Phase2Invalid ->
                    []
        -- Sub-transactions cannot themselves contain sub-transactions.
        SubTransactionDijkstra{} ->
            []

    discardedSubTransactions
        :: Transaction
        -> [Transaction]
    discardedSubTransactions = \case
        TransactionByron{} ->
            []
        TransactionShelley{} ->
            []
        TransactionAllegra{} ->
            []
        TransactionMary{} ->
            []
        TransactionAlonzo{} ->
            []
        TransactionBabbage{} ->
            []
        TransactionConway{} ->
            []
        TransactionDijkstra tx ->
            case tx ^. Ledger.isPhase2ValidTxL of
                Ledger.Phase2Valid ->
                    []
                Ledger.Phase2Invalid ->
                    SubTransactionDijkstra <$> toList (tx ^. Ledger.bodyTxL . Ledger.subTransactionsTxBodyL)
        SubTransactionDijkstra{} ->
            []

    spentInputs
        :: Transaction
        -> Set Input
    spentInputs = \case
        TransactionByron tx _ ->
            foldr (Set.insert . transformByron) Set.empty (Ledger.Byron.txInputs tx)
        TransactionShelley tx ->
            tx ^. Ledger.bodyTxL . Ledger.inputsTxBodyL
        TransactionAllegra tx ->
            tx ^. Ledger.bodyTxL . Ledger.inputsTxBodyL
        TransactionMary tx ->
            tx ^. Ledger.bodyTxL . Ledger.inputsTxBodyL
        TransactionAlonzo tx ->
            case tx ^. Ledger.isPhase2ValidTxL of
                Ledger.Phase2Valid ->
                    tx ^. Ledger.bodyTxL . Ledger.inputsTxBodyL
                Ledger.Phase2Invalid ->
                    tx ^. Ledger.bodyTxL . Ledger.collateralInputsTxBodyL
        TransactionBabbage tx ->
            case tx ^. Ledger.isPhase2ValidTxL of
                Ledger.Phase2Valid ->
                    tx ^. Ledger.bodyTxL . Ledger.inputsTxBodyL
                Ledger.Phase2Invalid ->
                    tx ^. Ledger.bodyTxL . Ledger.collateralInputsTxBodyL
        TransactionConway tx ->
            case tx ^. Ledger.isPhase2ValidTxL of
                Ledger.Phase2Valid ->
                    tx ^. Ledger.bodyTxL . Ledger.inputsTxBodyL
                Ledger.Phase2Invalid ->
                    tx ^. Ledger.bodyTxL . Ledger.collateralInputsTxBodyL
        TransactionDijkstra tx ->
            case tx ^. Ledger.isPhase2ValidTxL of
                Ledger.Phase2Valid ->
                    tx ^. Ledger.bodyTxL . Ledger.inputsTxBodyL
                Ledger.Phase2Invalid ->
                    tx ^. Ledger.bodyTxL . Ledger.collateralInputsTxBodyL
        SubTransactionDijkstra tx ->
            tx ^. Ledger.bodyTxL . Ledger.inputsTxBodyL
      where
        transformByron (Ledger.Byron.TxInUtxo txId ix) =
            mkOutputReference
                (transactionIdFromByron txId)
                ix

    mapMaybeOutputs
        :: forall result. ()
        => (OutputReference -> Output -> Metadata -> Maybe result)
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
                body = tx ^. Ledger.bodyTxL
                txId = Ledger.txIdTxBody @ShelleyEra body
                outs = body ^. Ledger.outputsTxBodyL
                meta = tx ^. Ledger.auxDataTxL & strictMaybe emptyMetadata fromShelleyMetadata
             in
                traverseAndTransform (fromShelleyOutput inject) txId meta 0 outs
        TransactionAllegra tx ->
            let
                body = tx ^. Ledger.bodyTxL
                txId = Ledger.txIdTxBody @AllegraEra body
                outs = body ^. Ledger.outputsTxBodyL
                meta = tx ^. Ledger.auxDataTxL & strictMaybe emptyMetadata fromAllegraMetadata
             in
                traverseAndTransform (fromShelleyOutput inject) txId meta 0 outs
        TransactionMary tx ->
            let
                body = tx ^. Ledger.bodyTxL
                txId = Ledger.txIdTxBody @MaryEra body
                outs = body ^. Ledger.outputsTxBodyL
                meta = tx ^. Ledger.auxDataTxL & strictMaybe emptyMetadata fromMaryMetadata
             in
                traverseAndTransform (fromShelleyOutput @MaryEra identity) txId meta 0 outs
        TransactionAlonzo tx ->
            let
                body = tx ^. Ledger.bodyTxL
                txId = Ledger.txIdTxBody @AlonzoEra body
                outs = body ^. Ledger.outputsTxBodyL
                meta = tx ^. Ledger.auxDataTxL & strictMaybe emptyMetadata fromAlonzoMetadata
             in
                case tx ^. Ledger.isPhase2ValidTxL of
                    Ledger.Phase2Valid ->
                        traverseAndTransform fromAlonzoOutput txId meta 0 outs
                    Ledger.Phase2Invalid ->
                        []
        TransactionBabbage tx ->
            let
                body = tx ^. Ledger.bodyTxL
                txId = Ledger.txIdTxBody @BabbageEra body
                outs = body ^. Ledger.outputsTxBodyL
                meta = tx ^. Ledger.auxDataTxL & strictMaybe emptyMetadata fromBabbageMetadata
             in
                case tx ^. Ledger.isPhase2ValidTxL of
                    Ledger.Phase2Valid ->
                        traverseAndTransform fromBabbageOutput txId meta 0 outs
                    Ledger.Phase2Invalid ->
                        -- From Babbage's formal specification:
                        --
                        --   Note that the new collOuts function generates a single output
                        --   with an index |txouts{txb}|.
                        let start = fromIntegral (length outs) in
                        case body ^. Ledger.collateralReturnTxBodyL of
                            SNothing ->
                                []
                            SJust r  ->
                                traverseAndTransform fromBabbageOutput txId meta start (r :<| mempty)
        TransactionConway tx ->
            let
                body = tx ^. Ledger.bodyTxL
                txId = Ledger.txIdTxBody @ConwayEra body
                outs = body ^. Ledger.outputsTxBodyL
                meta = tx ^. Ledger.auxDataTxL & strictMaybe emptyMetadata fromConwayMetadata
             in
                case tx ^. Ledger.isPhase2ValidTxL of
                    Ledger.Phase2Valid ->
                        traverseAndTransform fromConwayOutput txId meta 0 outs
                    Ledger.Phase2Invalid ->
                        -- From Conway formal specification:
                        --
                        --   Note that the new collOuts function generates a single output
                        --   with an index |txouts{txb}|.
                        let start = fromIntegral (length outs) in
                        case body ^. Ledger.collateralReturnTxBodyL of
                            SNothing ->
                                []
                            SJust r  ->
                                traverseAndTransform fromConwayOutput txId meta start (r :<| mempty)
        TransactionDijkstra tx ->
            let
                body = tx ^. Ledger.bodyTxL
                txId = Ledger.txIdTxBody @DijkstraEra body
                outs = body ^. Ledger.outputsTxBodyL
                meta = tx ^. Ledger.auxDataTxL & strictMaybe emptyMetadata identity
             in
                case tx ^. Ledger.isPhase2ValidTxL of
                    Ledger.Phase2Valid ->
                        traverseAndTransform identity txId meta 0 outs
                    Ledger.Phase2Invalid ->
                        let start = fromIntegral (length outs) in
                        case body ^. Ledger.collateralReturnTxBodyL of
                            SNothing ->
                                []
                            SJust r  ->
                                traverseAndTransform identity txId meta start (r :<| mempty)
        SubTransactionDijkstra tx ->
            let
                body = tx ^. Ledger.bodyTxL
                txId = Ledger.txIdTxBody @DijkstraEra body
                outs = body ^. Ledger.outputsTxBodyL
                meta = tx ^. Ledger.auxDataTxL & strictMaybe emptyMetadata identity
             in
                traverseAndTransform identity txId meta 0 outs
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
                    case fn outputRef (transform out) emptyMetadata of
                        Nothing ->
                            results
                        Just result ->
                            result : results

        traverseAndTransform
            :: forall output. ()
            => (output -> Output)
            -> TransactionId
            -> Metadata
            -> OutputIndex
            -> StrictSeq output
            -> [result]
        traverseAndTransform transform txId meta ix = \case
            Empty -> []
            output :<| rest ->
                let
                    outputRef = mkOutputReference txId ix
                    results   = traverseAndTransform transform txId meta (next ix) rest
                 in
                    case fn outputRef (transform output) meta of
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
            fromAlonzoData <$> Ledger.unTxDats (tx ^. Ledger.witsTxL . Ledger.datsTxWitsL)
        TransactionBabbage tx ->
            fromBabbageData <$> Ledger.unTxDats (tx ^. Ledger.witsTxL . Ledger.datsTxWitsL)
        TransactionConway tx ->
            fromConwayData <$> Ledger.unTxDats (tx ^. Ledger.witsTxL . Ledger.datsTxWitsL)
        TransactionDijkstra tx ->
            fromDijkstraData <$> Ledger.unTxDats (tx ^. Ledger.witsTxL . Ledger.datsTxWitsL)
        SubTransactionDijkstra tx ->
            fromDijkstraData <$> Ledger.unTxDats (tx ^. Ledger.witsTxL . Ledger.datsTxWitsL)

    witnessedScripts
        :: Transaction
        -> Map ScriptHash Script
    witnessedScripts = \case
        TransactionByron{} ->
            mempty
        TransactionShelley{} ->
            mempty
        TransactionAllegra tx ->
            ( fromAllegraScript <$> (tx ^. Ledger.witsTxL . Ledger.scriptTxWitsL)
            ) & strictMaybe identity
                    (scriptFromAllegraAuxiliaryData fromAllegraScript)
                    (tx ^. Ledger.auxDataTxL)
        TransactionMary tx ->
            ( fromMaryScript <$> (tx ^. Ledger.witsTxL . Ledger.scriptTxWitsL)
            ) & strictMaybe identity
                    (scriptFromAllegraAuxiliaryData fromMaryScript)
                    (tx ^. Ledger.auxDataTxL)
        TransactionAlonzo tx ->
            ( fromAlonzoScript <$> (tx ^. Ledger.witsTxL . Ledger.scriptTxWitsL)
            ) & strictMaybe identity
                    (scriptFromAlonzoAuxiliaryData @AlonzoEra fromAlonzoScript)
                    (tx ^. Ledger.auxDataTxL)
        TransactionBabbage tx ->
            ( fromBabbageScript <$> (tx ^. Ledger.witsTxL . Ledger.scriptTxWitsL)
            ) & strictMaybe identity
                    (scriptFromAlonzoAuxiliaryData identity)
                    (fromBabbageMetadata <$> tx ^. Ledger.auxDataTxL)
              & scriptsFromOutputs
                    (fromBabbageOutput <$> tx ^. Ledger.bodyTxL . Ledger.outputsTxBodyL)
        TransactionConway tx ->
            ( fromConwayScript <$> tx ^. Ledger.witsTxL . Ledger.scriptTxWitsL
            ) & strictMaybe identity
                    (scriptFromAlonzoAuxiliaryData fromConwayScript)
                    (tx ^. Ledger.auxDataTxL)
              & scriptsFromOutputs
                    (fromConwayOutput <$> tx ^. Ledger.bodyTxL . Ledger.outputsTxBodyL)
        TransactionDijkstra tx ->
            ( tx ^. Ledger.witsTxL . Ledger.scriptTxWitsL
            ) & strictMaybe identity
                    (scriptFromAlonzoAuxiliaryData identity)
                    (tx ^. Ledger.auxDataTxL)
              & scriptsFromOutputs
                    (tx ^. Ledger.bodyTxL . Ledger.outputsTxBodyL)
        SubTransactionDijkstra tx ->
            ( tx ^. Ledger.witsTxL . Ledger.scriptTxWitsL
            ) & strictMaybe identity
                    (scriptFromAlonzoAuxiliaryData identity)
                    (tx ^. Ledger.auxDataTxL)
              & scriptsFromOutputs
                    (tx ^. Ledger.bodyTxL . Ledger.outputsTxBodyL)

    userDefinedMetadata
        :: Transaction
        -> Maybe (MetadataHash, Metadata)
    userDefinedMetadata = \case
        TransactionByron{} ->
            Nothing
        TransactionShelley tx ->
            case tx ^. Ledger.auxDataTxL of
                SNothing ->
                    Nothing
                SJust auxData ->
                    let meta = fromShelleyMetadata auxData
                     in Just (hashMetadata meta, meta)
        TransactionAllegra tx ->
            case tx ^. Ledger.auxDataTxL of
                SNothing ->
                    Nothing
                SJust auxData ->
                    let meta = fromAllegraMetadata auxData
                     in Just (hashMetadata meta, meta)
        TransactionMary tx ->
            case tx ^. Ledger.auxDataTxL of
                SNothing ->
                    Nothing
                SJust auxData ->
                    let meta = fromMaryMetadata auxData
                     in Just (hashMetadata meta, meta)
        TransactionAlonzo tx ->
            case tx ^. Ledger.auxDataTxL of
                SNothing ->
                    Nothing
                SJust auxData ->
                    let meta = fromAlonzoMetadata auxData
                     in Just (hashMetadata meta, meta)
        TransactionBabbage tx ->
            case tx ^. Ledger.auxDataTxL of
                SNothing ->
                    Nothing
                SJust auxData ->
                    let meta = fromBabbageMetadata auxData
                     in Just (hashMetadata meta, meta)
        TransactionConway tx ->
            case tx ^. Ledger.auxDataTxL of
                SNothing ->
                    Nothing
                SJust auxData ->
                    let meta = fromConwayMetadata auxData
                     in Just (hashMetadata meta, meta)
        TransactionDijkstra tx ->
            case tx ^. Ledger.auxDataTxL of
                SNothing ->
                    Nothing
                SJust auxData ->
                    Just (hashMetadata auxData, auxData)
        SubTransactionDijkstra tx ->
            case tx ^. Ledger.auxDataTxL of
                SNothing ->
                    Nothing
                SJust auxData ->
                    Just (hashMetadata auxData, auxData)

    spendRedeemer
        :: Transaction
        -> InputIndex
        -> Maybe BinaryData
    spendRedeemer txInEra ix =
        lookupSpendRedeemer ix =<< case txInEra of
            TransactionByron{} ->
                Nothing
            TransactionShelley{} ->
                Nothing
            TransactionAllegra{} ->
                Nothing
            TransactionMary{} ->
                Nothing
            TransactionAlonzo tx ->
                Just (RedeemersAlonzo (tx ^. Ledger.witsTxL . Ledger.rdmrsTxWitsL))
            TransactionBabbage tx ->
                Just (RedeemersBabbage (tx ^. Ledger.witsTxL . Ledger.rdmrsTxWitsL))
            TransactionConway tx ->
                Just (RedeemersConway (tx ^. Ledger.witsTxL . Ledger.rdmrsTxWitsL))
            TransactionDijkstra tx ->
                Just (RedeemersDijkstra (tx ^. Ledger.witsTxL . Ledger.rdmrsTxWitsL))
            SubTransactionDijkstra tx ->
                Just (RedeemersDijkstra (tx ^. Ledger.witsTxL . Ledger.rdmrsTxWitsL))
