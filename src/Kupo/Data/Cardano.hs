--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kupo.Data.Cardano
    ( -- * Constraints
      Crypto
    , StandardCrypto

      -- * Block
    , IsBlock (..)
    , Block

      -- * TransactionIndex
    , TransactionIndex
    , transactionIndexToJson

      -- * Transaction
    , Transaction

      -- * Metadata
    , Metadata
    , Ledger.Metadatum (..)
    , mkMetadata
    , metadataToText
    , metadataFromText
    , metadataToJson
    , metadataFromJson
    , metadataToJson'

      -- * MetadataHash
    , MetadataHash
    , hashMetadata
    , metadataHashToJson
    , metadataHashToText
    , metadataHashFromText
    , unsafeMetadataHashFromBytes

      -- * TransactionId
    , HasTransactionId (..)
    , TransactionId
    , transactionIdToText
    , transactionIdFromHash
    , transactionIdToBytes
    , unsafeTransactionIdFromBytes
    , transactionIdToJson
    , transactionIdFromText

      -- * OutputIndex
    , OutputIndex
    , getOutputIndex
    , outputIndexToJson
    , outputIndexFromText
    , outputIndexToText

      -- * Input
    , Input
    , OutputReference
    , mkOutputReference
    , withReferences
    , outputReferenceToText
    , outputReferenceFromText

      -- * ExtendedOutputReference
    , ExtendedOutputReference

      -- * Output
    , Output
    , mkOutput
    , getAddress
    , getDatum
    , getValue
    , getScript

      -- * ComparableOutput
    , ComparableOutput
    , fromComparableOutput
    , toComparableOutput

    -- * Value
    , Value
    , hasAssetId
    , hasPolicyId
    , unsafeValueFromList
    , valueToJson
    , assetNameMaxLength

      -- * ComparableValue
    , ComparableValue
    , fromComparableValue
    , toComparableValue

    -- * AssetId
    , AssetId

    -- * PolicyId
    , PolicyId
    , unsafePolicyIdFromBytes
    , policyIdFromText
    , policyIdToText

    -- * AssetName
    , AssetName
    , unsafeAssetNameFromBytes
    , assetNameFromText
    , assetNameToText

    -- * Datum
    , Datum
    , getBinaryData
    , fromBinaryData
    , fromDatumHash
    , noDatum
    , hashDatum

    -- * DatumHash
    , DatumHash
    , datumHashFromText
    , datumHashToText
    , datumHashFromBytes
    , datumHashToBytes
    , unsafeDatumHashFromBytes
    , datumHashToJson

    -- * BinaryData
    , BinaryData
    , hashBinaryData
    , binaryDataToJson
    , binaryDataToBytes
    , binaryDataFromBytes
    , unsafeBinaryDataFromBytes

    -- * ScriptReference
    , ScriptReference (..)
    , mkScriptReference
    , hashScriptReference

    -- * Script
    , Script
    , unsafeScriptFromBytes
    , scriptToBytes
    , scriptFromBytes
    , scriptToJson
    , hashScript
    , fromNativeScript

    -- * NativeScript
    , NativeScript
    , pattern Ledger.RequireSignature
    , pattern Ledger.RequireAllOf
    , pattern Ledger.RequireAnyOf
    , pattern Ledger.RequireMOf
    , pattern Ledger.RequireTimeExpire
    , pattern Ledger.RequireTimeStart

    -- * ScriptHash
    , ScriptHash
    , unsafeScriptHashFromBytes
    , scriptHashFromBytes
    , scriptHashToBytes
    , scriptHashToText
    , scriptHashFromText
    , scriptHashToJson

    -- * KeyHash
    , Ledger.KeyHash (..)

      -- * Address
    , Address
    , unsafeAddressFromBytes
    , addressFromBytes
    , addressToBytes
    , addressToJson
    , isBootstrap
    , getPaymentPartBytes
    , getDelegationPartBytes

      -- * BlockNo
    , BlockNo(..)

      -- * SlotNo
    , SlotNo (..)
    , slotNoFromText
    , slotNoToText
    , slotNoToJson
    , distanceToSlot

      -- * Hash
    , Hash
    , HashAlgorithm (..)
    , hashFromBytes
    , digestSize
    , Blake2b_224
    , Blake2b_256
    , Ledger.unsafeMakeSafeHash

      -- * HeaderHash
    , HeaderHash
    , headerHashToText
    , headerHashFromText
    , headerHashToJson
    , headerHashToBytes
    , unsafeHeaderHashFromBytes

      -- * Point
    , Point
    , pattern GenesisPoint
    , pattern BlockPoint
    , pointFromText
    , pointToJson
    , pointSlot
    , getPointSlotNo
    , getPointHeaderHash
    , unsafeGetPointHeaderHash

      -- * Tip
    , Tip
    , pattern GenesisTip
    , pattern Tip
    , getTipSlotNo
    , distanceToTip

      -- * WithOrigin
    , WithOrigin (..)
    ) where

import Kupo.Prelude

import Cardano.Binary
    ( DecoderError (..)
    , FromCBOR (..)
    , decodeAnnotator
    )
import Cardano.Crypto.Hash
    ( Blake2b_224
    , Blake2b_256
    , Hash
    , HashAlgorithm (..)
    , hashFromBytes
    , hashFromTextAsHex
    , hashToBytesShort
    , pattern UnsafeHash
    , sizeHash
    )
import Cardano.Ledger.Allegra
    ( AllegraEra
    )
import Cardano.Ledger.Alonzo
    ( AlonzoEra
    )
import Cardano.Ledger.Babbage
    ( BabbageEra
    )
import Cardano.Ledger.Crypto
    ( Crypto
    , StandardCrypto
    )
import Cardano.Ledger.Mary
    ( MaryEra
    )
import Cardano.Ledger.Shelley
    ( ShelleyEra
    )
import Cardano.Ledger.Val
    ( Val (inject)
    )
import Cardano.Slotting.Block
    ( BlockNo (..)
    )
import Cardano.Slotting.Slot
    ( SlotNo (..)
    )
import Control.Arrow
    ( left
    )
import Data.Aeson
    ( (.:)
    )
import Data.Binary.Put
    ( runPut
    )
import Data.ByteString.Bech32
    ( HumanReadablePart (..)
    , encodeBech32
    )
import Data.Maybe.Strict
    ( StrictMaybe (..)
    , maybeToStrictMaybe
    , strictMaybe
    , strictMaybeToMaybe
    )
import Data.Sequence.Strict
    ( StrictSeq
    , pattern (:<|)
    , pattern Empty
    )
import GHC.Records
    ( HasField (..)
    )
import Ouroboros.Consensus.Block
    ( ConvertRawHash (..)
    )
import Ouroboros.Consensus.Byron.Ledger.Mempool
    ( GenTx (..)
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock
    , HardForkBlock (..)
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( OneEraHash (..)
    )
import Ouroboros.Consensus.Ledger.SupportsMempool
    ( HasTxs (extractTxs)
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    )
import Ouroboros.Consensus.Util
    ( eitherToMaybe
    )
import Ouroboros.Network.Block
    ( HeaderHash
    , blockPoint
    , pattern BlockPoint
    , pattern GenesisPoint
    , pointSlot
    )
import Ouroboros.Network.Point
    ( WithOrigin (..)
    )

import Ouroboros.Consensus.Cardano
    ()
import Ouroboros.Consensus.Protocol.Praos.Translate
    ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol
    ()

import qualified Cardano.Binary as Cbor
import qualified Cardano.Chain.Common as Ledger.Byron
import qualified Cardano.Chain.UTxO as Ledger.Byron
import qualified Cardano.Crypto as Ledger.Byron
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.Language as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.Tx as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxSeq as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Cardano.Ledger.Babbage.TxBody as Ledger.Babbage
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Ledger.Core
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Era as Ledger.Era
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.Shelley.BlockChain as Ledger.Shelley
import qualified Cardano.Ledger.Shelley.Metadata as Ledger
import qualified Cardano.Ledger.Shelley.Tx as Ledger.Shelley
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as Ledger.MaryAllegra
import qualified Cardano.Ledger.ShelleyMA.Timelocks as Ledger
import qualified Cardano.Ledger.ShelleyMA.Timelocks as Ledger.MaryAllegra
import qualified Cardano.Ledger.ShelleyMA.TxBody as Ledger.MaryAllegra
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Codec.CBOR.Read as Cbor
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Key as Json
import qualified Data.Aeson.Key as Json.Key
import qualified Data.Aeson.KeyMap as Json.KeyMap
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Ouroboros.Network.Block as Ouroboros

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

-- Metadata

type Metadata =
    Ledger.Metadata (BabbageEra StandardCrypto)

type Metadatum =
    Ledger.Metadatum

mkMetadata :: Map Word64 Metadatum -> (MetadataHash, Metadata)
mkMetadata (Ledger.Metadata -> meta) =
    (hashMetadata meta, meta)

metadataToText :: Metadata -> Text
metadataToText =
    encodeBase16 . Ledger.originalBytes
{-# INLINABLE metadataToText #-}

metadataFromText :: Text -> Maybe Metadata
metadataFromText txt = do
    bytes <- eitherToMaybe $ decodeBase16 (encodeUtf8 txt)
    eitherToMaybe $ decodeAnnotator "Metadata" fromCBOR (toLazy bytes)

metadataToJson :: Metadata -> Json.Encoding
metadataToJson (Ledger.Metadata meta) =
    encodeMap show encodeMetadatum meta
  where
    encodeMetadatum :: Ledger.Metadatum -> Json.Encoding
    encodeMetadatum = \case
        Ledger.I n ->
            encodeObject [("int", Json.integer n)]
        Ledger.S txt ->
            encodeObject [("string", Json.text txt)]
        Ledger.B bytes ->
            encodeObject [("bytes", Json.text $ encodeBase16 bytes)]
        Ledger.List xs ->
            encodeObject [("list", Json.list encodeMetadatum xs)]
        Ledger.Map xs ->
            encodeObject [("map", Json.list encodeKeyPair xs)]

    encodeKeyPair :: (Ledger.Metadatum, Ledger.Metadatum) -> Json.Encoding
    encodeKeyPair (k, v) =
        encodeObject
            [ ( "k", encodeMetadatum k )
            , ( "v", encodeMetadatum v )
            ]

metadataFromJson :: Json.Value -> Json.Parser (MetadataHash, Metadata)
metadataFromJson = fmap mkMetadata . Json.withObject "Metadata"
    (Json.KeyMap.foldrWithKey
        (\k v accum -> Map.insert
            <$> metadatumLabelFromJson k
            <*> metadatumFromJson v
            <*> accum
        )
        (pure Map.empty)
    )
  where
    metadatumLabelFromJson :: Json.Key -> Json.Parser Word64
    metadatumLabelFromJson key = do
        (lbl, remLbl) <- either fail pure (T.decimal $ Json.Key.toText key)
        guard (T.null remLbl)
        pure lbl

    metadatumFromJson :: Json.Value -> Json.Parser Metadatum
    metadatumFromJson = Json.withObject "Metadatum" $ \o -> asum
        [ metadatumIntFromJson o
        , metadatumStringFromJson o
        , metadatumBytesFromJson o
        , metadatumListFromJson o
        , metadatumMapFromJson o
        ]
      where
        metadatumIntFromJson :: Json.Object -> Json.Parser Metadatum
        metadatumIntFromJson o = do
            i <- o .: "int"
            pure (Ledger.I i)

        metadatumStringFromJson :: Json.Object -> Json.Parser Metadatum
        metadatumStringFromJson o = do
            s <- o .: "string"
            pure (Ledger.S s)

        metadatumBytesFromJson :: Json.Object -> Json.Parser Metadatum
        metadatumBytesFromJson o = do
            b <- (o .: "bytes")
              >>= either (fail . toString) pure . decodeBase16 . encodeUtf8 @Text
            pure (Ledger.B b)

        metadatumListFromJson :: Json.Object -> Json.Parser Metadatum
        metadatumListFromJson o = do
            xs <- (o .: "list") >>= traverse metadatumFromJson
            pure (Ledger.List xs)

        metadatumMapFromJson :: Json.Object -> Json.Parser Metadatum
        metadatumMapFromJson o = do
            xs <- (o .: "map") >>= traverse
                (Json.withObject "map" $ \m -> do
                    k <- m .: "k"
                    v <- m .: "v"
                    (,) <$> metadatumFromJson k <*> metadatumFromJson v
                )
            pure (Ledger.Map xs)

metadataToJson' :: (MetadataHash, Metadata) -> Json.Encoding
metadataToJson' (hash, meta) =
    encodeObject
        [ ("hash", metadataHashToJson hash)
        , ("raw", Json.text (metadataToText meta))
        , ("schema", metadataToJson meta)
        ]

-- MetadataHash

type MetadataHash =
    Ledger.SafeHash StandardCrypto Ledger.EraIndependentAuxiliaryData

hashMetadata :: Metadata -> MetadataHash
hashMetadata = Ledger.hashMetadata
{-# INLINABLE hashMetadata #-}

unsafeMetadataHashFromBytes :: HasCallStack => ByteString -> MetadataHash
unsafeMetadataHashFromBytes =
    Ledger.unsafeMakeSafeHash
    . fromMaybe (error "unsafeMetadataHashFromBytes")
    . hashFromBytes @Blake2b_256
{-# INLINABLE unsafeMetadataHashFromBytes #-}

metadataHashToText :: MetadataHash -> Text
metadataHashToText =
    encodeBase16 . Ledger.originalBytes . Ledger.extractHash
{-# INLINABLE metadataHashToText #-}

metadataHashFromText :: Text -> Maybe MetadataHash
metadataHashFromText =
    fmap Ledger.unsafeMakeSafeHash . hashFromTextAsHex @Blake2b_256
{-# INLINABLE metadataHashFromText #-}

metadataHashToJson :: MetadataHash -> Json.Encoding
metadataHashToJson =
    hashToJson . Ledger.extractHash
{-# INLINABLE metadataHashToJson #-}

-- Block

type Block =
    Block' StandardCrypto

type Block' crypto =
    CardanoBlock crypto

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
                        []
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

-- TransactionId

type TransactionId =
    TransactionId' StandardCrypto

type TransactionId' crypto =
    Ledger.TxId crypto

transactionIdFromByron
    :: Ledger.Byron.TxId
    -> TransactionId
transactionIdFromByron (Ledger.Byron.hashToBytes -> bytes) =
    Ledger.TxId (Ledger.unsafeMakeSafeHash (UnsafeHash (toShort bytes)))

transactionIdFromHash
    :: Hash Blake2b_256 Ledger.EraIndependentTxBody
    -> TransactionId
transactionIdFromHash =
    Ledger.TxId . Ledger.unsafeMakeSafeHash
{-# INLINABLE transactionIdFromHash #-}

transactionIdToBytes :: TransactionId -> ByteString
transactionIdToBytes =
    (\(UnsafeHash h) -> fromShort h) . Ledger.extractHash . Ledger._unTxId
{-# INLINABLE transactionIdToBytes #-}

unsafeTransactionIdFromBytes
    :: HasCallStack
    => ByteString
    -> TransactionId
unsafeTransactionIdFromBytes =
    transactionIdFromHash
    . UnsafeHash
    . toShort
    . sizeInvariant (== (digestSize @Blake2b_256))
{-# INLINABLE unsafeTransactionIdFromBytes #-}

class HasTransactionId (a :: Type) (crypto :: Type) where
    getTransactionId
        :: a
        -> TransactionId' crypto

transactionIdToText :: TransactionId -> Text
transactionIdToText =
    encodeBase16 . (\(UnsafeHash h) -> fromShort h) . Ledger.extractHash . Ledger._unTxId
{-# INLINABLE transactionIdToText #-}

transactionIdFromText
    :: Text
    -> Maybe TransactionId
transactionIdFromText =
    fmap transactionIdFromHash . hashFromTextAsHex @Blake2b_256
{-# INLINABLE transactionIdFromText #-}

transactionIdToJson :: TransactionId -> Json.Encoding
transactionIdToJson =
    hashToJson . Ledger.extractHash . Ledger._unTxId
{-# INLINABLE transactionIdToJson #-}

-- OutputIndex

type OutputIndex = Word16

getOutputIndex :: OutputReference' crypto -> OutputIndex
getOutputIndex (Ledger.TxIn _ (Ledger.TxIx ix)) =
    fromIntegral ix
{-# INLINABLE getOutputIndex #-}

outputIndexToJson :: OutputIndex -> Json.Encoding
outputIndexToJson =
    Json.integer . toInteger
{-# INLINABLE outputIndexToJson #-}

outputIndexFromText :: Text -> Maybe OutputIndex
outputIndexFromText txt = do
    (ix, remIx) <- either (const Nothing) Just (T.decimal txt)
    guard (T.null remIx)
    pure (fromInteger ix)
{-# INLINABLE outputIndexFromText #-}

outputIndexToText :: OutputIndex -> Text
outputIndexToText = show
{-# INLINABLE outputIndexToText #-}


-- Transaction

type Transaction = Transaction' StandardCrypto

data Transaction' crypto
    = TransactionByron
        !Ledger.Byron.Tx
        !Ledger.Byron.TxId
    | TransactionShelley
        !(Ledger.Shelley.Tx (ShelleyEra crypto))
    | TransactionAllegra
        !(Ledger.Shelley.Tx (AllegraEra crypto))
    | TransactionMary
        !(Ledger.Shelley.Tx (MaryEra crypto))
    | TransactionAlonzo
        !(Ledger.Alonzo.ValidatedTx (AlonzoEra crypto))
    | TransactionBabbage
        !(Ledger.Alonzo.ValidatedTx (BabbageEra crypto))

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

-- Input

type Input =
    Input' StandardCrypto

type Input' crypto =
    Ledger.TxIn crypto

-- OutputReference

type OutputReference =
    OutputReference' StandardCrypto

type OutputReference' crypto =
    Input' crypto

mkOutputReference
    :: TransactionId
    -> OutputIndex
    -> OutputReference
mkOutputReference i =
    Ledger.TxIn i . Ledger.TxIx . fromIntegral
{-# INLINABLE mkOutputReference #-}

withReferences
    :: TransactionId
    -> [Output]
    -> [(OutputReference, Output)]
withReferences txId = loop 0
  where
    loop ix = \case
        [] -> []
        out:rest ->
            let
                results = loop (next ix) rest
             in
                (mkOutputReference txId ix, out) : results

instance HasTransactionId (Ledger.TxIn crypto) crypto where
    getTransactionId (Ledger.TxIn i _) = i

outputReferenceFromText :: Text -> Maybe OutputReference
outputReferenceFromText txt =
    case T.splitOn "@" txt of
        [outputIndex, txId] -> do
            mkOutputReference
                <$> transactionIdFromText txId
                <*> outputIndexFromText outputIndex
        _malformedTextString ->
            Nothing

outputReferenceToText :: OutputReference -> Text
outputReferenceToText outRef =
    outputIndexToText (getOutputIndex outRef)
    <>
    "@"
    <>
    transactionIdToText (getTransactionId outRef)

-- ExtendedOutputReference

type ExtendedOutputReference =
    (OutputReference, TransactionIndex)

-- TransactionIndex

type TransactionIndex = Word16

transactionIndexToJson :: TransactionIndex -> Json.Encoding
transactionIndexToJson = toEncoding

-- Output

type Output =
    Output' StandardCrypto

type Output' crypto =
    Ledger.Babbage.TxOut (BabbageEra crypto)

mkOutput
    :: Address
    -> Value
    -> Datum
    -> Maybe Script
    -> Output
mkOutput address value datum script =
    Ledger.Babbage.TxOut
        address
        value
        datum
        (maybeToStrictMaybe script)
{-# INLINABLE mkOutput #-}

fromByronOutput
    :: forall crypto.
        ( Crypto crypto
        )
    => Ledger.Byron.TxOut
    -> Ledger.Core.TxOut (BabbageEra crypto)
fromByronOutput (Ledger.Byron.TxOut address value) =
    Ledger.Babbage.TxOut
        (Ledger.AddrBootstrap (Ledger.BootstrapAddress address))
        (inject $ Ledger.Coin $ toInteger $ Ledger.Byron.unsafeGetLovelace value)
        Ledger.Babbage.NoDatum
        SNothing
{-# INLINABLE fromByronOutput #-}

fromShelleyOutput
    :: forall (era :: Type -> Type) crypto.
        ( Ledger.Era.Era (era crypto)
        , Ledger.Era.Crypto (era crypto) ~ crypto
        , Ledger.Core.TxOut (era crypto) ~ Ledger.Shelley.TxOut (era crypto)
        , Show (Ledger.Core.Value (era crypto))
        )
    => (Ledger.Core.Value (era crypto) -> Ledger.Value crypto)
    -> Ledger.Core.TxOut (era crypto)
    -> Ledger.Core.TxOut (BabbageEra crypto)
fromShelleyOutput liftValue (Ledger.Shelley.TxOut addr value) =
    Ledger.Babbage.TxOut addr (liftValue value) Ledger.Babbage.NoDatum SNothing
{-# INLINABLE fromShelleyOutput #-}

fromAlonzoOutput
    :: forall crypto.
        ( Crypto crypto
        )
    => Ledger.Core.TxOut (AlonzoEra crypto)
    -> Ledger.Core.TxOut (BabbageEra crypto)
fromAlonzoOutput (Ledger.Alonzo.TxOut addr value datum) =
    case datum of
        SNothing ->
            Ledger.Babbage.TxOut
                addr
                value
                Ledger.Babbage.NoDatum
                SNothing

        SJust datumHash ->
            Ledger.Babbage.TxOut
                addr
                value
                (Ledger.Babbage.DatumHash datumHash)
                SNothing

getAddress
    :: Output
    -> Address
getAddress (Ledger.Babbage.TxOut address _value _datum _refScript) =
    address
{-# INLINABLE getAddress #-}

getValue
    :: Output
    -> Value
getValue (Ledger.Babbage.TxOut _address value _datum _refScript) =
    value
{-# INLINABLE getValue #-}

getDatum
    :: Output
    -> Datum
getDatum (Ledger.Babbage.TxOut _address _value datum _refScript) =
    datum
{-# INLINABLE getDatum #-}

getScript
    :: Output
    -> Maybe Script
getScript (Ledger.Babbage.TxOut _address _value _datum refScript) =
    strictMaybeToMaybe refScript
{-# INLINABLE getScript #-}

-- ComparableOutput

data ComparableOutput = ComparableOutput
    { comparableOutputAddress :: !Address
    , comparableOutputValue :: !ComparableValue
    , comparableOutputDatum :: !Datum
    , comparableOutputScript :: !(Maybe Script)
    } deriving (Generic, Eq, Show, Ord)

toComparableOutput
    :: Output
    -> ComparableOutput
toComparableOutput out = ComparableOutput
    { comparableOutputAddress = getAddress out
    , comparableOutputValue = toComparableValue (getValue out)
    , comparableOutputDatum = getDatum out
    , comparableOutputScript = getScript out
    }

fromComparableOutput
    :: ComparableOutput
    -> Output
fromComparableOutput (ComparableOutput addr val datum script) =
    mkOutput addr (fromComparableValue val) datum script

-- ScriptReference

data ScriptReference
    = NoScript
    | ReferencedScript !ScriptHash
    | InlineScript !Script
    deriving (Eq, Show)

mkScriptReference
    :: Maybe Script
    -> ScriptReference
mkScriptReference =
    maybe NoScript InlineScript

hashScriptReference
    :: ScriptReference
    -> Maybe ScriptHash
hashScriptReference = \case
    NoScript ->
        Nothing
    ReferencedScript hash ->
        Just hash
    InlineScript script ->
        Just (hashScript script)

-- Script

type Script =
    Ledger.Script (BabbageEra StandardCrypto)

scriptFromAllegraAuxiliaryData
    :: forall era. (Ledger.Core.Script era ~ Ledger.Timelock StandardCrypto)
    => (Ledger.Core.Script era -> Script)
    -> Ledger.MaryAllegra.AuxiliaryData era
    -> Map ScriptHash Script
    -> Map ScriptHash Script
scriptFromAllegraAuxiliaryData liftScript (Ledger.MaryAllegra.AuxiliaryData _ scripts) m0 =
    foldr
        (\(liftScript -> s) -> Map.insert (hashScript s) s)
        m0
        scripts
{-# INLINABLE scriptFromAllegraAuxiliaryData #-}

scriptFromAlonzoAuxiliaryData
    :: forall era.
        ( Ledger.Era era
        , Ledger.Core.Script era ~ Ledger.Script era
        )
    => (Ledger.Script era -> Script)
    -> Ledger.AuxiliaryData era
    -> Map ScriptHash Script
    -> Map ScriptHash Script
scriptFromAlonzoAuxiliaryData liftScript Ledger.AuxiliaryData{Ledger.scripts} m0 =
    foldr
        (\(liftScript -> s) -> Map.insert (hashScript s) s)
        m0
        scripts
{-# INLINABLE scriptFromAlonzoAuxiliaryData #-}

fromAllegraScript
    :: Ledger.MaryAllegra.Timelock StandardCrypto
    -> Script
fromAllegraScript =
    Ledger.TimelockScript
{-# INLINABLE fromAllegraScript #-}

fromMaryScript
    :: Ledger.MaryAllegra.Timelock StandardCrypto
    -> Script
fromMaryScript =
    Ledger.TimelockScript
{-# INLINABLE fromMaryScript  #-}

fromAlonzoScript
    :: Ledger.Script (AlonzoEra StandardCrypto)
    -> Script
fromAlonzoScript = \case
    Ledger.TimelockScript script ->
        Ledger.TimelockScript script
    Ledger.PlutusScript lang bytes ->
        Ledger.PlutusScript lang bytes

fromBabbageScript
    :: Ledger.Script (BabbageEra StandardCrypto)
    -> Script
fromBabbageScript =
    identity
{-# INLINABLE fromBabbageScript #-}

scriptToJson
    :: Script
    -> Json.Encoding
scriptToJson script = encodeObject
    [ ("script", encodeBytes (Ledger.originalBytes script))
    , ("language", case script of
        Ledger.TimelockScript{} ->
          Json.text "native"
        Ledger.PlutusScript Ledger.PlutusV1 _ ->
          Json.text "plutus:v1"
        Ledger.PlutusScript Ledger.PlutusV2 _ ->
          Json.text "plutus:v2"
      )
    ]

scriptToBytes
    :: Script
    -> ByteString
scriptToBytes = \case
    script@Ledger.TimelockScript{} ->
        BS.singleton 0 <> Ledger.originalBytes script
    script@(Ledger.PlutusScript Ledger.PlutusV1 _) ->
        BS.singleton 1 <> Ledger.originalBytes script
    script@(Ledger.PlutusScript Ledger.PlutusV2 _) ->
        BS.singleton 2 <> Ledger.originalBytes script

unsafeScriptFromBytes
    :: HasCallStack
    => ByteString
    -> Script
unsafeScriptFromBytes =
    fromMaybe (error "unsafeScriptFromBytes") . scriptFromBytes
{-# INLINABLE unsafeScriptFromBytes #-}

scriptFromBytes
    :: ByteString
    -> Maybe Script
scriptFromBytes (toLazy -> bytes) =
    eitherToMaybe $ do
        (script, tag) <- left (DecoderErrorDeserialiseFailure "Script") $
            Cbor.deserialiseFromBytes Cbor.decodeWord8 bytes
        case tag of
            0 -> Ledger.TimelockScript <$> decodeAnnotator "Timelock" fromCBOR script
            1 -> pure $ Ledger.PlutusScript Ledger.PlutusV1 (toShort $ toStrict script)
            2 -> pure $ Ledger.PlutusScript Ledger.PlutusV2 (toShort $ toStrict script)
            t -> Left (DecoderErrorUnknownTag "Script" t)

fromNativeScript
    :: NativeScript
    -> Script
fromNativeScript =
    Ledger.TimelockScript
{-# INLINABLE fromNativeScript #-}

hashScript
    :: Script
    -> ScriptHash
hashScript =
    Ledger.hashScript @(BabbageEra StandardCrypto)
{-# INLINABLE hashScript #-}

-- NativeScript

type NativeScript = Ledger.Timelock StandardCrypto

-- ScriptHash

type ScriptHash =
    Ledger.ScriptHash StandardCrypto

unsafeScriptHashFromBytes
    :: HasCallStack
    => ByteString
    -> ScriptHash
unsafeScriptHashFromBytes =
    fromMaybe (error "unsafeScriptFromBytes") . scriptHashFromBytes
{-# INLINABLE unsafeScriptHashFromBytes #-}

scriptHashFromBytes
    :: ByteString
    -> Maybe ScriptHash
scriptHashFromBytes bytes
    | BS.length bytes == digestSize @Blake2b_224 =
        Just $ Ledger.ScriptHash (UnsafeHash (toShort bytes))
    | otherwise =
        Nothing

scriptHashToBytes
    :: ScriptHash
    -> ByteString
scriptHashToBytes (Ledger.ScriptHash (UnsafeHash scriptHash)) =
    fromShort scriptHash
{-# INLINABLE scriptHashToBytes #-}

scriptHashToText
    :: ScriptHash
    -> Text
scriptHashToText (Ledger.ScriptHash (UnsafeHash scriptHash)) =
    encodeBase16 (fromShort scriptHash)
{-# INLINABLE scriptHashToText #-}

scriptHashFromText
    :: Text
    -> Maybe ScriptHash
scriptHashFromText txt =
    case decodeBase16 (encodeUtf8 txt) of
        Right bytes ->
            scriptHashFromBytes bytes
        Left{} ->
            Nothing

scriptHashToJson
    :: ScriptHash
    -> Json.Encoding
scriptHashToJson =
    Json.text . scriptHashToText
{-# INLINABLE scriptHashToJson #-}

-- Datum

type Datum =
    Ledger.Datum (BabbageEra StandardCrypto)

getBinaryData
    :: Datum
    -> Maybe BinaryData
getBinaryData = \case
    Ledger.Datum bin -> Just bin
    Ledger.NoDatum -> Nothing
    Ledger.DatumHash{} -> Nothing

noDatum
    :: Datum
noDatum =
    Ledger.NoDatum
{-# INLINEABLE noDatum #-}

fromDatumHash
    :: DatumHash
    -> Datum
fromDatumHash =
    Ledger.DatumHash
{-# INLINEABLE fromDatumHash #-}

fromBinaryData
    :: BinaryData
    -> Datum
fromBinaryData =
    Ledger.Datum
{-# INLINEABLE fromBinaryData #-}

hashDatum
    :: Datum
    -> Maybe DatumHash
hashDatum =
    strictMaybeToMaybe . Ledger.datumDataHash
{-# INLINABLE hashDatum #-}

-- DatumHash

type DatumHash =
    DatumHash' StandardCrypto

type DatumHash' crypto =
    Ledger.DataHash crypto

datumHashToBytes
    :: DatumHash
    -> ByteString
datumHashToBytes =
    Ledger.originalBytes
{-# INLINABLE datumHashToBytes #-}

datumHashFromBytes
    :: ByteString
    -> Maybe DatumHash
datumHashFromBytes bytes
    | BS.length bytes == (digestSize @Blake2b_256) =
        Just (unsafeDatumHashFromBytes bytes)
    | otherwise =
        Nothing

datumHashToText
    :: DatumHash
    -> Text
datumHashToText =
    encodeBase16 . Ledger.originalBytes
{-# INLINABLE datumHashToText #-}

datumHashFromText
    :: Text
    -> Maybe DatumHash
datumHashFromText str =
    case datumHashFromBytes <$> decodeBase16 (encodeUtf8 str) of
        Right (Just hash) -> Just hash
        Right Nothing -> Nothing
        Left{} -> Nothing

unsafeDatumHashFromBytes
    :: forall crypto.
        ( HasCallStack
        , Crypto crypto
        )
    => ByteString
    -> DatumHash' crypto
unsafeDatumHashFromBytes =
    Ledger.unsafeMakeSafeHash
    . UnsafeHash
    . toShort
    . sizeInvariant (== (digestSize @Blake2b_256))
{-# INLINABLE unsafeDatumHashFromBytes #-}

datumHashToJson
    :: DatumHash
    -> Json.Encoding
datumHashToJson =
    hashToJson . Ledger.extractHash
{-# INLINABLE datumHashToJson #-}

-- BinaryData

type BinaryData =
    Ledger.BinaryData (BabbageEra StandardCrypto)

type BinaryDataHash =
    DatumHash

hashBinaryData
    :: BinaryData
    -> BinaryDataHash
hashBinaryData  =
    Ledger.hashBinaryData
{-# INLINEABLE hashBinaryData #-}

binaryDataToJson
    :: BinaryData
    -> Json.Encoding
binaryDataToJson =
    Json.text . encodeBase16 . Ledger.originalBytes
{-# INLINABLE binaryDataToJson #-}

binaryDataToBytes
    :: BinaryData
    -> ByteString
binaryDataToBytes =
    Ledger.originalBytes . Ledger.binaryDataToData
{-# INLINABLE binaryDataToBytes #-}

binaryDataFromBytes
    :: ByteString
    -> Maybe BinaryData
binaryDataFromBytes =
    either (const Nothing) Just . Ledger.makeBinaryData . toShort
{-# INLINABLE binaryDataFromBytes #-}

unsafeBinaryDataFromBytes
    :: HasCallStack
    => ByteString
    -> BinaryData
unsafeBinaryDataFromBytes =
    either (error . toText) identity . Ledger.makeBinaryData . toShort
{-# INLINABLE unsafeBinaryDataFromBytes #-}

fromAlonzoData
    :: Ledger.Data (AlonzoEra StandardCrypto)
    -> BinaryData
fromAlonzoData =
    Ledger.dataToBinaryData . coerce
{-# INLINEABLE fromAlonzoData #-}

fromBabbageData
    :: Ledger.Data (BabbageEra StandardCrypto)
    -> BinaryData
fromBabbageData =
    Ledger.dataToBinaryData
{-# INLINEABLE fromBabbageData #-}

-- Value

type Value =
    Value' StandardCrypto

type Value' crypto =
    Ledger.Value crypto

hasAssetId :: Value -> AssetId -> Bool
hasAssetId value (policyId, assetName) =
    Ledger.lookup policyId assetName value > 0

hasPolicyId :: Value -> PolicyId -> Bool
hasPolicyId value policyId =
    policyId `Set.member` Ledger.policies value

unsafeValueFromList
    :: Integer
    -> [(ByteString, ByteString, Integer)]
    -> Value
unsafeValueFromList ada assets =
    Ledger.valueFromList
        ada
        [ ( unsafePolicyIdFromBytes pid, unsafeAssetNameFromBytes name, q)
        | (pid, name, q) <- assets
        ]

valueToJson :: Value -> Json.Encoding
valueToJson (Ledger.Value coins assets) = Json.pairs $ mconcat
    [ Json.pair "coins"  (Json.integer coins)
    , Json.pair "assets" (assetsToJson assets)
    ]
  where
    assetsToJson :: Map (Ledger.PolicyID StandardCrypto) (Map Ledger.AssetName Integer) -> Json.Encoding
    assetsToJson =
        Json.pairs
        .
        Map.foldrWithKey
            (\k v r -> Json.pair (assetIdToKey k) (Json.integer v) <> r)
            mempty
        .
        flatten

    flatten :: (Ord k1, Ord k2) => Map k1 (Map k2 a) -> Map (k1, k2) a
    flatten = Map.foldrWithKey
        (\k inner -> Map.union (Map.mapKeys (k,) inner))
        mempty

    assetIdToKey :: (Ledger.PolicyID StandardCrypto, Ledger.AssetName) -> Json.Key
    assetIdToKey (Ledger.PolicyID (Ledger.ScriptHash (UnsafeHash pid)), Ledger.AssetName bytes)
        | SBS.null bytes = Json.fromText
            (encodeBase16 (fromShort pid))
        | otherwise     = Json.fromText
            (encodeBase16 (fromShort pid) <> "." <> encodeBase16 (fromShort bytes))

-- ComparableValue

data ComparableValue = ComparableValue
    { comparableValueAda :: !Integer
    , comparableValueAssets :: !(Map PolicyId (Map AssetName Integer))
    } deriving (Generic, Eq, Show, Ord)

fromComparableValue :: ComparableValue -> Value
fromComparableValue (ComparableValue ada assets) =
    Ledger.Value ada assets

toComparableValue :: Value -> ComparableValue
toComparableValue (Ledger.Value ada assets) =
    ComparableValue ada assets

-- AssetId

type AssetId = (PolicyId, Ledger.AssetName)

-- PolicyId

type PolicyId = Ledger.PolicyID StandardCrypto

unsafePolicyIdFromBytes :: ByteString -> PolicyId
unsafePolicyIdFromBytes =
    maybe (error "unsafePolicyIdFromBytes") Ledger.PolicyID . scriptHashFromBytes
{-# INLINABLE unsafePolicyIdFromBytes #-}

policyIdFromText :: Text -> Maybe PolicyId
policyIdFromText =
    fmap Ledger.PolicyID . scriptHashFromText
{-# INLINABLE policyIdFromText #-}

policyIdToText :: PolicyId -> Text
policyIdToText =
    scriptHashToText . Ledger.policyID
{-# INLINABLE policyIdToText #-}

-- AssetName

type AssetName = Ledger.AssetName

assetNameMaxLength :: Int
assetNameMaxLength = 32
{-# INLINABLE assetNameMaxLength #-}

unsafeAssetNameFromBytes :: ByteString -> Ledger.AssetName
unsafeAssetNameFromBytes =
    Ledger.AssetName
    . toShort
    . sizeInvariant (<= assetNameMaxLength)
{-# INLINABLE unsafeAssetNameFromBytes #-}

assetNameFromText :: Text -> Maybe AssetName
assetNameFromText (encodeUtf8 -> bytes) = do
    -- NOTE: assuming base16 encoding, hence '2 *'
    guard (BS.length bytes <= 2 * assetNameMaxLength)
    unsafeAssetNameFromBytes <$> eitherToMaybe (decodeBase16 bytes)

assetNameToText :: AssetName -> Text
assetNameToText =
    encodeBase16 . fromShort . Ledger.assetName
{-# INLINABLE assetNameToText #-}

-- Address

type Address =
    Address' StandardCrypto

type Address' crypto =
    Ledger.Addr crypto

addressToJson :: Address -> Json.Encoding
addressToJson = \case
    addr@Ledger.AddrBootstrap{} ->
        (Json.text . encodeBase58 . addressToBytes) addr
    addr@(Ledger.Addr network _ _) ->
        (Json.text . encodeBech32 (hrp network) . addressToBytes) addr
  where
    hrp = \case
        Ledger.Mainnet -> HumanReadablePart "addr"
        Ledger.Testnet -> HumanReadablePart "addr_test"

addressToBytes :: Address -> ByteString
addressToBytes =
    Ledger.serialiseAddr
{-# INLINABLE addressToBytes #-}

unsafeAddressFromBytes :: HasCallStack => ByteString -> Address
unsafeAddressFromBytes =
    fromMaybe (error "unsafeAddressFromBytes") . addressFromBytes
{-# INLINABLE unsafeAddressFromBytes #-}

addressFromBytes :: ByteString -> Maybe Address
addressFromBytes =
    Ledger.deserialiseAddr
{-# INLINABLE addressFromBytes #-}

isBootstrap :: Address -> Bool
isBootstrap = \case
    Ledger.AddrBootstrap{} -> True
    Ledger.Addr{} -> False

getPaymentPartBytes :: Address -> Maybe ByteString
getPaymentPartBytes = \case
    Ledger.Addr _ payment _ ->
        Just $ toStrict $ runPut $ Ledger.putCredential payment
    Ledger.AddrBootstrap{} ->
        Nothing

getDelegationPartBytes :: Address -> Maybe ByteString
getDelegationPartBytes = \case
    Ledger.Addr _ _ (Ledger.StakeRefBase delegation) ->
        Just $ toStrict $ runPut $ Ledger.putCredential delegation
    Ledger.Addr{} ->
        Nothing
    Ledger.AddrBootstrap{} ->
        Nothing

-- HeaderHash

-- | Deserialise a 'HeaderHash' from a base16-encoded text string.
headerHashFromText
    :: Text
    -> Maybe (HeaderHash Block)
headerHashFromText =
    fmap (OneEraHash . hashToBytesShort) . hashFromTextAsHex @Blake2b_256
{-# INLINABLE headerHashFromText #-}

headerHashToText
    :: HeaderHash Block
    -> Text
headerHashToText =
    encodeBase16 . headerHashToBytes
{-# INLINABLE headerHashToText #-}

headerHashToBytes
    :: HeaderHash Block
    -> ByteString
headerHashToBytes =
    fromShort . toShortRawHash (Proxy @Block)
{-# INLINABLE headerHashToBytes #-}

headerHashToJson
    :: HeaderHash Block
    -> Json.Encoding
headerHashToJson =
    encodeBytes . headerHashToBytes
{-# INLINABLE headerHashToJson #-}

unsafeHeaderHashFromBytes
    :: ByteString
    -> HeaderHash Block
unsafeHeaderHashFromBytes =
    fromRawHash (Proxy @Block)
{-# INLINABLE unsafeHeaderHashFromBytes #-}

-- Tip

type Tip = Ouroboros.Tip Block
{-# COMPLETE GenesisTip, Tip #-}

pattern GenesisTip :: Tip
pattern GenesisTip <-
    Ouroboros.TipGenesis
  where
    GenesisTip =
        Ouroboros.TipGenesis

pattern Tip :: SlotNo -> HeaderHash Block -> BlockNo -> Tip
pattern Tip s h b <-
    Ouroboros.Tip s h b
  where
    Tip =
        Ouroboros.Tip

getTipSlotNo :: Tip -> SlotNo
getTipSlotNo tip =
    case Ouroboros.getTipSlotNo tip of
        Origin -> SlotNo 0
        At sl  -> sl

distanceToTip :: Tip -> SlotNo -> Word64
distanceToTip =
    distanceToSlot . getTipSlotNo
{-# INLINABLE distanceToTip #-}

-- Point

type Point = Ouroboros.Point Block

-- | Parse a 'Point' from a text string. This alternatively tries two patterns:
--
-- - "origin"        → for a points that refers to the beginning of the blockchain
--
-- - "N.hhhh...hhhh" → A dot-separated integer and base16-encoded digest, which
--                     refers to a specific point on chain identified by this
--                     slot number and header hash.
--
pointFromText :: Text -> Maybe (Point)
pointFromText txt =
    genesisPointFromText <|> blockPointFromText
  where
    genesisPointFromText = GenesisPoint
        <$ guard (T.toLower txt == "origin")

    blockPointFromText = BlockPoint
        <$> slotNoFromText slotNo
        <*> headerHashFromText (T.drop 1 headerHash)
      where
        (slotNo, headerHash) = T.breakOn "." (T.strip txt)

getPointSlotNo :: Point -> SlotNo
getPointSlotNo pt =
    case pointSlot pt of
        Origin -> SlotNo 0
        At sl  -> sl

getPointHeaderHash :: Point -> Maybe (HeaderHash Block)
getPointHeaderHash = \case
    GenesisPoint -> Nothing
    BlockPoint _ h -> Just h

unsafeGetPointHeaderHash :: HasCallStack => Point -> HeaderHash Block
unsafeGetPointHeaderHash =
    fromMaybe (error "Point is 'Origin'") . getPointHeaderHash
{-# INLINABLE unsafeGetPointHeaderHash #-}

pointToJson
    :: Point
    -> Json.Encoding
pointToJson = \case
    GenesisPoint ->
        Json.text "origin"
    BlockPoint slotNo headerHash ->
        Json.pairs $ mconcat
            [ Json.pair "slot_no" (slotNoToJson slotNo)
            , Json.pair "header_hash" (headerHashToJson headerHash)
            ]

-- SlotNo

slotNoToJson :: SlotNo -> Json.Encoding
slotNoToJson =
    Json.integer . toInteger . unSlotNo
{-# INLINABLE slotNoToJson #-}

-- | Parse a slot number from a text string.
slotNoFromText :: Text -> Maybe SlotNo
slotNoFromText txt = do
    (slotNo, remSlotNo) <- either (const Nothing) Just (T.decimal txt)
    guard (T.null remSlotNo)
    guard (slotNo < maxBound `div` 2 - 1)
    pure (SlotNo slotNo)

slotNoToText :: SlotNo -> Text
slotNoToText =
    show . unSlotNo
{-# INLINABLE slotNoToText #-}

distanceToSlot :: SlotNo -> SlotNo -> Word64
distanceToSlot (SlotNo a) (SlotNo b)
    | a > b = a - b
    | otherwise = b - a

-- Hash

hashToJson :: HashAlgorithm alg => Hash alg a -> Json.Encoding
hashToJson (UnsafeHash h) =
    encodeBytes (fromShort h)

-- Digest

digestSize :: forall alg. HashAlgorithm alg => Int
digestSize =
    fromIntegral (sizeHash (Proxy @alg))
{-# INLINABLE digestSize #-}

-- WithOrigin

instance ToJSON (WithOrigin SlotNo) where
    toEncoding = \case
        Origin -> toEncoding ("origin" :: Text)
        At sl -> toEncoding sl

-- Helper

sizeInvariant :: HasCallStack => (Int -> Bool) -> ByteString -> ByteString
sizeInvariant predicate bytes
    | predicate (BS.length bytes) =
        bytes
    | otherwise =
        error ("predicate failed for bytes: " <> show bytes)
