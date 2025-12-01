--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kupo.Data.Database
    ( -- * Point / Checkpoint
      Checkpoint (..)
    , pointToRow
    , pointFromRow

      -- * Result / Input
    , Input (..)
    , resultToRow
    , resultFromRow

      -- * Policy
    , Policy (..)
    , policyToRow

      -- * ExtendedOutputReference / OutputReference
    , outputReferenceToRow
    , outputReferenceFromRow
    , extendedOutputReferenceToRow
    , extendedOutputReferenceFromRow

      -- * Pattern
    , patternToRow
    , patternFromRow
    , patternToSql

      -- * Datum / BinaryData
    , BinaryData (..)
    , datumToRow
    , datumFromRow
    , datumInfoToRow
    , datumHashToRow
    , binaryDataToRow
    , binaryDataFromRow
    , redeemerToRow
    , redeemerFromRow

      -- * Script / ScriptReference
    , ScriptReference (..)
    , scriptToRow
    , scriptFromRow
    , scriptHashToRow
    , scriptHashFromRow
    , scriptReferenceToRow
    , scriptReferenceFromRow

      -- * Address
    , addressToRow
    , addressFromRow

      -- * StatusFlag
    , statusFlagToSql

      -- * Sorting
    , SortDirection (..)
    , mkSortDirection
    ) where

import Kupo.Prelude

import Cardano.Ledger.Hashes
    ( originalBytes
    )
import Data.Binary
    ( Get
    )
import Data.Bits
    ( Bits (..)
    )
import Kupo.Data.Cardano
    ( mkOutputReference
    , transactionIdToBytes
    , unsafeTransactionIdFromBytes
    )
import Kupo.Data.Http.OrderMatchesBy
    ( OrderMatchesBy (..)
    )
import Kupo.Data.Http.StatusFlag
    ( StatusFlag (..)
    )
import Ouroboros.Consensus.Block
    ( ConvertRawHash (..)
    )

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Plutus.Data as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Kupo.Data.Cardano as App
import qualified Kupo.Data.Pattern as App

--
-- Checkpoint
--

data Checkpoint = Checkpoint
    { checkpointHeaderHash :: !ByteString
    , checkpointSlotNo :: !Word64
    } deriving (Show)

pointFromRow
    :: Checkpoint
    -> App.Point
pointFromRow row = App.BlockPoint
    (App.SlotNo (checkpointSlotNo row))
    (fromShortRawHash (Proxy @App.Block) $ toShort $ checkpointHeaderHash row)
{-# INLINABLE pointFromRow #-}

pointToRow
    :: HasCallStack
    => App.Point
    -> Checkpoint
pointToRow = \case
    App.GenesisPoint -> error "pointToRow: genesis point."
    App.BlockPoint slotNo headerHash -> Checkpoint
        { checkpointHeaderHash = toRawHash proxy headerHash
        , checkpointSlotNo = App.unSlotNo slotNo
        }
  where
    proxy = Proxy @App.Block
{-# INLINABLE pointToRow #-}

--
-- Result
--

data Input = Input
    { extendedOutputReference :: !ByteString
    , address :: !Text
    , value :: !ByteString
    , datum :: !(Maybe BinaryData)
    , datumInfo :: !(Maybe ByteString)
    , refScript :: !(Maybe ScriptReference)
    , refScriptHash :: !(Maybe ByteString)
    , createdAtSlotNo :: !Word64
    , createdAtHeaderHash :: !ByteString
    , spentAtSlotNo :: !(Maybe Word64)
    , spentAtHeaderHash :: !(Maybe ByteString)
    , spentBy :: !(Maybe ByteString)
    , spentWith :: !(Maybe ByteString)
    } deriving (Show)

resultFromRow
    :: Input
    -> App.Result
resultFromRow row = App.Result
    { App.outputReference =
        extendedOutputReferenceFromRow (extendedOutputReference row)
    , App.address =
        addressFromRow (address row)
    , App.value =
        unsafeDecodeCbor @MaryEra "Value" decCBOR (toLazy (value row))
    , App.datum =
        datumFromRow (datumInfo row) (datum row)
    , App.scriptReference =
        scriptReferenceFromRow (refScriptHash row) (refScript row)
    , App.createdAt =
        pointFromRow (Checkpoint (createdAtHeaderHash row) (createdAtSlotNo row))
    , App.spentAt =
        pointFromRow <$> (Checkpoint <$> spentAtHeaderHash row <*> spentAtSlotNo row)
    , App.spentBy =
        outputReferenceFromRow <$> spentBy row
    , App.spentWith =
        redeemerFromRow <$> spentWith row
    }

resultToRow
    :: App.Result
    -> Input
resultToRow x =
    Input {..}
  where
    extendedOutputReference =
        extendedOutputReferenceToRow (App.outputReference x)

    address =
        addressToRow (App.address x)

    value =
        serializeCbor @MaryEra encCBOR (App.value x)

    (datumInfo, datum) =
        datumToRow (App.datum x)

    (refScriptHash, refScript) =
        scriptReferenceToRow (App.scriptReference x)

    (createdAtSlotNo, createdAtHeaderHash) =
        let row = pointToRow (App.createdAt x)
         in (checkpointSlotNo row, checkpointHeaderHash row)

    (spentAtSlotNo, spentAtHeaderHash) =
        let row = pointToRow <$> (App.spentAt x)
         in (checkpointSlotNo <$> row, checkpointHeaderHash <$> row)

    spentBy =
        outputReferenceToRow <$> App.spentBy x

    spentWith =
        redeemerToRow <$> App.spentWith x

--
-- Policy
--

data Policy = Policy
    { outputReference :: !ByteString
    , policyId :: !ByteString
    } deriving (Show, Eq, Ord)

policyToRow
    :: App.OutputReference
    -> App.PolicyId
    -> Policy
policyToRow (outputReferenceToRow -> outputReference) (App.policyIdToBytes -> policyId) =
    Policy { outputReference, policyId }

--
-- Output Reference
--

-- Serialize output references with their associated position within a block. This allows for a
-- compact representation of extended output references, and enable filters by transaction id,
-- transaction id + output index or transaction id + output index + transaction index by doing bytes
-- comparisons on byte string intervals.
--
--     ┏━━━━━━━━━━━━━━━━┯━━━━━━━━━━━━━━┯━━━━━━━━━━━━━━━━━━━┓
--     ┃ transaction id │ output index │ transaction index ┃
--     ┗━━━ 32 bytes ━━━┷━━━ 2 bytes ━━┷━━━━━ 2 bytes ━━━━━┛
--
extendedOutputReferenceToRow :: App.ExtendedOutputReference -> ByteString
extendedOutputReferenceToRow (Ledger.TxIn txId (Ledger.TxIx outIx), txIx) =
    BL.toStrict $ B.runPut $ do
        B.putByteString (transactionIdToBytes txId)
        B.putWord16be outIx
        B.putWord16be txIx

outputReferenceToRow :: App.OutputReference -> ByteString
outputReferenceToRow (Ledger.TxIn txId (Ledger.TxIx outIx)) =
    BL.toStrict $ B.runPut $ do
        B.putByteString (transactionIdToBytes txId)
        B.putWord16be outIx

outputReferenceFromRow :: ByteString -> App.OutputReference
outputReferenceFromRow bytes =
    case B.runGetOrFail parser (BL.fromStrict bytes) of
        Left (_remaining, _offset, hint) ->
            error (toText hint)
        Right (remaining, _offset, result) ->
            if BL.null remaining
            then result
            else error "outputReferenceFromRow: non-empty remaining bytes"
  where
    parser = do
        txId <- unsafeTransactionIdFromBytes <$> B.getByteString (digestSize @Blake2b_256)
        outIx <- Ledger.TxIx <$> B.getWord16be
        pure (Ledger.TxIn txId outIx)

extendedOutputReferenceFromRow :: ByteString -> App.ExtendedOutputReference
extendedOutputReferenceFromRow bytes =
    case B.runGetOrFail parser (BL.fromStrict bytes) of
        Left (_remaining, _offset, hint) ->
            error (toText hint)
        Right (remaining, _offset, result) ->
            if BL.null remaining
            then result
            else error "extendedOutputReferenceFromRow: non-empty remaining bytes"
  where
    parser = do
        txId <- unsafeTransactionIdFromBytes <$> B.getByteString (digestSize @Blake2b_256)
        outIx <- Ledger.TxIx <$> B.getWord16be
        txIx <- B.getWord16be
        pure (Ledger.TxIn txId outIx, txIx)

--
-- Pattern
--

patternToRow
    :: App.Pattern
    -> Text
patternToRow =
    App.patternToText
{-# INLINABLE patternToRow #-}

patternFromRow
    :: HasCallStack
    => Text
    -> App.Pattern
patternFromRow p =
    fromMaybe
        (error $ "patternFromRow: invalid pattern: " <> p)
        (App.patternFromText p)
{-# INLINABLE patternFromRow #-}

--
-- BinaryData / Datum
--

referenceDatum :: Word8
referenceDatum = 0
{-# INLINABLE referenceDatum #-}

inlineDatum :: Word8
inlineDatum = 1
{-# INLINABLE inlineDatum #-}

data BinaryData = BinaryData
    { binaryDataHash :: !ByteString
    , binaryData :: !ByteString
    } deriving (Show)

-- | Deserialise a datum from the database. This takes two parts arguments:
--
-- - A 'BinaryData' and;
-- - A serialized "datum_info":
--
--     ┏━━━━━━━━━━━━┯━━━━━━━━━━━━┓
--     ┃ datum flag │ datum hash ┃
--     ┗━━ 1 byte ━━┷━ 32 bytes ━┛
--
-- The `datum_info` carries a flag which can be either `0` to identify a datum that was only
-- referenced by hash in an output, or `1` to identify an inline datum. The binary data is
-- only provided when the full datum is fetched from the database (and is available!);
--
-- The `datum_info` may also simply be null (i.e. `Nothing`) meaning that there's simply no datum.
--
-- Invariant (1): 2nd arg is `Just`    => 1st arg is `Just`.
-- Invariant (2): 1st arg is `Nothing` => 2nd arg is `Nothing`
datumFromRow
    :: Maybe ByteString
    -> Maybe BinaryData
    -> App.Datum
datumFromRow hash = \case
    Just BinaryData{..} ->
        maybe
            (error "Database integrity issue: presence of a binary data without associated datum info?")
            (datumInfoFromRow (const (Right bin)))
            hash
      where
        bin = App.unsafeBinaryDataFromBytes binaryData
    Nothing ->
        maybe
            App.NoDatum
            (datumInfoFromRow (Left . App.unsafeDatumHashFromBytes))
            hash
  where
    datumInfoFromRow mk h =
        case BS.uncons h of
            Just (flag, bytes) | flag == referenceDatum ->
                App.Reference (mk bytes)
            Just (flag, bytes) | flag == inlineDatum ->
                App.Inline (mk bytes)
            _unknownFlag ->
                error "Unexpected flag prefix for serialized datum info. \
                      \Should be either: 0 => reference; or 1 => inline."
{-# INLINABLE datumFromRow #-}

datumToRow
    :: App.Datum
    -> (Maybe ByteString, Maybe BinaryData)
datumToRow = \case
    App.NoDatum ->
        (Nothing, Nothing)
    App.Reference (Left ref) ->
        (Just (datumInfoToRow referenceDatum ref), Nothing)
    App.Reference (Right bin) ->
        let ref = App.hashBinaryData bin
         in (Just (datumInfoToRow referenceDatum ref), Just (binaryDataToRow ref bin))
    App.Inline (Left ref) ->
        (Just (datumInfoToRow inlineDatum ref), Nothing)
    App.Inline (Right bin) ->
        let ref = App.hashBinaryData bin
         in (Just (datumInfoToRow inlineDatum ref), Just (binaryDataToRow ref bin))
{-# INLINABLE datumToRow #-}

datumInfoToRow
    :: Word8 -- ^ 0 => reference, 1 => inline
    -> App.DatumHash
    -> ByteString
datumInfoToRow flag =
    BS.cons flag . datumHashToRow
{-# INLINABLE datumInfoToRow #-}

datumHashToRow
    :: App.DatumHash
    -> ByteString
datumHashToRow =
    originalBytes
{-# INLINABLE datumHashToRow #-}

binaryDataToRow
    :: App.DatumHash
    -> App.BinaryData
    -> BinaryData
binaryDataToRow hash bin = BinaryData
    { binaryDataHash = datumHashToRow hash
    , binaryData = originalBytes (Ledger.binaryDataToData bin)
    }
{-# INLINABLE binaryDataToRow #-}

binaryDataFromRow
    :: BinaryData
    -> App.BinaryData
binaryDataFromRow =
    App.unsafeBinaryDataFromBytes . binaryData
{-# INLINABLE binaryDataFromRow #-}

redeemerToRow
    :: App.BinaryData
    -> ByteString
redeemerToRow =
    originalBytes . Ledger.binaryDataToData
{-# INLINABLE redeemerToRow #-}

redeemerFromRow
    :: ByteString
    -> App.BinaryData
redeemerFromRow =
    App.unsafeBinaryDataFromBytes
{-# INLINABLE redeemerFromRow #-}

--
-- Script / ScriptHash
--

data ScriptReference = ScriptReference
    { scriptHash :: !ByteString
    , script :: !ByteString
    } deriving (Show)

scriptHashToRow
    :: App.ScriptHash
    -> ByteString
scriptHashToRow =
    App.scriptHashToBytes
{-# INLINABLE scriptHashToRow #-}

scriptHashFromRow
    :: ByteString
    -> App.ScriptHash
scriptHashFromRow =
    App.unsafeScriptHashFromBytes
{-# INLINABLE scriptHashFromRow #-}

scriptToRow
    :: App.ScriptHash
    -> App.Script
    -> ScriptReference
scriptToRow hash s = ScriptReference
    { scriptHash = scriptHashToRow hash
    , script = App.scriptToBytes s
    }
{-# INLINABLE scriptToRow  #-}

scriptFromRow
    :: ScriptReference
    -> App.Script
scriptFromRow ScriptReference{..} =
    App.unsafeScriptFromBytes script
{-# INLINABLE scriptFromRow #-}

scriptReferenceToRow
    :: App.ScriptReference
    -> (Maybe ByteString, Maybe ScriptReference)
scriptReferenceToRow = \case
    App.NoScript ->
        (Nothing, Nothing)
    App.ReferencedScript h ->
        (Just (scriptHashToRow h), Nothing)
    App.InlineScript s ->
        let h = App.hashScript s
         in (Just (scriptHashToRow h), Just (scriptToRow h s))
{-# INLINABLE scriptReferenceToRow #-}

scriptReferenceFromRow
    :: Maybe ByteString
    -> Maybe ScriptReference
    -> App.ScriptReference
scriptReferenceFromRow hash = \case
    Just s ->
        App.InlineScript (scriptFromRow s)
    Nothing ->
        maybe App.NoScript (App.ReferencedScript . scriptHashFromRow) hash
{-# INLINABLE scriptReferenceFromRow  #-}

--
-- Address
--

-- Address serialization is a bit special because we want the ability to
-- leverage SQLite indexes whenever possible. Addresses can be looked up by
-- 'patterns' and, while we can't benefits from the database index for all
-- possible patterns, there are two particular scenarios where we absolutely
-- want to:
--
-- - When looking up by address (i.e. 'MatchExact' with a full address)
-- - When looking up by stake reference.
--
-- SQLite rules for using an index can depends on quite many things but for
-- column with a text affinity, the query must be in the form of:
--
--     column = expression
--     column >,>=,<,<=  expression
--     column IN (expression or sub-query)
--     column IS [NOT] NULL
--
-- Which means that we are pretty much covered regarding the first case. We
-- however make heavy use of the `LIKE` operator for searching addresses.
-- Addresses are indeed stored as once blob and, searching addresses is done by
-- searching on the serialized string.
--
-- However, when using wildcard operators in combination with `LIKE`, SQLite
-- will rewrite queries in the form of `_%` as `>= _ AND < _` for which it
-- becomes possible to make use of the index. Therefore, we can optimize later
-- search queries by always serializing the delegation part (when any) of
-- addresses first.
--
-- Fundamentally, addresses are serialised as such on-chain:
--
--     ┏━━━━━━━━┯━━━━━━━━━━━━━━━━━━━━━┯━━━━━━━━━━━━━━━━━━━━━━━━┓
--     ┃ header │ payment credentials │ delegation credentials ┃
--     ┗━━━━━━━━┷━━━━━━━━━━━━━━━━━━━━━┷━━━━━━━━━━━━━━━━━━━━━━━━┛
--
-- Kupo serialises them slightly differently:
--
--     ┏━━━━━━━━━━┯━━━━━━━━━━━━━━━━━━━━━━━━┯━━━━━━━━━━┯━━━━━━━━━━━━━━━━━━━━━┓
--     ┃    tag   │ delegation credentials │  header  │ payment credentials ┃
--     ┗━ 1 byte ━┷━━━━━━━ 28 bytes ━━━━━━━┷━ 1 byte ━┷━━━━━━ 28 bytes ━━━━━┛
--
-- The tag is nothing more than a simplified address discriminator which, unlike
-- the header, doesn't include a network id, and only distinguish addresses
-- based on their delegation credentials:
--
-- - 0: N/A (Byron)
-- - 1: Script or Key hash
-- - 2: Pointer
-- - 3: None
--
-- This makes it a lot easier to search addresses by delegation credentials
-- later on and to deserialise them. Note that having the payment credentials at
-- the end of the sequence in all cases is also done purposedly as it allows to
-- formulate search queries on payment credentials more easily (since we know
-- that, by definition, there are ALWAYS a payment credentials). Thus, the last
-- 28 bytes of an address in the database always represent a payment credential.
--
-- See 'patternToSql' to see how we can express the efficient search queries
-- based on this structure.
--
-- Beside this little reshuffling, we use the very same serialization methods
-- as the ledger for credentials, network and pointer.
addressToRow
    :: App.Address
    -> Text
addressToRow = encodeBase16 . BL.toStrict . B.runPut . \case
    Ledger.AddrBootstrap (Ledger.BootstrapAddress addr) -> do
        B.putWord8 0
        B.putByteString (serializeCbor @ByronEra encCBOR addr)
    Ledger.Addr (Ledger.networkToWord8 -> network) p (Ledger.StakeRefBase s) -> do
        let header = setDelegationBit s (setPaymentBit p network)
        B.putWord8 1
        Ledger.putCredential s
        B.putWord8 header
        Ledger.putCredential p
    Ledger.Addr (Ledger.networkToWord8 -> network) p (Ledger.StakeRefPtr ptr) -> do
        let header = setPaymentBit p (network `setBit` 6)
        B.putWord8 2
        Ledger.putPtr ptr
        B.putWord8 header
        Ledger.putCredential p
    Ledger.Addr (Ledger.networkToWord8 -> network) p Ledger.StakeRefNull -> do
        let header = setPaymentBit p (network `setBit` 5 `setBit` 6)
        B.putWord8 3
        B.putWord8 header
        Ledger.putCredential p
  where
    setPaymentBit = \case
        Ledger.ScriptHashObj _ -> (`setBit` 4)
        Ledger.KeyHashObj _ -> identity

    setDelegationBit = \case
        Ledger.ScriptHashObj _ -> (`setBit` 5)
        Ledger.KeyHashObj _ -> identity

-- | See 'addressToRow'.
addressFromRow
    :: Text
    -> App.Address
addressFromRow =
    unsafeDeserialize . unsafeDecodeBase16
  where
    unsafeDeserialize bytes =
        case B.runGetOrFail getAddr (BL.fromStrict bytes) of
            Left (_remaining, _offset, hint) ->
                error (toText hint)
            Right (remaining, _offset, result) ->
                if BL.null remaining
                then result
                else error "addressFromRow: non-empty remaining bytes"

    getAddr = do
        B.getWord8 >>= \case
            0 -> do
                bytes <- B.getRemainingLazyByteString
                case decodeCbor @ByronEra "Address<Byron>" decCBOR bytes of
                    Left e ->
                        fail (show e)
                    Right r ->
                        pure $ Ledger.AddrBootstrap $ Ledger.BootstrapAddress r
            1 -> do
                delegation <- getHash @Blake2b_224
                header <- B.getWord8
                payment <- getHash @Blake2b_224
                pure $ Ledger.Addr
                    (unsafeNetworkFromHeader header)
                    (mkPaymentCredential header payment)
                    (Ledger.StakeRefBase (mkDelegationCredential header delegation))
            2 -> do
                ptr <- getPtr
                header <- B.getWord8
                payment <- getHash @Blake2b_224
                pure $ Ledger.Addr
                    (unsafeNetworkFromHeader header)
                    (mkPaymentCredential header payment)
                    (Ledger.StakeRefPtr ptr)
            3 -> do
                header <- B.getWord8
                payment <- getHash @Blake2b_224
                pure $ Ledger.Addr
                    (unsafeNetworkFromHeader header)
                    (mkPaymentCredential header payment)
                    Ledger.StakeRefNull
            tag ->
                fail ("unknown tag: " <> show tag)

    getPtr = Ledger.Ptr
        <$> (Ledger.SlotNo32 . fromIntegral <$> getVariableLengthWord64)
        <*> (Ledger.TxIx . fromIntegral <$> getVariableLengthWord64)
        <*> (Ledger.CertIx . fromIntegral <$> getVariableLengthWord64)

    getHash :: forall alg. HashAlgorithm alg => Get ByteString
    getHash =
        B.getByteString (digestSize @alg)

    mkDelegationCredential
        :: forall kr. ()
        => Word8
        -> ByteString
        -> Ledger.Credential kr
    mkDelegationCredential header
        | header `testBit` 5 =
            Ledger.ScriptHashObj . Ledger.ScriptHash . unsafeHashFromBytes
        | otherwise =
            Ledger.KeyHashObj . Ledger.KeyHash . unsafeHashFromBytes

    mkPaymentCredential
        :: forall kr. ()
        => Word8
        -> ByteString
        -> Ledger.Credential kr
    mkPaymentCredential header
        | header `testBit` 4 =
            Ledger.ScriptHashObj . Ledger.ScriptHash . unsafeHashFromBytes
        | otherwise =
            Ledger.KeyHashObj . Ledger.KeyHash . unsafeHashFromBytes

    unsafeNetworkFromHeader :: Word8 -> Ledger.Network
    unsafeNetworkFromHeader header =
        case Ledger.word8ToNetwork (header .&. 0x0F) of
            Just network -> network
            Nothing -> error "unsafeNetworkFromHeader: invalid network id"

getVariableLengthWord64 :: Get Word64
getVariableLengthWord64 = word7sToWord64 <$> getWord7s

word7sToWord64 :: [Ledger.Word7] -> Word64
word7sToWord64 = foldl' f 0
  where
    f n (Ledger.Word7 r) = shiftL n 7 .|. fromIntegral r

getWord7s :: Get [Ledger.Word7]
getWord7s = do
  nextWord <- B.getWord8
  -- is the high bit set?
  if testBit nextWord 7
    then -- if so, grab more words
      (:) (Ledger.toWord7 nextWord) <$> getWord7s
    else -- otherwise, this is the last one
      pure [Ledger.Word7 nextWord]

--
-- Filters
--

-- Invariant: cannot be called with 'MatchMetadataTag'; this pattern is only for indexing.
patternToSql
    :: HasCallStack
    => App.Pattern
    -> (Text, Maybe Text)
patternToSql = \case
    App.MatchAny App.IncludingBootstrap ->
        ( "address LIKE '%'"
        , Nothing
        )
    App.MatchAny App.OnlyShelley ->
        ( "address NOT LIKE '00%'"
        , Nothing
        )
    App.MatchExact addr ->
        ( "address = '" <> addressToRow addr <> "'"
        , Nothing
        )
    App.MatchPayment payment ->
        ( "payment_credential = '" <> x payment <> "'"
        , Nothing
        )
    App.MatchDelegation delegation ->
        ( "address LIKE '01" <> x delegation <> "%'"
        , Nothing
        )
    App.MatchPaymentAndDelegation payment delegation ->
        ( "address LIKE '01" <> x delegation <> "%' AND payment_credential = '" <> x payment <> "'"
        , Nothing
        )
    App.MatchOutputReference ref ->
        ( "inputs.output_reference = x'" <> x (outputReferenceToRow ref) <> "'"
        , Nothing
        )
    App.MatchTransactionId txId ->
        let
            lowerBound = outputReferenceToRow (mkOutputReference txId minBound)
            upperBound = outputReferenceToRow (mkOutputReference txId maxBound)
        in
        ( "inputs.output_reference BETWEEN "
            <> "x'" <> x lowerBound <> "'"
            <> " AND "
            <> "x'" <> x upperBound <> "'"
        , Nothing
        )
    App.MatchPolicyId pid ->
        ( "policies.output_reference >= 0 AND policy_id = x'" <> x (App.policyIdToBytes pid) <> "'"
        , Just "JOIN policies ON inputs.output_reference = policies.output_reference"
        )
    App.MatchAssetId (pid, _) ->
        patternToSql (App.MatchPolicyId pid)
    App.MatchMetadataTag{} ->
        error "patternToSql: called for 'MatchMetadataTag'"
  where
    x = encodeBase16

statusFlagToSql  :: StatusFlag -> Text
statusFlagToSql  = \case
    NoStatusFlag ->
        ""
    OnlyUnspent ->
        "spent_at IS NULL"
    OnlySpent ->
        "spent_at IS NOT NULL"

data SortDirection = Asc | Desc
    deriving (Generic)

mkSortDirection :: OrderMatchesBy -> SortDirection
mkSortDirection = \case
    MostRecentFirst -> Desc
    NoExplicitOrder -> Desc
    OldestFirst -> Asc
