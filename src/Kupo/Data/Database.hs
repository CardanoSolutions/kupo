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

      -- * Pattern
    , patternToRow
    , patternFromRow
    , patternToSql

      -- * Datum / BinaryData
    , BinaryData (..)
    , datumToRow
    , datumFromRow
    , datumHashToRow
    , binaryDataToRow
    , binaryDataFromRow

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

      -- * Filtering
    , applyStatusFlag
    ) where

import Kupo.Prelude

import Cardano.Binary
    ( decodeFull
    , serialize
    )
import Data.Binary
    ( Get
    )
import Data.Bits
    ( Bits (..)
    )
import Kupo.Data.Cardano
    ( Blake2b_224
    , Crypto
    , Hash
    , HashAlgorithm
    , binaryDataToBytes
    , datumHashToBytes
    , digestSize
    , hashFromBytes
    , transactionIdToBytes
    )
import Kupo.Data.Http.StatusFlag
    ( StatusFlag (..)
    )
import Ouroboros.Consensus.Block
    ( ConvertRawHash (..)
    )

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString.Lazy as BSL
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
    { outputReference :: !ByteString
    , address :: !Text
    , value :: !ByteString
    , datum :: !(Maybe BinaryData)
    , datumHash :: !(Maybe ByteString)
    , refScript :: !(Maybe ScriptReference)
    , refScriptHash :: !(Maybe ByteString)
    , createdAtSlotNo :: !Word64
    , createdAtHeaderHash :: !ByteString
    , spentAtSlotNo :: !(Maybe Word64)
    , spentAtHeaderHash :: !(Maybe ByteString)
    } deriving (Show)

resultFromRow
    :: Input
    -> App.Result
resultFromRow row = App.Result
    { App.outputReference =
        unsafeDeserialize' (outputReference row)
    , App.address =
        addressFromRow (address row)
    , App.value =
        unsafeDeserialize' (value row)
    , App.datum =
        datumFromRow (datumHash row) (datum row)
    , App.scriptReference =
        scriptReferenceFromRow (refScriptHash row) (refScript row)
    , App.createdAt =
        pointFromRow (Checkpoint (createdAtHeaderHash row) (createdAtSlotNo row))
    , App.spentAt =
        pointFromRow <$> (Checkpoint <$> spentAtHeaderHash row <*> spentAtSlotNo row)
    }

resultToRow
    :: App.Result
    -> Input
resultToRow x =
    Input {..}
  where
    outputReference =
        serialize' (App.outputReference x)

    address =
        addressToRow (App.address x)

    value =
        serialize' (App.value x)

    (datumHash, datum) =
        datumToRow (App.datum x)

    (refScriptHash, refScript) =
        scriptReferenceToRow (App.scriptReference x)

    (createdAtSlotNo, createdAtHeaderHash) =
        let row = pointToRow (App.createdAt x)
         in (checkpointSlotNo row, checkpointHeaderHash row)

    (spentAtSlotNo, spentAtHeaderHash) =
        let row = pointToRow <$> (App.spentAt x)
         in (checkpointSlotNo <$> row, checkpointHeaderHash <$> row)

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

data BinaryData = BinaryData
    { binaryDataHash :: !ByteString
    , binaryData :: !ByteString
    } deriving (Show)

datumFromRow
    :: Maybe ByteString
    -> Maybe BinaryData
    -> App.Datum
datumFromRow hash = \case
    Just BinaryData{..} ->
        App.fromBinaryData (App.unsafeBinaryDataFromBytes binaryData)
    Nothing ->
        maybe App.noDatum (App.fromDatumHash . App.unsafeDatumHashFromBytes) hash
{-# INLINABLE datumFromRow #-}

datumToRow
    :: App.Datum
    -> (Maybe ByteString, Maybe BinaryData)
datumToRow = \case
    Ledger.NoDatum ->
        (Nothing, Nothing)
    Ledger.DatumHash h ->
        (Just (datumHashToRow h), Nothing)
    Ledger.Datum bin ->
        let h = App.hashBinaryData bin
         in (Just (datumHashToRow h), Just (binaryDataToRow h bin))
{-# INLINABLE datumToRow #-}

datumHashToRow
    :: App.DatumHash
    -> ByteString
datumHashToRow =
    datumHashToBytes
{-# INLINABLE datumHashToRow #-}

binaryDataToRow
    :: App.DatumHash
    -> App.BinaryData
    -> BinaryData
binaryDataToRow hash bin = BinaryData
    { binaryDataHash = datumHashToRow hash
    , binaryData = binaryDataToBytes bin
    }
{-# INLINABLE binaryDataToRow #-}

binaryDataFromRow
    :: BinaryData
    -> App.BinaryData
binaryDataFromRow =
    App.unsafeBinaryDataFromBytes . binaryData
{-# INLINABLE binaryDataFromRow #-}

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
--     ┏━━━━━┯━━━━━━━━━━━━━━━━━━━━━━━━┯━━━━━━━━┯━━━━━━━━━━━━━━━━━━━━━┓
--     ┃ tag │ delegation credentials │ header │ payment credentials ┃
--     ┗━━━━━┷━━━━━━━━━━━━━━━━━━━━━━━━┷━━━━━━━━┷━━━━━━━━━━━━━━━━━━━━━┛
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
addressToRow = encodeBase16 . BSL.toStrict . B.runPut . \case
    Ledger.AddrBootstrap (Ledger.BootstrapAddress addr) -> do
        B.putWord8 0
        B.putLazyByteString (serialize addr)
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
        case B.runGetOrFail getAddr (BSL.fromStrict bytes) of
            Left (_remaining, _offset, hint) ->
                error (toText hint)
            Right (remaining, _offset, result) ->
                if BSL.null remaining
                  then result
                  else error "addressFromRow: non-empty remaining bytes"

    getAddr = do
        B.getWord8 >>= \case
            0 -> do
                bytes <- B.getRemainingLazyByteString
                case decodeFull bytes of
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
                ptr <- Ledger.getPtr
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

    getHash :: forall alg. HashAlgorithm alg => Get ByteString
    getHash =
        B.getByteString (digestSize @alg)

    mkDelegationCredential
        :: forall kr crypto. (Crypto crypto)
        => Word8
        -> ByteString
        -> Ledger.Credential kr crypto
    mkDelegationCredential header
        | header `testBit` 5 =
            Ledger.ScriptHashObj . Ledger.ScriptHash . unsafeHashFromBytes
        | otherwise =
            Ledger.KeyHashObj . Ledger.KeyHash . unsafeHashFromBytes

    mkPaymentCredential
        :: forall kr crypto. (Crypto crypto)
        => Word8
        -> ByteString
        -> Ledger.Credential kr crypto
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

    unsafeHashFromBytes :: forall alg a. HashAlgorithm alg => ByteString -> Hash alg a
    unsafeHashFromBytes bytes =
        case hashFromBytes bytes of
            Nothing -> error "unsafeHashFromBytes: digest size mismatch"
            Just !h -> h


--
-- Filters
--

patternToSql
    :: App.Pattern
    -> Text
patternToSql = \case
    App.MatchAny App.IncludingBootstrap ->
        "address LIKE '%'"
    App.MatchAny App.OnlyShelley ->
        "address NOT LIKE '00%'"
    App.MatchExact addr ->
        "address = '" <> addressToRow addr <> "'"
    App.MatchPayment payment ->
        "address LIKE '%" <> encodeBase16 payment <> "'"
    App.MatchDelegation delegation ->
        "address LIKE '01" <> encodeBase16 delegation <> "%'"
    App.MatchPaymentAndDelegation payment delegation ->
        "address LIKE '01" <> encodeBase16 delegation <> "__" <> encodeBase16 payment <> "'"
    App.MatchOutputReference (encodeBase16 . serialize' -> str) ->
        "output_reference = x'" <> str <> "'"
    App.MatchTransactionId (encodeBase16 . transactionIdToBytes -> txId) ->
        unwords
        [ "(    output_reference >= x'825820" <> txId <> "00'\
          \ AND output_reference <= x'825820" <> txId <> "17')"
        , "OR"
        , "(    output_reference >= x'825820" <> txId <> "1818'\
          \ AND output_reference <= x'825820" <> txId <> "18ff')"
        , "OR"
        , "(    output_reference >= x'825820" <> txId <> "190100'\
          \ AND output_reference <= x'825820" <> txId <> "190100')"
        ]
    App.MatchPolicyId{} ->
        "address LIKE '%'"
    App.MatchAssetId{} ->
        "address LIKE '%'"

applyStatusFlag :: StatusFlag -> Text -> Text
applyStatusFlag = \case
    NoStatusFlag ->
        identity
    OnlyUnspent ->
        (<> " AND spent_at IS NULL")
    OnlySpent ->
        (<> " AND spent_at IS NOT NULL")
