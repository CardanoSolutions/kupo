--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kupo.Data.Database
    ( -- * Point
      pointToRow
    , pointFromRow

      -- * Result
    , resultToRow
    , resultFromRow

      -- * Pattern
    , patternToRow
    , patternFromRow
    , patternToSql

      -- * Datum / BinaryData
    , datumToRow
    , datumFromRow
    , datumHashToRow
    , binaryDataToRow
    , binaryDataFromRow

      -- * Filtering
    , applyStatusFlag
    ) where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( BinaryData
    , Block
    , Datum
    , DatumHash
    , SlotNo (..)
    , StandardCrypto
    , hashDatum
    , noDatum
    , unsafeBinaryDataFromBytes
    , unsafeDatumHashFromBytes
    )
import Kupo.Data.Http.StatusFlag
    ( StatusFlag (..) )
import Kupo.Data.Pattern
    ( MatchBootstrap (..)
    , Pattern (..)
    , Result (..)
    , patternFromText
    , patternToText
    )
import Ouroboros.Consensus.Block
    ( ConvertRawHash (..) )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock )
import Ouroboros.Network.Block
    ( pattern BlockPoint, pattern GenesisPoint, Point (..) )

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Kupo.Control.MonadDatabase as DB

--
-- Checkpoint
--

pointFromRow
    :: DB.Checkpoint
    -> Point (CardanoBlock StandardCrypto)
pointFromRow row = BlockPoint
    (SlotNo (DB.checkpointSlotNo row))
    (fromShortRawHash (Proxy @Block) $ toShort $ DB.checkpointHeaderHash row)

pointToRow
    :: HasCallStack
    => Point (CardanoBlock StandardCrypto)
    -> DB.Checkpoint
pointToRow = \case
    GenesisPoint -> error "pointToRow: genesis point."
    BlockPoint slotNo headerHash -> DB.Checkpoint
        { DB.checkpointHeaderHash = toRawHash proxy headerHash
        , DB.checkpointSlotNo = unSlotNo slotNo
        }
  where
    proxy = Proxy @(CardanoBlock StandardCrypto)

--
-- Result
--

resultFromRow
    :: HasCallStack
    => DB.Input
    -> Result
resultFromRow row = Result
    { outputReference =
        unsafeDeserialize' (DB.outputReference row)
    , address =
        (unsafeAddressFromBytes . unsafeDecodeBase16)  (DB.address row)
    , value =
        unsafeDeserialize' (DB.value row)
    , datum =
        datumFromRow (DB.datumHash row) (DB.datum row)
    , createdAt =
        pointFromRow (DB.Checkpoint (DB.createdAtHeaderHash row) (DB.createdAtSlotNo row))
    , spentAt =
        pointFromRow <$> (DB.Checkpoint <$> DB.spentAtHeaderHash row <*> DB.spentAtSlotNo row)
    }
  where
    unsafeAddressFromBytes =
        fromMaybe (error "unsafeAddressFromBytes") . Ledger.deserialiseAddr

resultToRow
    :: Result
    -> DB.Input
resultToRow Result{..} = DB.Input
    { DB.outputReference =
        serialize' outputReference
    , DB.address =
        encodeBase16 (Ledger.serialiseAddr address)
    , DB.value =
        serialize' value
    , DB.datum =
        datumToRow datum
    , DB.datumHash =
        datumHashToRow <$> hashDatum datum
    , DB.createdAtSlotNo =
        DB.checkpointSlotNo createdAtRow
    , DB.createdAtHeaderHash =
        DB.checkpointHeaderHash createdAtRow
    , DB.spentAtSlotNo =
        DB.checkpointSlotNo <$> spentAtRow
    , DB.spentAtHeaderHash =
        DB.checkpointHeaderHash <$> spentAtRow
    }
  where
    createdAtRow = pointToRow createdAt
    spentAtRow = pointToRow <$> spentAt

--
-- BinaryData / Datum
--

datumFromRow
    :: Maybe ByteString
    -> Maybe DB.BinaryData
    -> Datum
datumFromRow hash = \case
    Just DB.BinaryData{..} ->
        Ledger.Datum (unsafeBinaryDataFromBytes binaryData)
    Nothing ->
        maybe noDatum (Ledger.DatumHash . unsafeDatumHashFromBytes) hash

datumToRow
    :: Datum
    -> Maybe DB.BinaryData
datumToRow = \case
    Ledger.NoDatum ->
        Nothing
    Ledger.DatumHash{} ->
        Nothing
    Ledger.Datum bin ->
        Just (binaryDataToRow (Ledger.hashBinaryData bin) bin)

datumHashToRow
    :: DatumHash
    -> ByteString
datumHashToRow =
    Ledger.originalBytes

binaryDataToRow
    :: DatumHash
    -> BinaryData
    -> DB.BinaryData
binaryDataToRow hash bin = DB.BinaryData
    { DB.binaryDataHash = datumHashToRow hash
    , DB.binaryData = Ledger.originalBytes (Ledger.binaryDataToData bin)
    }

binaryDataFromRow
    :: DB.BinaryData
    -> BinaryData
binaryDataFromRow =
    unsafeBinaryDataFromBytes . DB.binaryData

--
-- Pattern
--

patternToRow
    :: Pattern
    -> Text
patternToRow =
    patternToText

patternFromRow
    :: HasCallStack
    => Text
    -> Pattern
patternFromRow p =
    fromMaybe
        (error $ "patternFromRow: invalid pattern: " <> p)
        (patternFromText p)

patternToSql
    :: Pattern
    -> Text
patternToSql = \case
    MatchAny IncludingBootstrap ->
        "LIKE '%'"
    MatchAny OnlyShelley ->
        "NOT LIKE '8%'"
    MatchExact addr ->
        "= '" <> encodeBase16 (Ledger.serialiseAddr addr) <> "'"
    MatchPayment payment ->
        "LIKE '__" <> encodeBase16 payment <> "%'"
    MatchDelegation delegation ->
        "LIKE '%" <> encodeBase16 delegation <> "' AND len > 58"
    MatchPaymentAndDelegation payment delegation ->
        "LIKE '__" <> encodeBase16 payment <> encodeBase16 delegation <> "'"

--
-- Filters
--

applyStatusFlag :: StatusFlag -> Text -> Text
applyStatusFlag = \case
    NoStatusFlag ->
        identity
    OnlyUnspent ->
        (<> " AND spent_at IS NULL")
    OnlySpent ->
        (<> " AND spent_at IS NOT NULL")
