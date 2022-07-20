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

      -- * Filtering
    , applyStatusFlag
    ) where

import Kupo.Prelude

import Kupo.Data.Http.StatusFlag
    ( StatusFlag (..) )
import Ouroboros.Consensus.Block
    ( ConvertRawHash (..) )

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Kupo.Data.Cardano as App
import qualified Kupo.Data.Pattern as App

data Input = Input
    { outputReference :: ByteString
    , address :: Text
    , value :: ByteString
    , datum :: Maybe BinaryData
    , datumHash :: Maybe ByteString
    , createdAtSlotNo :: Word64
    , createdAtHeaderHash :: ByteString
    , spentAtSlotNo :: Maybe Word64
    , spentAtHeaderHash :: Maybe ByteString
    } deriving (Show)

data BinaryData = BinaryData
    { binaryDataHash :: ByteString
    , binaryData :: ByteString
    } deriving (Show)

data Checkpoint = Checkpoint
    { checkpointHeaderHash :: ByteString
    , checkpointSlotNo :: Word64
    } deriving (Show)

--
-- Checkpoint
--

pointFromRow
    :: Checkpoint
    -> App.Point App.Block
pointFromRow row = App.BlockPoint
    (App.SlotNo (checkpointSlotNo row))
    (fromShortRawHash (Proxy @App.Block) $ toShort $ checkpointHeaderHash row)

pointToRow
    :: HasCallStack
    => App.Point App.Block
    -> Checkpoint
pointToRow = \case
    App.GenesisPoint -> error "pointToRow: genesis point."
    App.BlockPoint slotNo headerHash -> Checkpoint
        { checkpointHeaderHash = toRawHash proxy headerHash
        , checkpointSlotNo = App.unSlotNo slotNo
        }
  where
    proxy = Proxy @App.Block

--
-- Result
--

resultFromRow
    :: HasCallStack
    => Input
    -> App.Result
resultFromRow row = App.Result
    { App.outputReference =
        unsafeDeserialize' (outputReference row)
    , App.address =
        (unsafeAddressFromBytes . unsafeDecodeBase16)  (address row)
    , App.value =
        unsafeDeserialize' (value row)
    , App.datum =
        datumFromRow (datumHash row) (datum row)
    , App.createdAt =
        pointFromRow (Checkpoint (createdAtHeaderHash row) (createdAtSlotNo row))
    , App.spentAt =
        pointFromRow <$> (Checkpoint <$> spentAtHeaderHash row <*> spentAtSlotNo row)
    }
  where
    unsafeAddressFromBytes =
        fromMaybe (error "unsafeAddressFromBytes") . Ledger.deserialiseAddr

resultToRow
    :: App.Result
    -> Input
resultToRow App.Result{..} = Input
    { outputReference =
        serialize' outputReference
    , address =
        encodeBase16 (Ledger.serialiseAddr address)
    , value =
        serialize' value
    , datum =
        datumToRow datum
    , datumHash =
        datumHashToRow <$> App.hashDatum datum
    , createdAtSlotNo =
        checkpointSlotNo createdAtRow
    , createdAtHeaderHash =
        checkpointHeaderHash createdAtRow
    , spentAtSlotNo =
        checkpointSlotNo <$> spentAtRow
    , spentAtHeaderHash =
        checkpointHeaderHash <$> spentAtRow
    }
  where
    createdAtRow = pointToRow createdAt
    spentAtRow = pointToRow <$> spentAt

--
-- BinaryData / Datum
--

datumFromRow
    :: Maybe ByteString
    -> Maybe BinaryData
    -> App.Datum
datumFromRow hash = \case
    Just BinaryData{..} ->
        Ledger.Datum (App.unsafeBinaryDataFromBytes binaryData)
    Nothing ->
        maybe App.noDatum (Ledger.DatumHash . App.unsafeDatumHashFromBytes) hash

datumToRow
    :: App.Datum
    -> Maybe BinaryData
datumToRow = \case
    Ledger.NoDatum ->
        Nothing
    Ledger.DatumHash{} ->
        Nothing
    Ledger.Datum bin ->
        Just (binaryDataToRow (App.hashBinaryData bin) bin)

datumHashToRow
    :: App.DatumHash
    -> ByteString
datumHashToRow =
    Ledger.originalBytes

binaryDataToRow
    :: App.DatumHash
    -> App.BinaryData
    -> BinaryData
binaryDataToRow hash bin = BinaryData
    { binaryDataHash = datumHashToRow hash
    , binaryData = Ledger.originalBytes (Ledger.binaryDataToData bin)
    }

binaryDataFromRow
    :: BinaryData
    -> App.BinaryData
binaryDataFromRow =
    App.unsafeBinaryDataFromBytes . binaryData

--
-- Pattern
--

patternToRow
    :: App.Pattern
    -> Text
patternToRow =
    App.patternToText

patternFromRow
    :: HasCallStack
    => Text
    -> App.Pattern
patternFromRow p =
    fromMaybe
        (error $ "patternFromRow: invalid pattern: " <> p)
        (App.patternFromText p)

patternToSql
    :: App.Pattern
    -> Text
patternToSql = \case
    App.MatchAny App.IncludingBootstrap ->
        "LIKE '%'"
    App.MatchAny App.OnlyShelley ->
        "NOT LIKE '8%'"
    App.MatchExact addr ->
        "= '" <> encodeBase16 (Ledger.serialiseAddr addr) <> "'"
    App.MatchPayment payment ->
        "LIKE '__" <> encodeBase16 payment <> "%'"
    App.MatchDelegation delegation ->
        "LIKE '%" <> encodeBase16 delegation <> "' AND len > 58"
    App.MatchPaymentAndDelegation payment delegation ->
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
