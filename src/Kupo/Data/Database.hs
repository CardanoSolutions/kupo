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

--
-- Checkpoint
--

data Checkpoint = Checkpoint
    { checkpointHeaderHash :: ByteString
    , checkpointSlotNo :: Word64
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
    { outputReference :: ByteString
    , address :: Text
    , value :: ByteString
    , datum :: Maybe BinaryData
    , datumHash :: Maybe ByteString
    , refScript :: Maybe ScriptReference
    , refScriptHash :: Maybe ByteString
    , createdAtSlotNo :: Word64
    , createdAtHeaderHash :: ByteString
    , spentAtSlotNo :: Maybe Word64
    , spentAtHeaderHash :: Maybe ByteString
    } deriving (Show)

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
    , App.scriptReference =
        scriptReferenceFromRow (refScriptHash row) (refScript row)
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
resultToRow x =
    Input {..}
  where
    outputReference =
        serialize' (App.outputReference x)

    address =
        encodeBase16 (Ledger.serialiseAddr (App.address x))

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
-- BinaryData / Datum
--

data BinaryData = BinaryData
    { binaryDataHash :: ByteString
    , binaryData :: ByteString
    } deriving (Show)

datumFromRow
    :: Maybe ByteString
    -> Maybe BinaryData
    -> App.Datum
datumFromRow hash = \case
    Just BinaryData{..} ->
        Ledger.Datum (App.unsafeBinaryDataFromBytes binaryData)
    Nothing ->
        maybe App.noDatum (Ledger.DatumHash . App.unsafeDatumHashFromBytes) hash
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
    Ledger.originalBytes
{-# INLINABLE datumHashToRow #-}

binaryDataToRow
    :: App.DatumHash
    -> App.BinaryData
    -> BinaryData
binaryDataToRow hash bin = BinaryData
    { binaryDataHash = datumHashToRow hash
    , binaryData = Ledger.originalBytes (Ledger.binaryDataToData bin)
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
    { scriptHash :: ByteString
    , script :: ByteString
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
