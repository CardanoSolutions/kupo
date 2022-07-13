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
    , patternContainsSql

      -- * Filtering
    , StatusFlag (..)
    , statusFlagFromQueryParams
    , isNoStatusFlag
    , applyStatusFlag
    ) where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( Block, SlotNo (..), StandardCrypto, getDelegationPartBytes, getPaymentPartBytes )
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
import qualified Kupo.Control.MonadDatabase as DB
import qualified Network.HTTP.Types.URI as Http
import qualified Data.Text as Text

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
    { outputReference = unsafeDeserialize' (DB.outputReference row)
    , address = (unsafeAddressFromBytes . unsafeDecodeBase16)  (DB.address row)
    , value = unsafeDeserialize' (DB.value row)
    , datumHash = unsafeDeserialize' <$> (DB.datumHash row)
    , createdAt = pointFromRow (DB.Checkpoint (DB.createdAtHeaderHash row) (DB.createdAtSlotNo row))
    , spentAt = pointFromRow <$> (DB.Checkpoint <$> DB.spentAtHeaderHash row <*> DB.spentAtSlotNo row)
    }
  where
    unsafeAddressFromBytes =
        fromMaybe (error "unsafeAddressFromBytes") . Ledger.deserialiseAddr

resultToRow
    :: Result
    -> DB.Input
resultToRow Result{..} = DB.Input
    { DB.outputReference = serialize' outputReference
    , DB.address = encodeBase16 (Ledger.serialiseAddr address)
    , DB.value = serialize' value
    , DB.datumHash = serialize' <$> datumHash
    , DB.createdAtSlotNo = DB.checkpointSlotNo createdAtRow
    , DB.createdAtHeaderHash = DB.checkpointHeaderHash createdAtRow
    , DB.spentAtSlotNo = DB.checkpointSlotNo <$> spentAtRow
    , DB.spentAtHeaderHash = DB.checkpointHeaderHash <$> spentAtRow
    }
  where
    createdAtRow = pointToRow createdAt
    spentAtRow = pointToRow <$> spentAt

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


-- querying whether the given pattern is covered entirely by a pattern stored in the DB
patternContainsPatterns
    :: Pattern
    -> [Text]
patternContainsPatterns = \case
    MatchAny IncludingBootstrap ->
        ["*"]
    MatchAny OnlyShelley ->
        "*/*" : (patternContainsPatterns (MatchAny IncludingBootstrap))
    MatchPayment payment ->
        encodeBase16 payment <> "/*" : (patternContainsPatterns (MatchAny OnlyShelley))
    MatchDelegation delegation ->
        "*/" <> encodeBase16 delegation : (patternContainsPatterns (MatchAny OnlyShelley))
    MatchPaymentAndDelegation payment delegation ->
        encodeBase16 payment <> "/" <> encodeBase16 delegation : (
            patternContainsPatterns (MatchPayment payment) <>
            patternContainsPatterns (MatchDelegation delegation)
        )
    MatchExact addr ->
        encodeBase16 (Ledger.serialiseAddr addr) :
        case (getPaymentPartBytes addr) of
        Nothing ->
            patternContainsPatterns (MatchAny IncludingBootstrap)
        Just payment -> 
             case (getDelegationPartBytes addr) of
                Nothing ->
                    patternContainsPatterns (MatchPayment payment)
                Just delegation -> 
                    patternContainsPatterns (MatchPaymentAndDelegation payment delegation)


patternContainsSql
    :: Pattern
    -> Text
patternContainsSql p = "IN ('" <> Text.intercalate "','" (patternContainsPatterns p) <> "')"

--
-- Filters
--

data StatusFlag
    = NoStatusFlag
    | StatusFlag (Text -> Text)

isNoStatusFlag :: StatusFlag -> Bool
isNoStatusFlag = \case
    NoStatusFlag -> True
    _ -> False

applyStatusFlag :: StatusFlag -> Text -> Text
applyStatusFlag = \case
    NoStatusFlag  -> identity
    StatusFlag fn -> fn

statusFlagFromQueryParams
    :: Http.Query
    -> Maybe StatusFlag
statusFlagFromQueryParams = \case
    ("spent", val):rest -> do
        guard (isNothing val)
        guardM (isNoStatusFlag <$> statusFlagFromQueryParams rest)
        pure (StatusFlag (<> " AND spent_at IS NOT NULL"))
    ("unspent", val):rest -> do
        guard (isNothing val)
        guardM (isNoStatusFlag <$> statusFlagFromQueryParams rest)
        pure (StatusFlag (<> " AND spent_at IS NULL"))
    [] ->
        Just NoStatusFlag
    _:rest ->
        statusFlagFromQueryParams rest
