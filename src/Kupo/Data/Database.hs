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

      -- * Filtering
    , statusToSql
    ) where

import Kupo.Prelude

import Cardano.Crypto.Hash
    ( pattern UnsafeHash )
import Cardano.Ledger.Alonzo
    ( AlonzoEra )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Slotting.Slot
    ( SlotNo (..) )
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
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( OneEraHash (..) )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..), ShelleyHash (..) )
import Ouroboros.Network.Block
    ( pattern BlockPoint, pattern GenesisPoint, Point (..) )

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Kupo.Control.MonadDatabase as DB
import qualified Network.HTTP.Types.URI as Http

--
-- Checkpoint
--

pointFromRow
    :: DB.Checkpoint
    -> Point (CardanoBlock StandardCrypto)
pointFromRow row = BlockPoint
    (SlotNo (DB.checkpointSlotNo row))
    (fromShelleyHash $ fromShortRawHash proxy $ toShort $ DB.checkpointHeaderHash row)
  where
    proxy = Proxy @(ShelleyBlock (AlonzoEra StandardCrypto))
    fromShelleyHash (Ledger.unHashHeader . unShelleyHash -> UnsafeHash h) =
        coerce h

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

--
-- Filters
--

statusToSql
    :: Http.Query
    -> Maybe Text
statusToSql = \case
    [ ("spent", Nothing) ] ->
        Just "spent_at IS NOT NULL"
    [ ("unspent", Nothing) ] ->
        Just "spent_at IS NULL"
    [] ->
        Just ""
    _ ->
        Nothing
