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
    , patternToQueryLike
    ) where

import Kupo.Prelude

import Kupo.Data.Pattern
    ( MatchBootstrap (..), Pattern (..), Result (..) )

import Cardano.Crypto.Hash
    ( pattern UnsafeHash )
import Cardano.Ledger.Alonzo
    ( AlonzoEra )
import Cardano.Ledger.Shelley.API
    ( PraosCrypto )
import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Ouroboros.Consensus.Block
    ( ConvertRawHash (..) )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock )
import Ouroboros.Consensus.Cardano.CanHardFork
    ( CardanoHardForkConstraints )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( OneEraHash (..) )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..), ShelleyHash (..) )
import Ouroboros.Network.Block
    ( pattern BlockPoint, pattern GenesisPoint, Point (..) )

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Kupo.Control.MonadDatabase as DB

--
-- Checkpoint
--

pointFromRow
    :: forall crypto.
        ( PraosCrypto crypto
        )
    => DB.Checkpoint
    -> Point (CardanoBlock crypto)
pointFromRow row = BlockPoint
    (SlotNo (DB.checkpointSlotNo row))
    (fromShelleyHash $ fromShortRawHash proxy $ toShort $ DB.checkpointHeaderHash row)
  where
    proxy = Proxy @(ShelleyBlock (AlonzoEra crypto))
    fromShelleyHash (Ledger.unHashHeader . unShelleyHash -> UnsafeHash h) =
        coerce h

pointToRow
    :: forall crypto.
        ( HasCallStack
        , CardanoHardForkConstraints crypto
        )
    => Point (CardanoBlock crypto)
    -> DB.Checkpoint
pointToRow = \case
    GenesisPoint -> error "pointToRow: genesis point."
    BlockPoint slotNo headerHash -> DB.Checkpoint
        { DB.checkpointHeaderHash = toRawHash proxy headerHash
        , DB.checkpointSlotNo = unSlotNo slotNo
        }
  where
    proxy = Proxy @(CardanoBlock crypto)

--
-- Result
--

resultFromRow
    :: forall crypto.
        ( HasCallStack
        , PraosCrypto crypto
        )
    => DB.Input
    -> Result crypto
resultFromRow row = Result
    { outputReference = unsafeDeserialize' (DB.outputReference row)
    , address = (unsafeAddressFromBytes . unsafeDecodeBase16)  (DB.address row)
    , value = unsafeDeserialize' (DB.value row)
    , datumHash = unsafeDeserialize' <$> (DB.datumHash row)
    , point = pointFromRow (DB.Checkpoint (DB.headerHash row) (DB.slotNo row))
    }
  where
    unsafeAddressFromBytes =
        fromMaybe (error "unsafeAddressFromBytes") . Ledger.deserialiseAddr

resultToRow
    :: forall crypto.
        ( PraosCrypto crypto
        , CardanoHardForkConstraints crypto
        )
    => Result crypto
    -> DB.Input
resultToRow Result{..} = DB.Input
    { DB.outputReference = serialize' outputReference
    , DB.address = encodeBase16 (Ledger.serialiseAddr address)
    , DB.value = serialize' value
    , DB.datumHash = serialize' <$> datumHash
    , DB.headerHash = checkpointHeaderHash
    , DB.slotNo = checkpointSlotNo
    }
  where
    DB.Checkpoint
        { checkpointHeaderHash
        , checkpointSlotNo
        } = pointToRow point

--
-- Pattern
--

patternToQueryLike :: Pattern crypto -> Text
patternToQueryLike = \case
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
