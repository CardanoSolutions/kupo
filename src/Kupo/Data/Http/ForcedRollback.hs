--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.Http.ForcedRollback
    ( -- * ForcedRollback
      ForcedRollback (..)
    , forcedRollbackToJson
    , decodeForcedRollback
      -- ** ForcedRollbackLimit
    , ForcedRollbackLimit (..)
    , forcedRollbackLimitToJson
    , decodeForcedRollbackLimit
    ) where

import Kupo.Prelude

import Data.Aeson
    ( (.!=)
    , (.:)
    , (.:?)
    )
import Kupo.Data.Cardano
    ( Point
    , SlotNo (..)
    , pointFromText
    , pointToJson
    , slotNoFromText
    , slotNoToJson
    )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Types as Json

data ForcedRollback = ForcedRollback
    { since :: !(Either SlotNo Point)
    , limit :: !ForcedRollbackLimit
    } deriving (Generic, Show, Eq)

forcedRollbackToJson :: ForcedRollback -> Json.Encoding
forcedRollbackToJson ForcedRollback{since,limit} = Json.pairs $ mconcat
    [ Json.pair "rollback_to" (either singletonSlotNo pointToJson since)
    , Json.pair "limit" (forcedRollbackLimitToJson limit)
    ]
  where
    singletonSlotNo = Json.pairs . Json.pair "slot_no" . slotNoToJson

decodeForcedRollback :: Json.Value -> Json.Parser ForcedRollback
decodeForcedRollback =
    Json.withObject "ForcedRollback" $ \o -> do
        since <- (o .: "rollback_to") >>= decodePointOrSlotNo
        limit <- ((o .:? "limit") >>= traverse decodeForcedRollbackLimit) .!= OnlyAllowRollbackWithinSafeZone
        pure $ ForcedRollback { since, limit }
  where
    decodePointOrSlotNo =
        Json.withObject "PointOrSlotNo" $ \o -> do
            (slotNo :: Word) <- o .: "slot_no"
            mHeaderHash <- o .:? "header_hash"
            case mHeaderHash of
                Nothing ->
                    case slotNoFromText (show slotNo) of
                        Nothing -> fail "decodePointOrSlotNo(SlotNo)"
                        Just sl -> pure (Left sl)
                Just headerHash ->
                    case pointFromText (show slotNo <> "." <> headerHash) of
                        Nothing -> fail "decodePointOrSlotNo(Point)"
                        Just pt -> pure (Right pt)

-- ** ForcedRollbackLimit

data ForcedRollbackLimit
    = UnsafeAllowRollbackBeyondSafeZone
    | OnlyAllowRollbackWithinSafeZone
    deriving (Generic, Show, Eq)

forcedRollbackLimitToJson :: ForcedRollbackLimit -> Json.Encoding
forcedRollbackLimitToJson = \case
    UnsafeAllowRollbackBeyondSafeZone ->
        Json.text "unsafe_allow_beyond_safe_zone"
    OnlyAllowRollbackWithinSafeZone ->
        Json.text "within_safe_zone"

decodeForcedRollbackLimit :: Json.Value -> Json.Parser ForcedRollbackLimit
decodeForcedRollbackLimit =
    Json.withText "ForcedRollbackLimit" $ \case
        "unsafe_allow_beyond_safe_zone" ->
            pure UnsafeAllowRollbackBeyondSafeZone
        "within_safe_zone" ->
            pure OnlyAllowRollbackWithinSafeZone
        _otherwise ->
            fail "Invalid limit. Must be either of 'unsafe_allow_beyond_safe_zone' or 'within_safe_zone'."
