--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.Http.SlotRange
    ( Range (..)
    , RangeField (..)
    , intoSlotRange
    , slotRangeFromQueryParams
    ) where

import Kupo.Prelude

import Kupo.Data.Cardano.Point
    ( Point
    , getPointSlotNo
    , pointFromText
    )
import Kupo.Data.Cardano.SlotNo
    ( SlotNo (..)
    , slotNoFromText
    )

import qualified Network.HTTP.Types.URI as Http

-- | A basic range/closed interval with optional bounds.
data Range a
    = Whole
    | After RangeField a
    | Before RangeField a
    | Between (RangeField, a) (RangeField, a)
    deriving (Generic, Show, Eq)

-- | What field is the range on.
data RangeField = CreatedAt | SpentAt
    deriving (Generic, Show, Eq, Ord)

setLowerBound
    :: MonadFail m
    => RangeField
    -> Range a
    -> a
    -> m (Range a)
setLowerBound lowerField range lowerBound =
    case range of
        Whole ->
            pure (After lowerField lowerBound)
        Before upperField upperBound ->
            pure (Between (lowerField, lowerBound) (upperField, upperBound))
        _ -> fail "lower bound already set"

setUpperBound
    :: MonadFail m
    => RangeField
    -> Range a
    -> a
    -> m (Range a)
setUpperBound upperField range upperBound =
    case range of
        Whole ->
            pure (Before upperField upperBound)
        After lowerField lowerBound ->
            pure (Between (lowerField, lowerBound) (upperField, upperBound))
        _ -> fail "upper bound already set"

-- | Convert a range of SlotNo/Point to a mere 'Range SlotNo', but performs action with points when
-- they're provided. This allows, for example, running extra checks on full points but then fallback
-- on an more homogeneous range for downstream manipulation.
intoSlotRange
    :: forall m. (Monad m)
    => (Point -> m ())
        -- ^ Action to perform when lower-bound is a 'Point'
    -> (Point -> m ())
        -- ^ Action to perform when upper-bound is a 'Point'
    -> Range (Either SlotNo Point)
        -- ^ A flexible range
    -> m (Range SlotNo)
intoSlotRange withLowerPoint withUpperPoint = \case
    Whole ->
        pure Whole

    After lowerField lowerBound ->
        After lowerField <$> intoSlotBound withLowerPoint lowerBound

    Before upperField upperBound ->
        Before upperField <$> intoSlotBound withUpperPoint upperBound

    Between (lowerField, lowerBound) (upperField, upperBound) -> do
        lowerBoundSlot <- intoSlotBound withLowerPoint lowerBound
        upperBoundSlot <- intoSlotBound withUpperPoint upperBound
        pure $ Between (lowerField, lowerBoundSlot) (upperField, upperBoundSlot)
  where
    intoSlotBound :: (Point -> m ()) -> Either SlotNo Point -> m SlotNo
    intoSlotBound withPoint = \case
        Left slot ->
            pure slot
        Right point ->
            withPoint point $> getPointSlotNo point

-- | Parse a 'Range' from query parameters; the range can be specified as either full points or just
-- slot numbers; or a mix of both.
slotRangeFromQueryParams
    :: Http.Query
    -> Maybe (Range (Either SlotNo Point))
slotRangeFromQueryParams = \case
    ("created_after", param):rest -> do
        range <- slotRangeFromQueryParams rest
        setLowerBound CreatedAt range =<< slotOrPoint param

    ("created_before", param):rest -> do
        range <- slotRangeFromQueryParams rest
        setUpperBound CreatedAt range =<< slotOrPoint param

    ("spent_after", param):rest -> do
        range <- slotRangeFromQueryParams rest
        setLowerBound SpentAt range =<< slotOrPoint param

    ("spent_before", param):rest -> do
        range <- slotRangeFromQueryParams rest
        setUpperBound SpentAt range =<< slotOrPoint param

    [] ->
        pure Whole

    _:rest ->
        slotRangeFromQueryParams rest
  where
    slotOrPoint param =
      asum
          [ fmap Left . slotNoFromText . decodeUtf8 =<< param
          , fmap Right . pointFromText . decodeUtf8 =<< param
          ]
