--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.Http.SlotRange
    ( Range
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
type Range a = (Maybe a, Maybe a)

-- | Convert a range of SlotNo/Point to a mere 'Range SlotNo', but performs action with points when
-- they're provided. This allows, for example, running extra checks on full points but then fallback
-- on an more homogeneous range for downstream manipulation.
intoSlotRange
    :: forall m. (Applicative m)
    => Range (Either SlotNo Point)
        -- ^ A flexible range
    -> (Point -> m ())
        -- ^ Action to perform when lower-bound is a 'Point'
    -> (Point -> m ())
        -- ^ Action to perform when upper-bound is a 'Point'
    -> m (Range SlotNo)
intoSlotRange (lower, upper) withLowerPoint withUpperPoint = (,)
    <$> traverse (intoSlotBound withLowerPoint) lower
    <*> traverse (intoSlotBound withUpperPoint) upper
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
    ("created_before", param):rest -> do
        (lower, upper') <- slotRangeFromQueryParams rest
        guard (isNothing upper')
        upper <- asum
            [ fmap Left . slotNoFromText . decodeUtf8 =<< param
            , fmap Right . pointFromText . decodeUtf8 =<< param
            ]
        pure (lower, Just upper)
    ("created_after", param):rest -> do
        (lower', upper) <- slotRangeFromQueryParams rest
        guard (isNothing lower')
        lower <- asum
            [ fmap Left . slotNoFromText . decodeUtf8 =<< param
            , fmap Right . pointFromText . decodeUtf8 =<< param
            ]
        pure (Just lower, upper)
    [] ->
        pure (Nothing, Nothing)
    _:rest ->
        slotRangeFromQueryParams rest
