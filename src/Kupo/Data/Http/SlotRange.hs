--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.Http.SlotRange
    ( Range
    , slotRangeFromQueryParams
    ) where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( SlotNo (..)
    , slotNoFromText
    )
import qualified Network.HTTP.Types.URI as Http

type Range a = (Maybe a, Maybe a)

slotRangeFromQueryParams
    :: Http.Query
    -> Maybe (Range SlotNo)
slotRangeFromQueryParams = \case
    ("created_before", param):rest -> do
        (lower, upper') <- slotRangeFromQueryParams rest
        guard (isNothing upper')
        upper <- slotNoFromText . decodeUtf8 =<< param
        pure (lower, Just upper)
    ("created_after", param):rest -> do
        (lower', upper) <- slotRangeFromQueryParams rest
        guard (isNothing lower')
        lower <- slotNoFromText . decodeUtf8 =<< param
        pure (Just lower, upper)
    [] ->
        pure (Nothing, Nothing)
    _:rest ->
        slotRangeFromQueryParams rest
