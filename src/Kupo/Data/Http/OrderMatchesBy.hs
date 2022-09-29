--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.Http.OrderMatchesBy
    ( OrderMatchesBy (..)
    , orderMatchesBy
    ) where

import Kupo.Prelude

import qualified Network.HTTP.Types.URI as Http

data OrderMatchesBy = MostRecentFirst | OldestFirst | NoExplicitOrder
    deriving (Eq, Show, Generic)

-- | Creates a 'OrderMatchesBy' from query parameters. This search for zero or one query parameter.
--
-- For examples:
--
-- ✓ ?
-- ✓ ?order=most_recent_first
-- ✓ ?order=oldest_first
-- x ?order=foo
-- x ?order
-- x ?order=most_recent_first&order=oldest_first
orderMatchesBy
    :: Http.Query
    -> Maybe OrderMatchesBy
orderMatchesBy = \case
    ("order", fmap decodeUtf8' -> Just (Right val)):rest | val == "most_recent_first" -> do
        guard (Just NoExplicitOrder == orderMatchesBy rest)
        Just MostRecentFirst
    ("order", fmap decodeUtf8' -> Just (Right val)):rest | val == "oldest_first" -> do
        guard (Just NoExplicitOrder == orderMatchesBy rest)
        Just OldestFirst
    ("order", _):_ ->
        Nothing
    [] ->
        Just NoExplicitOrder
    _:rest ->
        orderMatchesBy rest
