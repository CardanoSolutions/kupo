--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.Http.ReferenceFlag
    ( ReferenceFlag (..)
    , referenceFlagFromQueryParams
    ) where

import Kupo.Prelude

import qualified Network.HTTP.Types.URI as Http

data ReferenceFlag
    = AsReference
    | InlineAll
    deriving (Show)

referenceFlagFromQueryParams
    :: Http.Query
    -> Maybe ReferenceFlag
referenceFlagFromQueryParams = \case
    ("resolve_hashes", val):_ -> do
        guard (isNothing val)
        Just InlineAll
    [] ->
        Just AsReference
    _:rest ->
        referenceFlagFromQueryParams rest
