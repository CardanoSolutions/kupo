--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.Http.StatusFlag
    ( StatusFlag (..)
    , isNoStatusFlag
    , statusFlagFromQueryParams
    ) where

import Kupo.Prelude

import qualified Network.HTTP.Types.URI as Http

data StatusFlag
    = NoStatusFlag
    | OnlySpent
    | OnlyUnspent
    deriving (Show)

isNoStatusFlag :: StatusFlag -> Bool
isNoStatusFlag = \case
    NoStatusFlag -> True
    OnlyUnspent -> False
    OnlySpent -> False

statusFlagFromQueryParams
    :: Http.Query
    -> Maybe StatusFlag
statusFlagFromQueryParams = \case
    ("spent", val):rest -> do
        guard (isNothing val)
        guardM (isNoStatusFlag <$> statusFlagFromQueryParams rest)
        Just OnlySpent
    ("unspent", val):rest -> do
        guard (isNothing val)
        guardM (isNoStatusFlag <$> statusFlagFromQueryParams rest)
        Just OnlyUnspent
    [] ->
        Just NoStatusFlag
    _:rest ->
        statusFlagFromQueryParams rest
