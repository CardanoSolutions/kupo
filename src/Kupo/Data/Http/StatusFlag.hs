--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.Http.StatusFlag
    ( StatusFlag (..)
    , isNoStatusFlag
    , hideTransientGhostInputs
    , statusFlagFromQueryParams
    ) where

import Kupo.Prelude

import Kupo.Data.Configuration
    ( InputManagement (..)
    )
import qualified Network.HTTP.Types.URI as Http

data StatusFlag
    = NoStatusFlag
    | OnlySpent
    | OnlyUnspent

isNoStatusFlag :: StatusFlag -> Bool
isNoStatusFlag = \case
    NoStatusFlag -> True
    OnlyUnspent -> False
    OnlySpent -> False

-- | Because the chain is only eventually immutable, kupo does not remove recent
-- data right away. Thus, it is possible that, even though the configuration
-- asked to prune all spent inputs, some remains present in the database and
-- marked as 'spent'. Yet, this is an implementation detail and we therefore
-- gives clients the illusion that there's no 'spent' inputs in the database if
-- they're running with the 'RemoveSpentInputs' option enabled.
hideTransientGhostInputs
    :: InputManagement
    -> StatusFlag
    -> StatusFlag
hideTransientGhostInputs inputManagement statusFlag =
    case inputManagement of
        MarkSpentInputs ->
           statusFlag
        RemoveSpentInputs ->
            OnlyUnspent

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
