--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.Http.GetCheckpointMode
    ( GetCheckpointMode (..)
    , getCheckpointModeFromQuery
    ) where

import Kupo.Prelude

import qualified Network.HTTP.Types as Http

data GetCheckpointMode
    = GetCheckpointStrict
    | GetCheckpointClosestAncestor

getCheckpointModeFromQuery
    :: Http.Query
    -> Maybe GetCheckpointMode
getCheckpointModeFromQuery = \case
    [("strict", Nothing)] -> Just GetCheckpointStrict
    [] -> Just GetCheckpointClosestAncestor
    _ -> Nothing
