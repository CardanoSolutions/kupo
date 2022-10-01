--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.FetchBlock where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( Point
    )

--  | A simple block fetch client, that retrieves blocks from a given point.
type FetchBlockClient m block =
    Point -> (Maybe block -> m ()) -> m ()
