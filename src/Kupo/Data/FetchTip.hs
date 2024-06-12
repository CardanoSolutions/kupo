--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.FetchTip where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( Tip
    )

data UnableToFetchTipFromReadOnlyReplicaException
    = UnableToFetchTipFromReadOnlyReplica
    deriving (Generic, Show, Eq)

instance Exception UnableToFetchTipFromReadOnlyReplicaException

data UnableToFetchTipFromHydraException
    = UnableToFetchTipFromHydra
    deriving (Generic, Show, Eq)

instance Exception UnableToFetchTipFromHydraException

--  | A simple tip fetch client
type FetchTipClient m = m Tip
