--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.ChainSync
    ( -- * Types
      IntersectionNotFoundException (..)

      -- * Tracer
    , TraceChainSync (..)
    ) where

import Kupo.Prelude

import Data.Severity
    ( HasSeverityAnnotation (..)
    , Severity (..)
    )
import Kupo.Data.Cardano
    ( SlotNo
    , WithOrigin (..)
    )
import Kupo.Data.ChainSync
    ( IntersectionNotFoundException (..)
    )

--
-- Tracer
--

data TraceChainSync where
    ChainSyncIntersectionNotFound
        :: { points :: [WithOrigin SlotNo] }
        -> TraceChainSync
    ChainSyncUnknownException
        :: { exception :: Text }
        -> TraceChainSync
    deriving stock (Generic, Show)

instance ToJSON TraceChainSync where
    toEncoding =
        defaultGenericToEncoding

instance HasSeverityAnnotation TraceChainSync where
    getSeverityAnnotation = \case
        ChainSyncIntersectionNotFound{} ->
            Error
        ChainSyncUnknownException{} ->
            Error
