--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Control.MonadTime
    ( MonadTime (..)
    , Time (..)
    , DiffTime
    , addTime
    , diffTime
    , secondsToDiffTime
    , millisecondsToDiffTime
    , timeout
    ) where

import Kupo.Prelude

import Control.Monad.Class.MonadTime
    ( MonadTime (..)
    , Time (..)
    , addTime
    , diffTime
    )
import Control.Monad.Class.MonadTimer
    ( timeout
    )
import Data.Time.Clock
    ( DiffTime
    , secondsToDiffTime
    )

millisecondsToDiffTime :: Integer -> DiffTime
millisecondsToDiffTime = toEnum . fromInteger . (* 1_000_000_000)
