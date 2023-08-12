--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Control.MonadTime
    ( MonadTime (..)
    , DiffTime
    , secondsToDiffTime
    , millisecondsToDiffTime
    , diffTimeToMicroseconds
    , timeout
    ) where

import Kupo.Prelude

import Control.Monad.Class.MonadTime
    ( MonadTime (..)
    )
import Control.Monad.Class.MonadTimer
    ( timeout
    )
import Data.Time.Clock
    ( DiffTime
    , diffTimeToPicoseconds
    , picosecondsToDiffTime
    , secondsToDiffTime
    )

millisecondsToDiffTime :: Integer -> DiffTime
millisecondsToDiffTime = picosecondsToDiffTime . (* 1_000_000_000)

diffTimeToMicroseconds :: DiffTime -> Integer
diffTimeToMicroseconds = (`div` 1_000_000) . diffTimeToPicoseconds
