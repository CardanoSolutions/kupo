--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Control.MonadDelay
    ( MonadDelay (..)
    , foreverCalmly
    ) where

import Kupo.Prelude

import Control.Monad.Class.MonadTimer
    ( MonadDelay (..)
    )
import Kupo.Control.MonadTime
    ( DiffTime
    )

foreverCalmly :: (MonadDelay m) => m DiffTime -> m Void
foreverCalmly a = do
    let a' = a >>= threadDelay >> a' in a'
