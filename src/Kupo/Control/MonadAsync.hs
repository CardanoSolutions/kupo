--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Control.MonadAsync
    ( MonadAsync (..)
    , concurrently3
    ) where

import Control.Monad.Class.MonadAsync
    ( MonadAsync (..) )

concurrently3 :: MonadAsync m => m a -> m b -> m c -> m ()
concurrently3 a b c = concurrently_ a (concurrently_ b c)
