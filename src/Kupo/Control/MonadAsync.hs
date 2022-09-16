--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Control.MonadAsync
    ( MonadAsync (..)
    , concurrently4
    , forConcurrently_
    , mapConcurrently_
    ) where

import Control.Monad.Class.MonadAsync
    ( MonadAsync (..)
    , forConcurrently_
    , mapConcurrently_
    )

concurrently4 :: MonadAsync m => m a -> m b -> m c -> m d -> m ()
concurrently4 a b c d =
    concurrently_ a
        ( concurrently_ b
            ( concurrently_ c
                d
            )
        )
