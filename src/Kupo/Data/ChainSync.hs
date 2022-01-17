--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}

module Kupo.Data.ChainSync
    ( -- * Point
      Point (..)
    , pattern GenesisPoint

      -- * Tip
    , Tip (..)
      -- * Constraints
    , IsBlock
    ) where

import Kupo.Prelude

import Ouroboros.Network.Block
    ( pattern GenesisPoint, Point (..), StandardHash, Tip (..) )

type IsBlock block =
    ( StandardHash block
    , Typeable block
    )
