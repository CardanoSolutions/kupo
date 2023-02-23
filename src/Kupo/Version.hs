--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TemplateHaskell #-}

module Kupo.Version
    ( version
    ) where

import Kupo.Prelude

import Data.Version
    ( makeVersion
    , showVersion
    )
import Kupo.Version.TH
    ( gitRevisionTH
    )

import qualified Paths_kupo as Pkg

version :: Text
version
    | Pkg.version == makeVersion [0] =
        "nightly" <> $(gitRevisionTH)
    | otherwise =
        toText ("v" <> showVersion Pkg.version <> $(gitRevisionTH))
