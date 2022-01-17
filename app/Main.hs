--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Main where

import Kupo
import Kupo.Prelude

main :: IO ()
main = parseOptions >>= \case
    Version -> do
        putTextLn version
    Run (Identity ntwrk) cfg -> do
        env <- newEnvironment ntwrk cfg
        kupo `runWith` env
