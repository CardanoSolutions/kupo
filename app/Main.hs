--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Main where

import Kupo
import Kupo.Prelude

main :: IO ()
main = parseOptions >>= \case
    Run (Identity ntwrk) cfg tracers -> do
        withTracers stdout version tracers $ \tr -> do
            env <- newEnvironment tr ntwrk cfg
            kupo tr `runWith` env
    HealthCheck host port -> do
        healthCheck host port
    Version -> do
        putTextLn version
