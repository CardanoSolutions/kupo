--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Main where

import Kupo
import Kupo.Prelude

main :: IO ()
main = parseOptions >>= \case
    Run cfg tracers -> do
        withTracers stdout version tracers $ \tr -> do
            env <- newEnvironment cfg
            kupo tr `runWith` env
    Copy from into patterns -> do
        withTracers stdout version (defaultTracers @TracersCopy (Just Info)) $ \tr -> do
            copyDatabase (tracerCopy tr, tracerProgress tr) from into patterns
    HealthCheck host port -> do
        healthCheck host port
    Version -> do
        putTextLn version
