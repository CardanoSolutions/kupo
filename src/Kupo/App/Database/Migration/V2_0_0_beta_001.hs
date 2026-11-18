--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TemplateHaskell #-}

module Kupo.App.Database.Migration.V2_0_0_beta_001
    ( migration
    ) where

import Data.FileEmbed
    ( embedFile
    )
import Kupo.App.Database.Migration.Definition
    ( Migration (..)
    , MigrationAction (..)
    )

migration :: Migration
migration = Migration
    { migrationId = "v2.0.0-beta/001"
    , migrationSummary = "Recreate inputs with spent_at, add binary_data and scripts tables"
    , migrationAction = Automatic $(embedFile "db/v2.0.0-beta/001.sql")
    }
