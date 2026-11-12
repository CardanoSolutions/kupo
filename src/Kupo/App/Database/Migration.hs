--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.Database.Migration
    ( -- * Migrations
      migrations
    ) where

import Kupo.App.Database.Migration.Definition
    ( Migration (..)
    )

import qualified Kupo.App.Database.Migration.V1_0_0_001 as V1_0_0_001
import qualified Kupo.App.Database.Migration.V1_0_0_002 as V1_0_0_002
import qualified Kupo.App.Database.Migration.V1_0_0_beta_001 as V1_0_0_beta_001
import qualified Kupo.App.Database.Migration.V1_0_1_001 as V1_0_1_001
import qualified Kupo.App.Database.Migration.V2_0_0_beta_001 as V2_0_0_beta_001
import qualified Kupo.App.Database.Migration.V2_10_0_001 as V2_10_0_001
import qualified Kupo.App.Database.Migration.V2_1_0_001 as V2_1_0_001
import qualified Kupo.App.Database.Migration.V2_1_0_002 as V2_1_0_002
import qualified Kupo.App.Database.Migration.V2_1_0_003 as V2_1_0_003
import qualified Kupo.App.Database.Migration.V2_2_0_001 as V2_2_0_001

--
-- Migrations
--

-- | Every migration, oldest first. Append only: the position of an entry is its @user_version@.
migrations :: [Migration]
migrations =
    [ V1_0_0_beta_001.migration
    , V1_0_0_001.migration
    , V1_0_0_002.migration
    , V1_0_1_001.migration
    , V2_0_0_beta_001.migration
    , V2_1_0_001.migration
    , V2_1_0_002.migration
    , V2_1_0_003.migration
    , V2_2_0_001.migration
    , V2_10_0_001.migration
    ]
