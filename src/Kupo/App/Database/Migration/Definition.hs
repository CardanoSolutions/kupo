--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.Database.Migration.Definition
    ( Migration (..)
    , MigrationAction (..)
    , MigrationKind (..)
    , migrationKind
    , migrationKindToText
    , BatchRepair (..)
    , BatchResult (..)
    , RepairContext (..)
    ) where

import Kupo.Prelude

import Database.SQLite.Simple
    ( Connection
    )
import Kupo.Data.Cardano.SlotNo
    ( SlotNo
    )

-- | A database migration, identified by a stable id such as @v2.10.0/001@.
data Migration = Migration
    { migrationId :: !Text
    , migrationSummary :: !Text
    , migrationAction :: !MigrationAction
    }

-- | What a migration does, which decides when it may run. SQL holds statements separated by @;@,
-- run in the migration's transaction.
data MigrationAction
    = Automatic !ByteString
    -- ^ Cheap and safe: applied at startup.
    | Blocking !ByteString
    -- ^ Applied at startup on a fresh database only; otherwise Kupo refuses to start.
    | Repair !BatchRepair
    -- ^ Fixes existing data: never applied at startup, and Kupo still starts.
    -- A repair changes rows only, never the schema: it does not run on a fresh database, so a
    -- schema change belongs in an 'Automatic' or 'Blocking' migration.

-- | The kind of a 'MigrationAction', which logs and reports name.
data MigrationKind
    = AutomaticKind
    | BlockingKind
    | RepairKind
    deriving (Generic, Eq, Show, Enum, Bounded)

migrationKind :: Migration -> MigrationKind
migrationKind Migration{migrationAction} =
    case migrationAction of
        Automatic{} -> AutomaticKind
        Blocking{} -> BlockingKind
        Repair{} -> RepairKind

migrationKindToText :: MigrationKind -> Text
migrationKindToText = \case
    AutomaticKind -> "automatic"
    BlockingKind -> "blocking"
    RepairKind -> "repair"

-- | A data repair that runs in batches. Each batch commits together with the cursor it returns.
data BatchRepair = BatchRepair
    { repairDataIntroducedBy :: Text
    -- ^ The id of the migration that introduced the data to repair. When the same plan applies that
    -- migration, the database has no such data, and the repair is skipped.
    , repairPrecondition :: Connection -> RepairContext -> IO (Either Text ())
    -- ^ Whether the repair can run on this database, or why not. Reads only.
    , repairEstimate :: Connection -> RepairContext -> IO Text
    -- ^ What the repair costs, from cheap statistics only (never a @COUNT(*)@). Reads only.
    , repairBatch :: Connection -> RepairContext -> Maybe ByteString -> IO BatchResult
    -- ^ Repair one batch from a cursor, 'Nothing' at the start. A batch need not be idempotent:
    -- the runner never repeats one.
    }

-- | What a batch leaves to do.
data BatchResult
    = NextBatch !ByteString
    -- ^ The cursor that the next batch starts from.
    | RepairDone !Text
    -- ^ The repair is done. The text sums up what it did, for the operator.
    deriving (Generic, Eq, Show)

-- | What a repair knows about the database it repairs.
data RepairContext = RepairContext
    { repairCutoffSlot :: !SlotNo
    -- ^ The latest checkpoint when the repair was registered; later data needs no repair.
    , repairCompleteIndexConfirmed :: !Bool
    -- ^ The operator confirms that the index matches @*@ from origin, which the data cannot prove.
    }
