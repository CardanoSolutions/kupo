--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.Database.Migration.Status
    ( StatusReport (..)
    , MigrationStatus (..)
    , renderStatus
    , statusExitCode
    ) where

import Kupo.Prelude

import Kupo.App.Database.Migration.Definition
    ( MigrationKind (..)
    , migrationKindToText
    )
import Kupo.App.Database.Migration.Plan
    ( MigrationState (..)
    , UnknownMigration
    , describeUnknownMigration
    , exitCodeMigrationRequired
    , migrationRequiredHint
    , migrationStateToText
    , unknownMigrationsHint
    )
import System.Exit
    ( ExitCode (..)
    )

import qualified Data.Text as T

-- | What @kupo db status@ reports about a database.
data StatusReport = StatusReport
    { reportMigrations :: ![MigrationStatus]
    -- ^ Every known migration, oldest first.
    , reportBlocked :: ![Text]
    -- ^ The migrations that stop Kupo from starting, if any.
    , reportUnknown :: ![UnknownMigration]
    -- ^ Registry rows this version does not know: a newer Kupo migrated the database.
    }
    deriving (Generic, Eq, Show)

-- | What the database records about one migration.
data MigrationStatus = MigrationStatus
    { statusId :: !Text
    , statusKind :: !MigrationKind
    , statusState :: !(Maybe MigrationState)
    -- ^ 'Nothing' until the migration is applied, or registered.
    , statusSummary :: !Text
    , statusEstimate :: !(Maybe Text)
    -- ^ What the remaining work costs, for a repair that is not done.
    , statusCursor :: !(Maybe ByteString)
    -- ^ Where an interrupted repair resumes.
    }
    deriving (Generic, Eq, Show)

-- | One line per migration, with details below a repair that is not done, then hints.
renderStatus
    :: StatusReport
    -- ^ The report to render.
    -> Text
renderStatus StatusReport{reportMigrations, reportBlocked, reportUnknown} =
    T.unlines $ mconcat
        [ concatMap renderMigration reportMigrations
        , unknownSection
        , hints
        ]
  where
    renderMigration MigrationStatus{statusId, statusKind, statusState, statusSummary, statusEstimate, statusCursor} =
        T.intercalate "  "
            [ T.justifyLeft idWidth ' ' statusId
            , T.justifyLeft kindWidth ' ' $ migrationKindToText statusKind
            , T.justifyLeft stateWidth ' ' $ stateToText statusState
            , statusSummary
            ]
        : mconcat
            [ ("    estimate: " <>) <$> toList statusEstimate
            , ("    resume cursor: " <>) . encodeBase16 <$> toList statusCursor
            ]

    unknownSection
        | null reportUnknown = []
        | otherwise = "" : "Unknown migrations:" : fmap (("  " <>) . describeUnknownMigration) reportUnknown

    hints = mconcat
        [ [ unknownMigrationsHint | not (null reportUnknown) ]
        , [ migrationRequiredHint | not (null reportBlocked) ]
        , [ "Data repairs are pending. Run 'kupo db migrate' to apply them." | hasPendingRepair ]
        , [ "The database is up to date." | isUpToDate ]
        ]
        & \case
            [] -> []
            footer -> "" : footer

    hasPendingRepair =
        any ((== Just Pending) . statusState) reportMigrations

    isUpToDate =
        null reportUnknown && all (isDone . statusState) reportMigrations

    isDone =
        (`elem` [Just Applied, Just Skipped])

    idWidth = foldl' max 0 $ T.length . statusId <$> reportMigrations
    kindWidth = foldl' max 0 $ T.length . migrationKindToText <$> [minBound .. maxBound]
    stateWidth = foldl' max 0 $ T.length . stateToText <$> Nothing : fmap Just [minBound .. maxBound]

-- | What Kupo would exit with at startup, or success when it would start.
statusExitCode
    :: StatusReport
    -- ^ The report on the database.
    -> ExitCode
statusExitCode StatusReport{reportBlocked, reportUnknown}
    -- Kupo refuses a database that a newer Kupo migrated before it checks the gate.
    | not (null reportUnknown) = ExitFailure 1
    | not (null reportBlocked) = ExitFailure exitCodeMigrationRequired
    | otherwise = ExitSuccess

stateToText :: Maybe MigrationState -> Text
stateToText =
    maybe "not yet applied" migrationStateToText
