--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Kupo.App.Configuration
    ( -- * NetworkParameters
      parseNetworkParameters

    -- * Application Setup
    , startOrResume
    , newPatternsCache
    , ConflictingOptionsException (..)
    , NoStartingPointException (..)

    -- * Tracer
    , TraceConfiguration (..)
    ) where

import Kupo.Prelude

import Control.Monad.Trans.Except
    ( throwE
    , withExceptT
    )
import Data.Aeson.Lens
    ( _String
    , key
    )
import Kupo.App.Database
    ( Database (..)
    )
import Kupo.Control.MonadLog
    ( HasSeverityAnnotation (..)
    , MonadLog (..)
    , Severity (..)
    , Tracer
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Control.MonadThrow
    ( MonadThrow (..)
    )
import Kupo.Data.Cardano
    ( Point
    , SlotNo (..)
    , getPointSlotNo
    )
import Kupo.Data.Configuration
    ( Configuration (..)
    , NetworkParameters (..)
    )
import Kupo.Data.Database
    ( patternFromRow
    , patternToRow
    , pointFromRow
    )
import Kupo.Data.Pattern
    ( Pattern (..)
    , patternToText
    )
import System.FilePath.Posix
    ( replaceFileName
    )

import qualified Data.Aeson as Json
import qualified Data.Yaml as Yaml


parseNetworkParameters :: FilePath -> IO NetworkParameters
parseNetworkParameters configFile = runOrDie $ do
    config <- decodeYaml @Yaml.Value configFile
    let genesisFiles = (,)
            <$> config ^? key "ByronGenesisFile" . _String
            <*> config ^? key "ShelleyGenesisFile" . _String
    case genesisFiles of
        Nothing ->
            throwE "Missing 'ByronGenesisFile' and/or 'ShelleyGenesisFile' from \
                   \Cardano's configuration (i.e. '--node-config' option)?"
        Just (toString -> byronGenesisFile, toString -> shelleyGenesisFile) -> do
            byronGenesis   <- decodeYaml (replaceFileName configFile byronGenesisFile)
            shelleyGenesis <- decodeYaml (replaceFileName configFile shelleyGenesisFile)
            case Json.fromJSON (Json.Object (byronGenesis <> shelleyGenesis)) of
                Json.Error e -> throwE e
                Json.Success params -> pure params
  where
    runOrDie :: ExceptT String IO a -> IO a
    runOrDie = runExceptT >=> either (die . ("Failed to parse network parameters: " <>)) pure

    prettyParseException :: Yaml.ParseException -> String
    prettyParseException e = "Failed to decode JSON (or YAML) file: " <> show e

    decodeYaml :: FromJSON a => FilePath -> ExceptT String IO a
    decodeYaml = withExceptT prettyParseException . ExceptT . Yaml.decodeFileEither

--
-- Application Bootstrapping
--

startOrResume
    :: forall m.
        ( MonadThrow m
        , MonadLog m
        )
    => Tracer IO TraceConfiguration
    -> Configuration
    -> Database m
    -> m (Maybe SlotNo, [Point])
startOrResume tr configuration Database{..} = do
    checkpoints <- runReadOnlyTransaction (listCheckpointsDesc pointFromRow)

    let slots = unSlotNo . getPointSlotNo <$> checkpoints
    logWith tr $ ConfigurationCheckpointsForIntersection { slots }

    case (since, checkpoints) of
        (Nothing, []) -> do
            logWith tr errNoStartingPoint
            throwIO NoStartingPointException
        (Just point, mostRecentCheckpoint:_) -> do
            if getPointSlotNo point > getPointSlotNo mostRecentCheckpoint then do
                logWith tr errConflictingSinceOptions
                throwIO ConflictingOptionsException
            else do
                pure
                    ( Just (getPointSlotNo mostRecentCheckpoint)
                    , sortOn (Down . getPointSlotNo) (point : checkpoints)
                    )
        (Nothing, pts@(mostRecentCheckpoint:_)) -> do
            pure (Just (getPointSlotNo mostRecentCheckpoint), pts)
        (Just pt, []) ->
            pure (Nothing, [pt])
  where
    Configuration{since} = configuration

    errNoStartingPoint = ConfigurationInvalidOrMissingOption
        "No '--since' provided and no checkpoints found in the \
        \database. An explicit starting point (e.g. 'origin') is \
        \required the first time launching the application."

    errConflictingSinceOptions = ConfigurationInvalidOrMissingOption
        "The point provided through '--since' is more recent than \
        \any of the known checkpoints and it isn't possible to make \
        \a choice for resuming the application: should synchronization \
        \restart from the latest checkpoint or from the provided \
        \--since point? Please dispel the confusion by either choosing \
        \a different starting point (or none at all) or by using a \
        \fresh new database."

newPatternsCache
    :: forall m.
        ( MonadThrow m
        , MonadSTM m
        , MonadLog m
        )
    => Tracer IO TraceConfiguration
    -> Configuration
    -> Database m
    -> m (TVar m (Set Pattern))
newPatternsCache tr configuration Database{..} = do
    alreadyKnownPatterns <- runReadOnlyTransaction (listPatterns patternFromRow)
    patterns <- case (alreadyKnownPatterns, configuredPatterns) of
        (x:xs, []) ->
            pure (x:xs)
        ([], y:ys) -> do
            runReadWriteTransaction (insertPatterns (patternToRow <$> (y:ys)))
            pure (y:ys)
        ([], []) -> do
            logWith tr errNoPatterns
            throwIO ConflictingOptionsException
        (xs, ys) | sort xs /= sort ys -> do
            logWith tr errConflictingOptions
            throwIO ConflictingOptionsException
        (xs, _) ->
            pure xs
    logWith tr $ ConfigurationPatterns (patternToText <$> patterns)
    newTVarIO (fromList patterns)
  where
    Configuration{patterns = configuredPatterns} = configuration

    errNoPatterns = ConfigurationInvalidOrMissingOption
        "Hey! It looks like the current configuration has no pattern defined. \
        \This would cause the indexer to run and index... nothing! You should \
        \try to define matching patterns using --match because there are much \
        \better ways to heat a room than to burn CPU cycles. If you're unsure \
        \about how to use patterns, have a look at the 'pattern' section on \
        \the user guide: <https://cardanosolutions.github.io/kupo/#section/Pattern>."

    errConflictingOptions = ConfigurationInvalidOrMissingOption
        "Configuration patterns are different from previously known \
        \patterns. Restarting a running server using different \
        \command-line patterns is not allowed for it may likely be \
        \an error. If you do intent do dynamically manage patterns, \
        \please use the HTTP API instead of the command-line options."

data NoStartingPointException = NoStartingPointException deriving (Show)
instance Exception NoStartingPointException

data ConflictingOptionsException = ConflictingOptionsException  deriving (Show)
instance Exception ConflictingOptionsException

--
-- Tracer
--

data TraceConfiguration where
    ConfigurationNetwork
        :: { networkParameters :: NetworkParameters }
        -> TraceConfiguration
    ConfigurationOgmios
        :: { ogmiosHost :: String, ogmiosPort :: Int }
        -> TraceConfiguration
    ConfigurationCardanoNode
        :: { nodeSocket :: FilePath, nodeConfig :: FilePath }
        -> TraceConfiguration
    ConfigurationPatterns
        :: { patterns :: [Text] }
        -> TraceConfiguration
    ConfigurationCheckpointsForIntersection
        :: { slots :: [Word64] }
        -> TraceConfiguration
    ConfigurationInvalidOrMissingOption
        :: { hint :: Text }
        -> TraceConfiguration
    deriving stock (Generic, Show)

instance ToJSON TraceConfiguration where
    toEncoding =
        defaultGenericToEncoding

instance HasSeverityAnnotation TraceConfiguration where
    getSeverityAnnotation = \case
        ConfigurationNetwork{} -> Info
        ConfigurationOgmios{} -> Info
        ConfigurationCardanoNode{} -> Info
        ConfigurationPatterns{} -> Info
        ConfigurationCheckpointsForIntersection{} -> Info
        ConfigurationInvalidOrMissingOption{} -> Error
