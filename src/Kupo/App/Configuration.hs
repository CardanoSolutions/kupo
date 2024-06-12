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
    )
import Data.Aeson.Lens
    ( _String
    , key
    )
import Kupo.App.Database.Types
    ( Database (..)
    )
import Kupo.Control.MonadCatch
    ( catch
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
    , getPointSlotNo, pointFromTip
    )
import Kupo.Data.Configuration
    ( Configuration (..)
    , NetworkParameters (..)
    , Since (..)
    )
import Kupo.Data.Pattern
    ( Pattern (..)
    , patternToText
    )
import System.Directory
    ( doesFileExist
    )
import System.FilePath.Posix
    ( replaceFileName
    )

import qualified Data.Aeson as Json
import qualified Data.Set as Set
import qualified Data.Yaml as Yaml
import Kupo.Data.FetchTip (FetchTipClient)

parseNetworkParameters :: FilePath -> IO NetworkParameters
parseNetworkParameters configFile = runOrDie $ do
    config <- decodeYaml @Yaml.Value configFile
    let genesisFiles = (,)
            <$> config ^? key "ByronGenesisFile" . _String
            <*> config ^? key "ShelleyGenesisFile" . _String
    case genesisFiles of
        Nothing ->
            throwE "missing or invalid 'ByronGenesisFile' and/or 'ShelleyGenesisFile' from Cardano's configuration"
        Just (toString -> byronGenesisFile, toString -> shelleyGenesisFile) -> do
            byronGenesis   <- decodeYaml (replaceFileName configFile byronGenesisFile)
            shelleyGenesis <- decodeYaml (replaceFileName configFile shelleyGenesisFile)
            case Json.fromJSON (Json.Object (byronGenesis <> shelleyGenesis)) of
                Json.Error e -> throwE e
                Json.Success params -> pure params
  where
    runOrDie :: ExceptT String IO a -> IO a
    runOrDie = runExceptT >=> either (die . ("Failed to configure network parameters: " <>)) pure

    decodeYaml :: FromJSON a => FilePath -> ExceptT String IO a
    decodeYaml filepath = Yaml.decodeFileThrow filepath `catch` (\(e :: Yaml.ParseException) -> do
        lift (doesFileExist configFile) >>= \case
            True ->
                throwE $ "failed to parse configuration: malformed JSON/YAML: " <> show e
            False ->
                throwE $ "no configuration file found at the given location: " <> configFile
        )

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
    -> FetchTipClient m
    -> m (Maybe Point, [Point])
startOrResume tr configuration Database{..} fetchTip = do
    checkpoints <- runTransaction listCheckpointsDesc

    let slots = unSlotNo . getPointSlotNo <$> checkpoints
    logWith tr $ ConfigurationCheckpointsForIntersection { slots }

    case (since, checkpoints) of
        (Nothing, []) -> do
            logWith tr (ConfigurationInvalidOrMissingOption errNoStartingPoint)
            throwIO (NoStartingPointException errNoStartingPoint)

        (Just (SincePoint point), mostRecentCheckpoint:_) -> do
            if getPointSlotNo point > getPointSlotNo mostRecentCheckpoint then do
                logWith tr (ConfigurationInvalidOrMissingOption errConflictingSinceOptions)
                throwIO (ConflictingOptionsException errConflictingSinceOptions)
            else do
                pure
                    ( Just mostRecentCheckpoint
                    , sortOn (Down . getPointSlotNo) (point : checkpoints)
                    )

        (Nothing, pts@(mostRecentCheckpoint:_)) -> do
            pure (Just mostRecentCheckpoint, pts)

        (Just (SincePoint pt), []) ->
            pure (Nothing, [pt])

        (Just SinceTip, []) -> do
            tip <- pointFromTip <$> fetchTip
            pure (Just tip, [tip])

        (Just SinceTip, pts@(mostRecentCheckpoint:_)) -> do
            pure (Just mostRecentCheckpoint, pts)
  where
    Configuration{since} = configuration

    errNoStartingPoint =
        "No '--since' provided and no checkpoints found in the \
        \database. An explicit starting point (e.g. 'origin') is \
        \required the first time launching the application."

    errConflictingSinceOptions =
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
    alreadyKnownPatterns <- runTransaction listPatterns
    patterns <- if
        | null alreadyKnownPatterns && null configuredPatterns -> do
            logWith tr (ConfigurationInvalidOrMissingOption errNoPatterns)
            throwIO (ConflictingOptionsException errNoPatterns)
        | null configuredPatterns -> do
            pure alreadyKnownPatterns
        | null alreadyKnownPatterns -> do
            runTransaction (insertPatterns configuredPatterns)
            pure configuredPatterns
        | otherwise ->
            if alreadyKnownPatterns /= configuredPatterns then do
                logWith tr $ ConfigurationInvalidOrMissingOption errConflictingOptions
                throwIO (ConflictingOptionsException errConflictingOptions)
            else
                pure alreadyKnownPatterns
    logWith tr $ ConfigurationPatterns (Set.map patternToText patterns)
    newTVarIO patterns
  where
    Configuration{patterns = configuredPatterns} = configuration

    errNoPatterns =
        "Hey! It looks like the current configuration has no pattern defined. \
        \This would cause the indexer to run and index... nothing! You should \
        \try to define matching patterns using --match because there are much \
        \better ways to heat a room than to burn CPU cycles. If you're unsure \
        \about how to use patterns, have a look at the 'pattern' section on \
        \the user guide: <https://cardanosolutions.github.io/kupo/#section/Pattern>."

    errConflictingOptions =
        "Configuration patterns are different from previously known \
        \patterns. Restarting a running server using different \
        \command-line patterns is not allowed for it may likely be \
        \an error. If you do intent do dynamically manage patterns, \
        \please use the HTTP API instead of the command-line options."

data NoStartingPointException = NoStartingPointException Text deriving (Show)
instance Exception NoStartingPointException

data ConflictingOptionsException = ConflictingOptionsException Text deriving (Show)
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
    ConfigurationHydra
        :: { hydraHost :: String, hydraPort :: Int }
        -> TraceConfiguration
    ConfigurationCardanoNode
        :: { nodeSocket :: FilePath, nodeConfig :: FilePath }
        -> TraceConfiguration
    ConfigurationPatterns
        :: { patterns :: Set Text }
        -> TraceConfiguration
    ConfigurationCheckpointsForIntersection
        :: { slots :: [Word64] }
        -> TraceConfiguration
    ConfigurationInvalidOrMissingOption
        :: { hint :: Text }
        -> TraceConfiguration
    ConfigurationMaxConcurrency
        :: { maxConcurrentReaders :: Int, maxConcurrentWriters :: Int }
        -> TraceConfiguration
    deriving stock (Generic, Show)

instance ToJSON TraceConfiguration where
    toEncoding =
        defaultGenericToEncoding

instance HasSeverityAnnotation TraceConfiguration where
    getSeverityAnnotation = \case
        ConfigurationNetwork{} -> Info
        ConfigurationOgmios{} -> Info
        ConfigurationHydra{} -> Info
        ConfigurationCardanoNode{} -> Info
        ConfigurationPatterns{} -> Info
        ConfigurationCheckpointsForIntersection{} -> Info
        ConfigurationMaxConcurrency{} -> Info
        ConfigurationInvalidOrMissingOption{} -> Error
