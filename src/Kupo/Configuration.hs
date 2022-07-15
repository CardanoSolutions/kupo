--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kupo.Configuration
    (
    -- * Configuration
      Configuration (..)
    , WorkDir (..)
    , InputManagement (..)
    , ChainProducer (..)
    , LongestRollback (..)

    -- * NetworkParameters
    , NetworkParameters (..)
    , parseNetworkParameters

    -- ** Parameters Components
    , NetworkMagic (..)
    , EpochSlots (..)
    , SystemStart (..)
    , mkSystemStart

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
    ( throwE, withExceptT )
import Data.Aeson
    ( (.:) )
import Data.Aeson.Lens
    ( key, _String )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Data.Time.Format.ISO8601
    ( iso8601ParseM )
import Kupo.Control.MonadDatabase
    ( Database (..), LongestRollback (..) )
import Kupo.Control.MonadLog
    ( HasSeverityAnnotation (..), MonadLog (..), Severity (..), Tracer )
import Kupo.Control.MonadOuroboros
    ( EpochSlots (..), NetworkMagic (..) )
import Kupo.Control.MonadSTM
    ( MonadSTM (..) )
import Kupo.Control.MonadThrow
    ( MonadThrow (..) )
import Kupo.Data.Cardano
    ( Block, Point (..), SlotNo (..), getPointSlotNo )
import Kupo.Data.Database
    ( patternFromRow, patternToRow, pointFromRow )
import Kupo.Data.Pattern
    ( Pattern (..), patternToText )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( SystemStart (..) )
import System.FilePath.Posix
    ( replaceFileName )

import qualified Data.Aeson as Json
import qualified Data.Yaml as Yaml
import Kupo.Control.MonadTime
    ( DiffTime )

-- | Application-level configuration.
data Configuration = Configuration
    { chainProducer :: !ChainProducer
        -- ^ Where the data comes from: cardano-node vs ogmios
    , workDir :: !WorkDir
        -- ^ Where to store the data: in-memory vs specific location on-disk
    , serverHost :: !String
        -- ^ Hostname for the API HTTP server
    , serverPort :: !Int
        -- ^ Port for the API HTTP Server
    , since :: !(Maybe (Point Block))
        -- ^ Point from when a *new* synchronization should start
    , patterns :: ![Pattern]
        -- ^ List of address patterns to look for when synchronizing
    , inputManagement :: !InputManagement
        -- ^ Behavior to adopt towards spent inputs (prune or mark-and-leave)
    , longestRollback :: !LongestRollback
        -- ^ Number of slots before which data can be considered immutable
    , pruneThrottleDelay :: !DiffTime
        -- ^ Delay between each garbage-collection of database data
    } deriving (Generic, Eq, Show)

-- | Where does kupo pulls its data from. Both 'cardano-node' and 'ogmios' are
-- equivalent in the capabilities and information they offer; a cardano-node
-- will have to be through a local connection (domain socket) whereas ogmios can
-- happen _over the wire_ on a remote server but is slower overall. So both have
-- trade-offs.
data ChainProducer
    = CardanoNode
        { nodeSocket :: !FilePath
        , nodeConfig :: !FilePath
        }
    | Ogmios
        { ogmiosHost :: !String
        , ogmiosPort :: !Int
        }
    deriving (Generic, Eq, Show)

-- | Database working directory. 'in-memory' runs the database in hot memory,
-- only suitable for non-permissive patterns or testing.
data WorkDir
    = Dir FilePath
    | InMemory
    deriving (Generic, Eq, Show)

-- | What to do with inputs that are spent. There are two options:
--
-- - 'Mark': keeps all spent inputs in the index, but marks them as spent or
-- unspent. Clients may then query filtered results based on their status.
--
-- - 'Remove': which deletes any spent inputs from the index, keeping it
-- concise.
--
-- There are use-cases for both behavior, hence why is is a user-configured
-- behavior. The default should be the less destructive one, which is 'Mark'.
data InputManagement
    = MarkSpentInputs
    | RemoveSpentInputs
    deriving (Generic, Eq, Show)

data NetworkParameters = NetworkParameters
    { networkMagic :: !NetworkMagic
    , systemStart :: !SystemStart
    , slotsPerEpoch :: !EpochSlots
    } deriving stock (Generic, Eq, Show)
      deriving anyclass (ToJSON)

deriving newtype instance ToJSON EpochSlots
deriving newtype instance ToJSON SystemStart
deriving newtype instance ToJSON NetworkMagic

instance FromJSON NetworkParameters where
    parseJSON = Json.withObject "NetworkParameters" $ \obj -> do
        nm <- obj .: "networkMagic"
        ss <- obj .: "systemStart" >>= parseISO8601
        k  <- obj .: "protocolConsts" >>= Json.withObject "protocolConst" (.: "k")
        pure NetworkParameters
            { networkMagic =
                NetworkMagic (fromIntegral @Integer nm)
            , systemStart =
                SystemStart ss
            , slotsPerEpoch  =
                EpochSlots (fromIntegral @Integer $ 10 * k)
            }
      where
        parseISO8601 (toString @Text -> str) =
            case iso8601ParseM str of
                Nothing -> fail "couldn't parse ISO-8601 date-time."
                Just t  -> pure t

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

-- | Construct a 'SystemStart' from a number of seconds.
mkSystemStart :: Int -> SystemStart
mkSystemStart =
    SystemStart . posixSecondsToUTCTime . toPicoResolution . toEnum
  where
    toPicoResolution = (*1000000000000)

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
    -> m [Point Block]
startOrResume tr configuration Database{..} = do
    -- TODO: include sanity check and fail if '--prune-utxo' is included but
    -- there exists marked inputs in the database. This would indicate that the
    -- index was restarted with a different configuration.
    checkpoints <- runTransaction (listCheckpointsDesc pointFromRow)

    case nonEmpty (sortOn Down (unSlotNo . getPointSlotNo <$> checkpoints)) of
        Nothing -> pure ()
        Just slots -> do
            let mostRecentCheckpoint = head slots
            let oldestCheckpoint = last slots
            let totalCheckpoints = length slots
            logWith tr $ ConfigurationFoundCheckpoints
                { totalCheckpoints
                , mostRecentCheckpoint
                , oldestCheckpoint
                }

    nSpent <- runTransaction countSpentInputs
    case inputManagement of
        RemoveSpentInputs | nSpent > 0 -> do
            logWith tr errConflictingUtxoManagementOption
            throwIO ConflictingOptionsException
        _ ->
            pure ()

    case (since, checkpoints) of
        (Nothing, []) -> do
            logWith tr errNoStartingPoint
            throwIO NoStartingPointException
        (Just point, mostRecentCheckpoint:_) -> do
            if getPointSlotNo point > getPointSlotNo mostRecentCheckpoint then do
                logWith tr errConflictingSinceOptions
                throwIO ConflictingOptionsException
            else do
                pure (sortOn (Down . getPointSlotNo) (point : checkpoints))
        (Nothing, pts) -> do
            pure pts
        (Just pt, []) ->
            pure [pt]

  where
    Configuration{since, inputManagement} = configuration

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

    errConflictingUtxoManagementOption = ConfigurationInvalidOrMissingOption
        "It appears that the application was restarted with a conflicting \
        \behavior w.r.t. UTxO management. Indeed, `--prune-utxo` indicates \
        \that inputs should be pruned from the database when spent. However \
        \inputs marked as 'spent' were found in the database, which suggests \
        \that it was first constructed without the flag `--prune-utxo`. \
        \Continuing would lead to a inconsistent state and is therefore \
        \prevented. \n\nShould you still want to proceed, make sure to first \
        \prune all spent inputs from the database using the following \
        \query: \n\n\t DELETE FROM inputs WHERE spent_at IS NOT NULL;"

newPatternsCache
    :: forall m.
        ( MonadThrow m
        , MonadSTM m
        , MonadLog m
        )
    => Tracer IO TraceConfiguration
    -> Configuration
    -> Database m
    -> m (TVar m [Pattern])
newPatternsCache tr configuration Database{..} = do
    alreadyKnownPatterns <- runTransaction (listPatterns patternFromRow)
    patterns <- case (alreadyKnownPatterns, configuredPatterns) of
        (x:xs, []) ->
            pure (x:xs)
        ([], y:ys) -> do
            runTransaction (insertPatterns (patternToRow <$> (y:ys)))
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
    newTVarIO patterns
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
    ConfigurationFoundCheckpoints
        :: { totalCheckpoints :: Int
           , mostRecentCheckpoint :: Word64
           , oldestCheckpoint :: Word64
           }
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
        ConfigurationFoundCheckpoints{} -> Info
        ConfigurationInvalidOrMissingOption{} -> Error
