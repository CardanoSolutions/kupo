--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Kupo.Options
    ( -- * Command
      Command (..)
    , parseOptions
    , parseOptionsPure

      -- * Options
    , nodeSocketOption
    , nodeConfigOption
    , serverHostOption
    , serverPortOption
    , versionOptionOrCommand
    , healthCheckCommand

      -- * Tracers
    , Tracers (..)
    , TracersCopy (..)
    ) where

import Kupo.Prelude hiding
    ( group
    )

import Options.Applicative

import Control.Monad.Trans.Except
    ( Except
    , except
    )
import Data.Char
    ( toUpper
    )
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Kupo.App
    ( TraceConsumer
    , TraceGardener
    , TraceKupo
    )
import Kupo.App.Configuration
    ( TraceConfiguration
    )
import Kupo.App.Database
    ( TraceDatabase
    )
import Kupo.App.Http
    ( TraceHttpServer
    )
import Kupo.Control.MonadLog
    ( Severity (..)
    , TraceProgress
    , Tracer
    , TracerDefinition (..)
    , TracerHKD
    , defaultTracers
    )
import Kupo.Control.MonadTime
    ( DiffTime
    , secondsToDiffTime
    )
import Kupo.Data.Cardano
    ( pointFromText
    )
import Kupo.Data.Configuration
    ( ChainProducer (..)
    , Configuration (..)
    , DatabaseLocation (..)
    , DeferIndexesInstallation (..)
    , InputManagement (..)
    , Since (..)
    )
import Kupo.Data.Pattern
    ( Pattern
    , patternFromText
    )
import Options.Applicative.Help.Pretty
    ( Doc
    , align
    , annotate
    , bold
    , fillSep
    , hardline
    , hsep
    , indent
    , softline
    , vsep
    )
import Options.Applicative.Types
    ( ReadM (..)
    )
import Safe
    ( readMay
    )
import qualified Text.URI as URI
import Text.URI
    ( URI
    )

data Command
    = Run !Configuration !(Tracers IO MinSeverities)
    | Copy !FilePath !FilePath !(Set Pattern)
    | HealthCheck !String !Int
    | Version
    deriving (Eq, Show)

parseOptions :: IO Command
parseOptions =
    customExecParser (prefs showHelpOnEmpty) parserInfo

parseOptionsPure :: [String] -> Either String Command
parseOptionsPure args =
    case execParserPure defaultPrefs parserInfo args of
        Success a -> Right a
        Failure e -> Left (show e)
        CompletionInvoked{} -> Left "Completion Invoked."

parserInfo :: ParserInfo Command
parserInfo = info (helper <*> parser) $ mempty
    <> progDesc "Kupo - Fast, lightweight & configurable chain-index for Cardano."
    <> footerDoc (Just footer')
  where
    parser =
        versionOptionOrCommand
        <|>
        healthCheckCommand
        <|>
        copyCommand
        <|>
        ( Run
            <$> ( Configuration
                    <$> chainProducerOption
                    <*> databaseLocationOption
                    <*> serverHostOption
                    <*> serverPortOption
                    <*> optional sinceOption
                    <*> fmap fromList (many patternOption)
                    <*> inputManagementOption
                    <*> pure 129600 -- TODO: should be pulled from genesis parameters
                    <*> garbageCollectionIntervalOption
                    <*> deferIndexesOption
                )
            <*> (tracersOption <|> Tracers
                    <$> fmap Const (logLevelOption "http-server")
                    <*> fmap Const (logLevelOption "database")
                    <*> fmap Const (logLevelOption "consumer")
                    <*> fmap Const (logLevelOption "garbage-collector")
                    <*> fmap Const (logLevelOption "configuration")
                    <*> pure (Const (Just Info))
                )
        )

    footer' = hsep
        [ "See more details on <https://cardanosolutions.github.io/kupo/> or in the manual for"
        , annotate bold "kupo(1)."
        ]

--
-- Command-line options
--

chainProducerOption :: Parser ChainProducer
chainProducerOption =
    cardanoNodeOptions <|> ogmiosOptions <|> hydraOptions <|> readOnlyReplicaFlag
  where
    cardanoNodeOptions = CardanoNode
        <$> nodeSocketOption
        <*> nodeConfigOption

    ogmiosOptions = Ogmios
        <$> ogmiosHostOption
        <*> ogmiosPortOption

    hydraOptions = Hydra
        <$> hydraHostOption
        <*> hydraPortOption

-- | --node-socket=FILEPATH
nodeSocketOption :: Parser FilePath
nodeSocketOption = option str $ mempty
    <> long "node-socket"
    <> metavar "FILEPATH"
    <> help "Path to the node socket."
    <> completer (bashCompleter "file")

-- | --node-config=FILEPATH
nodeConfigOption :: Parser FilePath
nodeConfigOption = option str $ mempty
    <> long "node-config"
    <> metavar "FILEPATH"
    <> help "Path to the node configuration file."
    <> completer (bashCompleter "file")

-- | --workdir=DIR | --in-memory | --postgres-url=URL
databaseLocationOption :: Parser DatabaseLocation
databaseLocationOption =
    dirOption <|> inMemoryFlag <|> remoteOption
  where
    dirOption = fmap Dir $ option str $ mempty
        <> long "workdir"
        <> metavar "DIRECTORY"
        <> help "Path to a working directory, where the database is stored."
        <> completer (bashCompleter "directory")
#if postgres
        <> internal
#endif

    inMemoryFlag = flag' (InMemory Nothing) $ mempty
        <> long "in-memory"
        <> help "Run fully in-memory, data is short-lived and lost when the process exits."
#if postgres
        <> internal
#endif

    remoteOption = fmap Remote $ option uriBuilder $ mempty
      <> long "postgres-url"
      <> metavar "URL"
      <> help
          ( "fully qualified PostgreSQL URL in the form"
            <> " postgresql://[user[:password]@][host][:port][/dbname][?param1=value1&...]"
          )
#if !postgres
      <> internal
#endif

      where
          uriBuilder :: ReadM URI
          uriBuilder = ReadM $ do
             lift . uriEither . fromString =<< ask

          uriEither :: Text -> Except ParseError URI
          uriEither input = except $ (ErrorMsg . displayException) `first` URI.mkURI input

-- | [--host=IPv4], default: 127.0.0.1
serverHostOption :: Parser String
serverHostOption = option str $ mempty
    <> long "host"
    <> metavar "IPv4"
    <> help "Address to bind to. Prefix with 'wss://' to connect to hosts behind TLS."
    <> value "127.0.0.1"
    <> showDefault
    <> completer (bashCompleter "hostname")

-- | [--port=TCP/PORT], default: 1337
serverPortOption :: Parser Int
serverPortOption = option auto $ mempty
    <> long "port"
    <> metavar "TCP/PORT"
    <> help "Port to listen on."
    <> value 1442
    <> showDefault

-- | [--ogmios-host=IPv4]
ogmiosHostOption :: Parser String
ogmiosHostOption = option str $ mempty
    <> long "ogmios-host"
    <> metavar "IPv4"
    <> help "Ogmios' host address."
    <> completer (bashCompleter "hostname")

-- | [--ogmios-port=TCP/PORT]
ogmiosPortOption :: Parser Int
ogmiosPortOption = option auto $ mempty
    <> long "ogmios-port"
    <> metavar "TCP/PORT"
    <> help "Ogmios' port."

-- | [--hydra-host=IPv4]
hydraHostOption :: Parser String
hydraHostOption = option str $ mempty
    <> long "hydra-host"
    <> metavar "IPv4"
    <> help "Hydra-node host address to connect to."
    <> completer (bashCompleter "hostname")

-- | [--hydra-port=TCP/PORT]
hydraPortOption :: Parser Int
hydraPortOption = option auto $ mempty
    <> long "hydra-port"
    <> metavar "TCP/PORT"
    <> help "Hydra-node port to connect to."

-- | [--since=POINT]
sinceOption :: Parser Since
sinceOption = option (maybeReader rdr) $ mempty
    <> long "since"
    <> metavar "POINT"
    <> helpDoc (Just $ mconcat
        [ "A point on chain from where to start syncing. Mandatory on first start. Optional after. "
        , softline
        , "Expects either:"
        , hardline
        , vsep
            [ align $ indent 2 "- \"origin\" synchronise from the beginning of the network."
            , align $ indent 2 "- \"tip\" synchronise from wherever the node is at."
            , align $ indent 2 $ longline "- A dot-separated integer (slot number) and base16-encoded digest (block header hash)."
            ]
        ])
  where
    rdr :: String -> Maybe Since
    rdr "tip" = pure SinceTip
    rdr s = fmap SincePoint (pointFromText $ toText s)

-- | [--match=PATTERN]
patternOption :: Parser Pattern
patternOption = option (maybeReader (patternFromText . toText)) $ mempty
    <> long "match"
    <> metavar "PATTERN"
    <> help "A pattern to match on. Can be provided multiple times (as a logical disjunction, i.e. 'or')"

-- | [--prune-utxo]
inputManagementOption :: Parser InputManagement
inputManagementOption = flag MarkSpentInputs RemoveSpentInputs $ mempty
    <> long "prune-utxo"
    <> help "When enabled, eventually remove inputs from the index when spent, instead of marking them as 'spent'."

-- | [--gc-interval=SECONDS]
garbageCollectionIntervalOption :: Parser DiffTime
garbageCollectionIntervalOption = option diffTime $ mempty
    <> long "gc-interval"
    <> metavar "SECONDS"
    <> help "Number of seconds between background database garbage collections pruning obsolete or unnecessary data."
    <> value 3600
    <> showDefault

-- | [--defer-db-indexes]
deferIndexesOption :: Parser DeferIndexesInstallation
deferIndexesOption = flag InstallIndexesIfNotExist SkipNonEssentialIndexes $ mempty
    <> long "defer-db-indexes"
    <> help
        ( "When enabled, defer the creation of database indexes to the next start. "
          <> "This is useful to make the first-ever synchronization faster but will make certain "
          <> "queries considerably slower."
        )

-- | [--read-only]
readOnlyReplicaFlag :: Parser ChainProducer
readOnlyReplicaFlag = flag' ReadOnlyReplica $ mempty
    <> long "read-only"
    <> help
        ( "Start this Kupo instance as a read-only replica. It will only read from the database. "
          <> "Requires a master write server to keep it synchronized. Note: replicas cannot access "
          <> "transactions metadata."
        )

-- | [--log-level-{COMPONENT}=SEVERITY], default: Info
logLevelOption :: Text -> Parser (Maybe Severity)
logLevelOption component =
    option severity $ mempty
        <> long ("log-level-" <> toString component)
        <> metavar "SEVERITY"
        <> helpDoc (Just doc)
        <> value (Just Info)
        <> showDefaultWith (maybe "ø" show)
        <> completer (listCompleter severities)
  where
    doc =
        fromString $ "Minimal severity of " <> toString component <> " log messages."

-- | [--log-level=SEVERITY]
tracersOption :: Parser (Tracers m MinSeverities)
tracersOption = fmap defaultTracers $ option severity $ mempty
    <> long "log-level"
    <> metavar "SEVERITY"
    <> helpDoc (Just doc)
    <> completer (listCompleter severities)
  where
    doc =
        vsep $ fromString <$> mconcat
            [ [ "Minimal severity of all log messages." ]
            , ("- " <>) <$> severities
            , [ "Or alternatively, to turn a logger off:" ]
            , [ "- Off" ]
            ]

-- | --{from,into}=FILEPATH
copyFilePathOption :: String -> Parser FilePath
copyFilePathOption optName =
    option str $ mempty
        <> long optName
        <> metavar "DIR"
        <> help ("Working directory to copy " <> optName <> ".")
        <> completer (bashCompleter "dir")

-- | [--version|-v] | version
versionOptionOrCommand :: Parser Command
versionOptionOrCommand =
    flag' Version (mconcat
        [ long "version"
        , short 'v'
        , help helpText
        ])
  <|>
    subparser (mconcat
        [ hidden
        , command "version" $ info (pure Version) (progDesc helpText)
        ])
  where
    helpText = "Show the software current version."

-- | health-check
healthCheckCommand :: Parser Command
healthCheckCommand =
    subparser $ command "health-check" $ info (helper <*> parser) $ mempty
        <> progDesc helpText
        <> headerDoc (Just $ vsep
            [ fromString $ toString $ unwords
                [ "Handy command to check whether a Kupo daemon is up-and-running,"
                , "and correctly connected to a network / cardano-node."
                ]
            , mempty
            , fromString $ toString $ unwords
                [ "This can, for example, be wired to Docker's HEALTHCHECK"
                , "feature easily."
                ]
            ])
  where
    parser = HealthCheck <$> serverHostOption <*> serverPortOption
    helpText = "Performs a health check against a running daemon."

-- | copy --from [FILEPATH] --into [FILEPATH] [--match PATTERN...]
copyCommand :: Parser Command
copyCommand =
    subparser $ command "copy" $ info (helper <*> parser) $ mempty
        <> progDesc "Copy from a source database into another, while applying the provided pattern filters."
        <> header
            ( "This is useful to rapidly bootstrap new database from existing ones. Note that "
              <> "it implies that the source database pattern configuration is a superset of the "
              <> "target; results may otherwise be missing."
            )
  where
    parser = Copy
        <$> copyFilePathOption "from"
        <*> copyFilePathOption "into"
        <*> fmap fromList (many patternOption)

--
-- Tracers
--

data Tracers m (kind :: TracerDefinition) = Tracers
    { tracerHttp
        :: !(TracerHKD kind (Tracer m TraceHttpServer))
    , tracerDatabase
        :: !(TracerHKD kind (Tracer m TraceDatabase))
    , tracerConsumer
        :: !(TracerHKD kind (Tracer m TraceConsumer))
    , tracerGardener
        :: !(TracerHKD kind (Tracer m TraceGardener))
    , tracerConfiguration
        :: !(TracerHKD kind (Tracer m TraceConfiguration))
    , tracerKupo
        :: !(TracerHKD kind (Tracer m TraceKupo))
    } deriving (Generic)

deriving instance Show (Tracers m MinSeverities)
deriving instance Eq (Tracers m MinSeverities)

data TracersCopy m (kind :: TracerDefinition) = TracersCopy
    { tracerCopy
        :: !(TracerHKD kind (Tracer m TraceDatabase))
    , tracerProgress
        :: !(TracerHKD kind (Tracer m TraceProgress))
    } deriving (Generic)

deriving instance Show (TracersCopy m MinSeverities)
deriving instance Eq (TracersCopy m MinSeverities)

--
-- Helper
--

longline :: Text -> Doc
longline = fillSep . fmap (fromString . toString) . words

severities :: [String]
severities =
    show @_ @Severity <$> [minBound .. maxBound]

severity :: ReadM (Maybe Severity)
severity = maybeReader $ \case
    [] -> Nothing
    (toUpper -> h):q ->
        if h:q == "Off" then
            Just Nothing
        else
            Just <$> readMay (h:q)

diffTime :: ReadM DiffTime
diffTime = eitherReader $ \s -> do
    (n, remainder) <- T.decimal (toText s)
    unless (T.null remainder) $ Left "Invalid number of seconds, must be a positive integer with no decimals."
    pure (secondsToDiffTime n)
