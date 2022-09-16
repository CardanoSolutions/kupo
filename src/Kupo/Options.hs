--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
    ) where

import Kupo.Prelude hiding
    ( group
    )

import Options.Applicative

import Data.Char
    ( toUpper
    )
import Kupo.App
    ( TraceConsumer
    , TraceGardener
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
    , Tracer
    , TracerDefinition (..)
    , TracerHKD
    , defaultTracers
    )
import Kupo.Control.MonadTime
    ( DiffTime
    , millisecondsToDiffTime
    )
import Kupo.Data.Cardano
    ( Point
    , pointFromText
    )
import Kupo.Data.Configuration
    ( ChainProducer (..)
    , Configuration (..)
    , InputManagement (..)
    , WorkDir (..)
    )
import Kupo.Data.Pattern
    ( Pattern
    , patternFromText
    )
import Options.Applicative.Help.Pretty
    ( Doc
    , align
    , fillSep
    , hardline
    , indent
    , softbreak
    , string
    , text
    , vsep
    )
import Safe
    ( readMay
    )

import qualified Data.Text as T
import qualified Data.Text.Read as T

data Command
    = Run !Configuration !(Tracers IO MinSeverities)
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
    <> progDesc "Kupo - A daemon for building portable lookup indexes on Cardano."
    <> footerDoc (Just footer')
  where
    parser =
        versionOptionOrCommand
        <|>
        healthCheckCommand
        <|>
        ( Run
            <$> ( Configuration
                    <$> chainProducerOption
                    <*> workDirOption
                    <*> serverHostOption
                    <*> serverPortOption
                    <*> optional sinceOption
                    <*> many patternOption
                    <*> inputManagementOption
                    <*> pure 129600 -- TODO: should be pulled from genesis parameters
                    <*> garbageCollectionIntervalOption
                )
            <*> (tracersOption <|> Tracers
                    <$> fmap Const (logLevelOption "http-server")
                    <*> fmap Const (logLevelOption "database")
                    <*> fmap Const (logLevelOption "consumer")
                    <*> fmap Const (logLevelOption "garbage-collector")
                    <*> fmap Const (logLevelOption "configuration")
                )
        )

    footer' = vsep
        [ "Patterns: "
        , indent 2 "Patterns have the following syntax:"
        , mempty
        , indent 2 "PATTERN"
        , indent 4 "   ╭───╮                                  "
        , indent 4 "╾┬─┤ * ├───────────────────────────────┬╼ "
        , indent 4 " │ ╰───╯                               │  "
        , indent 4 " │ ┏━━━━━━━━━┓                         │  "
        , indent 4 " ├─┫ ADDRESS ┣─────────────────────────┤  "
        , indent 4 " │ ┗━━━━━━━━━┛                         │  "
        , indent 4 " │ ┏━━━━━━━━━━━━━━━┓                   │  "
        , indent 4 " ├─┫ STAKE-ADDRESS ┣───────────────────┤  "
        , indent 4 " │ ┗━━━━━━━━━━━━━━━┛                   │  "
        , indent 4 " │ ┏━━━━━━━━━━━━┓ ╭───╮ ┏━━━━━━━━━━━━┓ │  "
        , indent 4 " └─┫ CREDENTIAL ┣─┤ / ├─┫ CREDENTIAL ┣─┘  "
        , indent 4 "   ┗━━━━━━━━━━━━┛ ╰───╯ ┗━━━━━━━━━━━━┛    "
        , mempty
        , indent 2 "CREDENTIAL"
        , indent 4 "   ╭───╮                                                     "
        , indent 4 "╾┬─┤ * ├──────────────────────────────────────────────────┬╼ "
        , indent 4 " │ ╰───╯                                                  │  "
        , indent 4 " │ ┏━━━━━━━━━━━━━━━━━━━━━━━━┓                             │  "
        , indent 4 " ├─┫ BASE16(bytes .size 32) ┣─────────────────────────────┤  "
        , indent 4 " │ ┗━━━━━━━━━━━━━━━━━━━━━━━━┛                             │  "
        , indent 4 " │ ┏━━━━━━━━━━━━━━━━━━━━━━━━┓                             │  "
        , indent 4 " ├─┫ BASE16(bytes .size 28) ┣─────────────────────────────┤  "
        , indent 4 " │ ┗━━━━━━━━━━━━━━━━━━━━━━━━┛                             │  "
        , indent 4 " │ ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓           │  "
        , indent 4 " ├─┫ BECH32(bytes) .hrp (vk|addr_vk|stake_vk) ┣───────────┤  "
        , indent 4 " │ ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛           │  "
        , indent 4 " │ ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓ │  "
        , indent 4 " └─┫ BECH32(bytes) .hrp (vkh|addr_vkh|stake_vkh|script) ┣─┘  "
        , indent 4 "   ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛    "
        , mempty
        , indent 2 "Examples:"
        , mempty
        , indent 4 "🗸 --match *"
        , indent 4 "🗸 --match */*"
        , indent 4 "🗸 --match addr1vyc29pvl2uyzqt8nwxrcxnf558ffm27u3d9calxn8tdudjgz4xq9p"
        , indent 4 "🗸 --match addr_vk1x7da0l25j04my8sej5ntrgdn38wmshxhplxdfjskn07ufavsgtkqn5hljl/*"
        , indent 4 "🗸 --match */script1cda3khwqv60360rp5m7akt50m6ttapacs8rqhn5w342z7r35m37"
        , indent 4 "🗸 --match dca1e44765b9f80c8b18105e17de90d4a07e4d5a83de533e53fee32e0502d17e/*"
        , indent 4 "🗸 --match */4fc6bb0c93780ad706425d9f7dc1d3c5e3ddbf29ba8486dce904a5fc"
        ]

--
-- Command-line options
--

chainProducerOption :: Parser ChainProducer
chainProducerOption =
    cardanoNodeOptions <|> ogmiosOptions
  where
    cardanoNodeOptions = CardanoNode
        <$> nodeSocketOption
        <*> nodeConfigOption

    ogmiosOptions = Ogmios
        <$> ogmiosHostOption
        <*> ogmiosPortOption

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

-- | --workdir=DIR | --in-memory
workDirOption :: Parser WorkDir
workDirOption =
    dirOption <|> inMemoryFlag
  where
    dirOption = fmap Dir $ option str $ mempty
        <> long "workdir"
        <> metavar "DIRECTORY"
        <> help "Path to a working directory, where the database is stored."
        <> completer (bashCompleter "directory")

    inMemoryFlag = flag' InMemory $ mempty
        <> long "in-memory"
        <> help "Run fully in-memory, data is short-lived and lost when the process exits."

-- | [--host=IPv4], default: 127.0.0.1
serverHostOption :: Parser String
serverHostOption = option str $ mempty
    <> long "host"
    <> metavar "IPv4"
    <> help "Address to bind to."
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

-- | [--since=POINT]
sinceOption :: Parser Point
sinceOption = option (maybeReader rdr) $ mempty
    <> long "since"
    <> metavar "POINT"
    <> helpDoc (Just $ mconcat
        [ "A point on chain from where to start syncing. "
        , softbreak
        , "Expects either:"
        , hardline
        , vsep
            [ align $ indent 2 "- \"origin\""
            , align $ indent 2 $ longline "- A dot-separated integer (slot number) and base16-encoded digest (block header hash)."
            ]
        ])
  where
    rdr :: String -> Maybe Point
    rdr = pointFromText . toText

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
    <> helpDoc (Just doc)
  where
    doc =
        string "Remove inputs from the index when spent, instead of marking them as 'spent'."

-- | [--gc-interval=SECONDS]
garbageCollectionIntervalOption :: Parser DiffTime
garbageCollectionIntervalOption = option diffTime $ mempty
    <> long "gc-interval"
    <> metavar "SECONDS"
    <> help "Number of seconds between background database garbage collections pruning obsolete or unnecessary data."
    <> value 600
    <> showDefault

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
        string $ "Minimal severity of " <> toString component <> " log messages."

-- | [--log-level=SEVERITY]
tracersOption :: Parser (Tracers m MinSeverities)
tracersOption = fmap defaultTracers $ option severity $ mempty
    <> long "log-level"
    <> metavar "SEVERITY"
    <> helpDoc (Just doc)
    <> completer (listCompleter severities)
  where
    doc =
        vsep $ string <$> mconcat
            [ [ "Minimal severity of all log messages." ]
            , ("- " <>) <$> severities
            , [ "Or alternatively, to turn a logger off:" ]
            , [ "- Off" ]
            ]

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
            [ string $ toString $ unwords
                [ "Handy command to check whether a Kupo daemon is up-and-running,"
                , "and correctly connected to a network / cardano-node."
                ]
            , mempty
            , string $ toString $ unwords
                [ "This can, for example, be wired to Docker's HEALTHCHECK"
                , "feature easily."
                ]
            ])
  where
    parser = HealthCheck <$> serverHostOption <*> serverPortOption
    helpText = "Performs a health check against a running daemon."

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
    } deriving (Generic)

deriving instance Show (Tracers m MinSeverities)
deriving instance Eq (Tracers m MinSeverities)

--
-- Helper
--

longline :: Text -> Doc
longline = fillSep . fmap (text . toString) . words

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
    pure (millisecondsToDiffTime (n * 1_000))
