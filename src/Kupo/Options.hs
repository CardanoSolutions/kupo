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

      -- * Types
    , parseNetworkParameters
    ) where

import Kupo.Prelude hiding
    ( group )
import Options.Applicative

import Control.Monad.Trans.Except
    ( throwE, withExceptT )
import Data.Aeson
    ( FromJSON )
import Data.Aeson.Lens
    ( key, _String )
import Kupo.Configuration
    ( Block
    , Configuration (..)
    , NetworkParameters (..)
    , Pattern (..)
    , Point (..)
    , StandardCrypto
    , patternFromText
    , pointFromText
    )
import Options.Applicative.Help.Pretty
    ( Doc, align, fillSep, hardline, indent, softbreak, text, vsep )
import System.FilePath.Posix
    ( replaceFileName )

import qualified Data.Aeson as Json
import qualified Data.Yaml as Yaml

data Command (f :: Type -> Type)
    = Run (f NetworkParameters) Configuration
    | Version

deriving instance Eq (f NetworkParameters) => Eq (Command f)
deriving instance Show (f NetworkParameters) => Show (Command f)

parseOptions :: IO (Command Identity)
parseOptions =
    customExecParser (prefs showHelpOnEmpty) parserInfo >>= \case
        Version -> pure Version
        Run _ cfg@Configuration{nodeConfig} -> do
            networkParameters <- parseNetworkParameters nodeConfig
            pure $ Run (Identity networkParameters) cfg

parseOptionsPure :: [String] -> Either String (Command Proxy)
parseOptionsPure args =
    case execParserPure defaultPrefs parserInfo args of
        Success a -> Right a
        Failure e -> Left (show e)
        CompletionInvoked{} -> Left "Completion Invoked."

parserInfo :: ParserInfo (Command Proxy)
parserInfo = info (helper <*> parser) $ mempty
    <> progDesc "Kupo - A daemon for building portable lookup indexes on Cardano."
    <> footerDoc (Just footer')
  where
    parser =
        versionOptionOrCommand
        <|>
        ( Run Proxy
            <$> ( Configuration
                    <$> nodeSocketOption
                    <*> nodeConfigOption
                    <*> workDirOption
                    <*> serverHostOption
                    <*> serverPortOption
                    <*> optional sinceOption
                    <*> many patternOption
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

-- | --workdir=DIR
workDirOption :: Parser FilePath
workDirOption = option str $ mempty
    <> long "workdir"
    <> metavar "DIRECTORY"
    <> help "Path to a working directory, where the database is stored."
    <> completer (bashCompleter "directory")

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

-- | [--since=POINT]
sinceOption :: Parser (Point (Block StandardCrypto))
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
    rdr :: String -> Maybe (Point (Block StandardCrypto))
    rdr = pointFromText . toText

-- | [--match=PATTERN]
patternOption :: Parser (Pattern StandardCrypto)
patternOption = option (maybeReader (patternFromText . toText)) $ mempty
    <> long "match"
    <> metavar "PATTERN"
    <> help "A pattern to match on. Can be provided multiple times (as a logical disjunction, i.e. 'or')"

-- | [--version|-v] | version
versionOptionOrCommand :: Parser (Command f)
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

--
-- Environment
--

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
-- Helper
--

longline :: Text -> Doc
longline = fillSep . fmap (text . toString) . words
