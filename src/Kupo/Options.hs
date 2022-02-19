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

import Kupo.Prelude
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
    , pointFromText
    )
import Options.Applicative.Help.Pretty
    ( indent, string, vsep )
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
  where
    parser =
        versionOptionOrCommand
        <|>
        ( Run Proxy
            <$> ( Configuration
                    <$> nodeSocketOption
                    <*> nodeConfigOption
                    <*> serverHostOption
                    <*> serverPortOption
                    <*> optional sinceOption
                    <*> many patternOption
                )
        )

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
    <> helpDoc (Just $ vsep
        [ string "A point on chain from where to start syncing. Expects either:"
        , indent 2 "- \"origin\""
        , indent 2 "- A dot-separated integer (slot number) and base16-encoded \
                   \digest (block header hash)."
        ])
  where
    rdr :: String -> Maybe (Point (Block StandardCrypto))
    rdr = pointFromText . toText

-- | [--match=PATTERN]
patternOption :: Parser (Pattern StandardCrypto)
patternOption = undefined

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
    runOrDie = runExceptT >=> either die pure

    prettyParseException :: Yaml.ParseException -> String
    prettyParseException e = "Failed to decode JSON (or YAML) file: " <> show e

    decodeYaml :: FromJSON a => FilePath -> ExceptT String IO a
    decodeYaml = withExceptT prettyParseException . ExceptT . Yaml.decodeFileEither
