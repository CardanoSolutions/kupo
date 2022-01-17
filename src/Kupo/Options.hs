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
import Data.Aeson.Lens
    ( key, _Integer, _String )
import Data.Time.Format.ISO8601
    ( iso8601ParseM )
import Kupo.Configuration
    ( Configuration (..)
    , EpochSlots (..)
    , NetworkMagic (..)
    , NetworkParameters (..)
    , SystemStart (..)
    )
import System.FilePath.Posix
    ( replaceFileName )

import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Pretty as Yaml

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
    <> value 1337
    <> showDefault

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

-- TODO: This code is mostly duplicated with Ogmios and could be factored out in
-- a small module.
parseNetworkParameters :: FilePath -> IO NetworkParameters
parseNetworkParameters configFile = runOrDie $ do
    config <- decodeYaml configFile
    let genesisFiles = (,)
            <$> config ^? key "ByronGenesisFile" . _String
            <*> config ^? key "ShelleyGenesisFile" . _String
    case genesisFiles of
        Nothing ->
            throwE "Missing 'ByronGenesisFile' and/or 'ShelleyGenesisFile' from Cardano's configuration?"
        Just (toString -> byronGenesisFile, toString -> shelleyGenesisFile) -> do
            byronGenesis   <- decodeYaml (replaceFileName configFile byronGenesisFile)
            shelleyGenesis <- decodeYaml (replaceFileName configFile shelleyGenesisFile)
            let params = (,,)
                    <$> (shelleyGenesis ^? key "networkMagic" . _Integer)
                    <*> (iso8601ParseM . toString =<< shelleyGenesis ^? key "systemStart" . _String)
                    <*> (byronGenesis ^? key "protocolConsts" . key "k" . _Integer)
            case params of
                Nothing -> do
                    let prettyYaml = decodeUtf8 (Yaml.encodePretty Yaml.defConfig shelleyGenesis)
                    throwE $ toString $ unwords
                        [ "Couldn't find (or failed to parse) required network"
                        , "parameters (networkMagic, systemStart and/or epochLength)"
                        , "in genesis file: \n" <> prettyYaml
                        ]
                Just (nm, ss, k) ->
                    return NetworkParameters
                        { networkMagic =
                            NetworkMagic (fromIntegral nm)
                        , systemStart =
                            SystemStart ss
                        , slotsPerEpoch  =
                            EpochSlots (fromIntegral $ 10 * k)
                        }
  where
    runOrDie :: ExceptT String IO a -> IO a
    runOrDie = runExceptT >=> either die pure

    prettyParseException :: Yaml.ParseException -> String
    prettyParseException e = "Failed to decode JSON (or YAML) file: " <> show e

    decodeYaml :: FilePath -> ExceptT String IO Yaml.Value
    decodeYaml = withExceptT prettyParseException . ExceptT . Yaml.decodeFileEither
