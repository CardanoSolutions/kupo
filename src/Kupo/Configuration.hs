--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kupo.Configuration
    (
    -- * Configuration
      Configuration (..)
    , WorkDir (..)
    , ChainProducer (..)

    -- * NetworkParameters
    , NetworkParameters (..)
    , parseNetworkParameters

    -- ** Parameters Components
    , NetworkMagic (..)
    , EpochSlots (..)
    , SystemStart (..)
    , mkSystemStart

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
import Kupo.Control.MonadLog
    ( HasSeverityAnnotation (..), Severity (..) )
import Kupo.Control.MonadOuroboros
    ( EpochSlots (..), NetworkMagic (..) )
import Kupo.Data.Cardano
    ( Block, Point (..) )
import Kupo.Data.Pattern
    ( Pattern (..) )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( SystemStart (..) )
import System.FilePath.Posix
    ( replaceFileName )

import qualified Data.Aeson as Json
import qualified Data.Yaml as Yaml

data Configuration = Configuration
    { chainProducer :: !ChainProducer
    , workDir :: !WorkDir
    , serverHost :: !String
    , serverPort :: !Int
    , since :: !(Maybe (Point Block))
    , patterns :: ![Pattern]
    } deriving (Generic, Eq, Show)

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

data WorkDir
    = Dir FilePath
    | InMemory
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
        ConfigurationInvalidOrMissingOption{} -> Error
