--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kupo.Configuration
    (
    -- * Configuration
      Configuration (..)
    , WorkDir (..)
    , StandardCrypto
    , Point (..)
    , Block
    , Pattern (..)

    -- ** FromText
    , patternFromText
    , pointFromText
    , headerHashFromText
    , slotNoFromText

    -- * NetworkParameters
    , NetworkParameters (..)
    , NetworkMagic (..)
    , EpochSlots (..)
    , SystemStart (..)
    , mkSystemStart

    -- * Tracer
    , TraceConfiguration (..)
    ) where

import Kupo.Prelude

import Cardano.Crypto.Hash
    ( Blake2b_256, hashFromTextAsHex, hashToBytesShort )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Data.Aeson
    ( (.:) )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Data.Time.Format.ISO8601
    ( iso8601ParseM )
import Kupo.Control.MonadLog
    ( HasSeverityAnnotation (..), Severity (..) )
import Kupo.Control.MonadOuroboros
    ( EpochSlots (..), NetworkMagic (..) )
import Kupo.Data.Cardano
    ( Block
    , pattern BlockPoint
    , pattern GenesisPoint
    , HeaderHash
    , Point (..)
    , SlotNo (..)
    )
import Kupo.Data.Pattern
    ( Pattern (..), patternFromText )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( SystemStart (..) )
import Ouroboros.Consensus.HardFork.Combinator
    ( OneEraHash (..) )

import qualified Data.Aeson as Json
import qualified Data.Text as T
import qualified Data.Text.Read as T

data Configuration = Configuration
    { nodeSocket :: !FilePath
    , nodeConfig :: !FilePath
    , workDir :: !WorkDir
    , serverHost :: !String
    , serverPort :: !Int
    , since :: !(Maybe (Point (Block StandardCrypto)))
    , patterns :: ![Pattern StandardCrypto]
    } deriving (Generic, Eq, Show)

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

-- | Construct a 'SystemStart' from a number of seconds.
mkSystemStart :: Int -> SystemStart
mkSystemStart =
    SystemStart . posixSecondsToUTCTime . toPicoResolution . toEnum
  where
    toPicoResolution = (*1000000000000)

-- | Parse a 'Point' from a text string. This alternatively tries two patterns:
--
-- - "origin"        → for a points that refers to the beginning of the blockchain
--
-- - "N.hhhh...hhhh" → A dot-separated integer and base16-encoded digest, which
--                     refers to a specific point on chain identified by this
--                     slot number and header hash.
--
pointFromText :: Text -> Maybe (Point (Block crypto))
pointFromText txt =
    genesisPointFromText <|> blockPointFromText
  where
    genesisPointFromText = GenesisPoint
        <$ guard (T.toLower txt == "origin")

    blockPointFromText = BlockPoint
        <$> slotNoFromText slotNo
        <*> headerHashFromText (T.drop 1 headerHash)
      where
        (slotNo, headerHash) = T.breakOn "." (T.strip txt)

-- | Parse a slot number from a text string.
slotNoFromText :: Text -> Maybe SlotNo
slotNoFromText txt = do
    (slotNo, remSlotNo) <- either (const Nothing) Just (T.decimal txt)
    guard (T.null remSlotNo)
    pure (SlotNo slotNo)

-- | Deserialise a 'HeaderHash' from a base16-encoded text string.
headerHashFromText
    :: Text
    -> Maybe (HeaderHash (Block crypto))
headerHashFromText =
    fmap (OneEraHash . hashToBytesShort) . hashFromTextAsHex @Blake2b_256

--
-- Tracer
--

data TraceConfiguration where
    ConfigurationNetwork
        :: { networkParameters :: NetworkParameters }
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
        ConfigurationInvalidOrMissingOption{} -> Error
