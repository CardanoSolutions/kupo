--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kupo.Data.Configuration
    (
    -- * Configuration
      Configuration (..)
    , WorkDir (..)
    , InputManagement (..)
    , ChainProducer (..)
    , LongestRollback (..)
    , mailboxCapacity
    , maxInFlight

    -- * NetworkParameters
    , NetworkParameters (..)

    -- ** Parameters Components
    , NetworkMagic (..)
    , EpochSlots (..)
    , SystemStart (..)
    , mkSystemStart
    ) where

import Kupo.Prelude

import Data.Aeson
    ( (.:)
    )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    )
import Data.Time.Format.ISO8601
    ( iso8601ParseM
    )
import Kupo.Control.MonadOuroboros
    ( EpochSlots (..)
    , NetworkMagic (..)
    )
import Kupo.Control.MonadTime
    ( DiffTime
    )
import Kupo.Data.Cardano
    ( Point
    )
import Kupo.Data.Pattern
    ( Pattern (..)
    )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( SystemStart (..)
    )

import qualified Data.Aeson as Json


-- | Application-level configuration.
data Configuration = Configuration
    { chainProducer :: ChainProducer
        -- ^ Where the data comes from: cardano-node vs ogmios
        --
        -- NOTE: There's no bang pattern on this field because we do not want it
        -- to be unnecessarily evaluated in some test scenarios (e.g. state-machine)
        -- where we mock the chain producer.
        --
        -- This is slightly hacky, but the alternative would be to either split
        -- this type in two or to introduce some higher-kinded type parameter to
        -- the record. Both seems overly complicated given the benefits.
    , workDir :: !WorkDir
        -- ^ Where to store the data: in-memory vs specific location on-disk
    , serverHost :: !String
        -- ^ Hostname for the API HTTP server
    , serverPort :: !Int
        -- ^ Port for the API HTTP Server
    , since :: !(Maybe Point)
        -- ^ Point from when a *new* synchronization should start
    , patterns :: ![Pattern]
        -- ^ List of address patterns to look for when synchronizing
    , inputManagement :: !InputManagement
        -- ^ Behavior to adopt towards spent inputs (prune or mark-and-leave)
    , longestRollback :: !LongestRollback
        -- ^ Number of slots before which data can be considered immutable
    , garbageCollectionInterval :: !DiffTime
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
    = Dir !FilePath
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

-- | A thin wrapper around the number of slots we consider to be the _longest
-- rollback_. This impacts how long we have to keep data in the database before
-- removing it, as well as how we create checkpoints when looking for chain
-- intersection with the chain-producer.
newtype LongestRollback = LongestRollback
    { getLongestRollback :: Word64
    } deriving newtype (Integral, Real, Num, Enum, Ord, Eq, Show)

-- Mailbox's capacity, or how many messages can be enqueued in the queue between
-- the consumer and the producer workers. More means faster synchronization (up
-- to a certain level) but also higher memory consumption. We try to stay under
-- ~1GB of RAM so 500-600 seems to be a good number.
--
-- Note that this is intrinsically linked to the `-A` runtime flag. Higher
-- capacity requires a larger allocation area. Because of this, and because
-- users would in general not necessarily know what to put, this isn't
-- configurable as such. Sensible numbers were picked.
mailboxCapacity :: Natural
mailboxCapacity = 200
{-# INLINABLE mailboxCapacity #-}

-- | Maximum pipelining at any given time. No need to go too high here, it only
-- arms performance beyond a certain point. This also goes hand-in-hand with the
-- 'mailboxCapacity'
maxInFlight :: Int
maxInFlight = 100
{-# INLINABLE maxInFlight #-}

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
