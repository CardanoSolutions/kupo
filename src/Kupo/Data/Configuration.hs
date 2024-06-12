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
    , DatabaseLocation (..)
    , Since (..)
    , InputManagement (..)
    , ChainProducer (..)
    , LongestRollback (..)
    , DeferIndexesInstallation (..)
    , mailboxCapacity
    , pruneInputsMaxIncrement
    , maxReconnectionAttempts
    , isReadOnlyReplica

    -- * Signals
    , NodeTipHasBeenReachedException (..)
    , UnableToFetchBlockFromReadOnlyReplicaException (..)

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
import Text.URI
    ( URI
    )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json


-- | Application-level configuration.
data Configuration = Configuration
    { chainProducer :: !ChainProducer
        -- ^ Where the data comes from: cardano-node, ogmios, or hydra
        --
        -- NOTE: There's no bang pattern on this field because we do not want it
        -- to be unnecessarily evaluated in some test scenarios (e.g. state-machine)
        -- where we mock the chain producer.
        --
        -- This is slightly hacky, but the alternative would be to either split
        -- this type in two or to introduce some higher-kinded type parameter to
        -- the record. Both seems overly complicated given the benefits.
    , databaseLocation :: !DatabaseLocation
        -- ^ Where to store the data: in-memory vs specific location on-disk vs. remote URL for Postgres
    , serverHost :: !String
        -- ^ Hostname for the API HTTP server
    , serverPort :: !Int
        -- ^ Port for the API HTTP Server
    , since :: !(Maybe Since)
        -- ^ Point from when a *new* synchronization should start
    , patterns :: !(Set Pattern)
        -- ^ List of address patterns to look for when synchronizing
    , inputManagement :: !InputManagement
        -- ^ Behavior to adopt towards spent inputs (prune or mark-and-leave)
    , longestRollback :: !LongestRollback
        -- ^ Number of slots before which data can be considered immutable
    , garbageCollectionInterval :: !DiffTime
        -- ^ Delay between each garbage-collection of database data
    , deferIndexes :: !DeferIndexesInstallation
        -- ^ Whether to install non-essential database indexes on start-up.
        --
        -- Non-essential refers to indexes that make query faster but that aren't
        -- necessary for the application to synchronize at high speed.
    } deriving (Generic, Eq, Show)

isReadOnlyReplica :: Configuration -> Bool
isReadOnlyReplica Configuration{chainProducer} =
    case chainProducer of
        CardanoNode{} -> False
        Ogmios{} -> False
        Hydra{} -> False
        ReadOnlyReplica{} -> True

-- | Where does kupo pulls its data from. Both 'cardano-node' and 'ogmios' are
-- equivalent in the capabilities and information they offer; a cardano-node
-- will have to be through a local connection (domain socket) whereas ogmios can
-- happen _over the wire_ on a remote server but is slower overall. So both have
-- trade-offs. The 'hydra' chain producer is slightly different as it's "chain"
-- is actually the UTxO and transactions happening on the Hydra head layer 2.
data ChainProducer
    = CardanoNode
        { nodeSocket :: !FilePath
        , nodeConfig :: !FilePath
        }
    | Ogmios
        { ogmiosHost :: !String
        , ogmiosPort :: !Int
        }
    | Hydra
        { hydraHost :: !String
        , hydraPort :: !Int
        }
    | ReadOnlyReplica
        -- ^ A read-only replica will only watch the database and do not require a connection to the
        -- network. Such a replica therefore needs to be combined with a main writer. This would allow
        -- horizontally scaling kupo more easily, as new replicas can be added so long as the
        -- file-system supports it. Ultimately, the work that can be done on a single database file is
        -- bounded by the CPU capabilities and the I/O read access.
    deriving (Generic, Eq, Show)

-- | Captures the starting point of the indexer when the is no known checkpoint.
data Since = SinceTip | SincePoint Point
    deriving (Generic, Eq, Show)



-- | Database working directory. 'in-memory' runs the database in hot memory,
-- only suitable for non-permissive patterns or testing.
data DatabaseLocation
    = Dir !FilePath
    | InMemory !(Maybe FilePath)
    | Remote !URI
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

-- | Defer installation of non-essential database indexes. Non-essential refers to
-- indexes that are not crucial to synchronization but that are only useful for
-- speeding up certain lookup queries. Kupo makes a heavy use of database indexes on
-- columns and virtual columns which can increase the overall synchronization time
-- by a factor 2 or 3.
-- Maintaining these indexes during synchronization has little
-- benefits and can therefore be postponed to only after the database has been
-- fully synchronized.
data DeferIndexesInstallation
    = SkipNonEssentialIndexes
    | InstallIndexesIfNotExist
    deriving (Generic, Eq, Show)

instance ToJSON DeferIndexesInstallation where
    toEncoding = Json.text . \case
        SkipNonEssentialIndexes -> "deferred"
        InstallIndexesIfNotExist -> "installed"

-- | A signal sent by the consumer once the tip of the chain has been reached.
data NodeTipHasBeenReachedException = NodeTipHasBeenReached
    { distance :: Word64
    } deriving (Generic, Eq, Show)
instance Exception NodeTipHasBeenReachedException

-- | An exception which occurs when a client tries to fetch metadata from a read-only replicas.
data UnableToFetchBlockFromReadOnlyReplicaException = UnableToFetchBlockFromReadOnlyReplica
    deriving (Generic, Eq, Show)
instance Exception UnableToFetchBlockFromReadOnlyReplicaException where
    displayException _ =
        "An attempt to fetch a block from a read-only replica has occured likely \
        \caused by a request to access transaction metadata. This is, unfortunately, \
        \not a possible operation from a read-only replica. Only the master server \
        \can do that."

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

-- Maximum number of inputs pruned at a time. See note on 'pruneInputsQry'
pruneInputsMaxIncrement :: Int
pruneInputsMaxIncrement = 50000

-- Maximum number of attempts to acquire a database connection on a retrying strategy
maxReconnectionAttempts :: Word
maxReconnectionAttempts = 3

data NetworkParameters = NetworkParameters
    { networkMagic :: !NetworkMagic
    , systemStart :: !SystemStart
    , slotsPerEpoch :: !EpochSlots
    } deriving stock (Generic, Eq, Show)
      deriving anyclass (ToJSON)

deriving newtype instance ToJSON EpochSlots
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
