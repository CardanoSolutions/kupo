--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module Kupo.Data.Health
    ( -- * Health
      Health (..)
    , SerialisableHealth (..)
    , emptyHealth

      -- * ConnectionStatus
    , ConnectionStatus (..)

    , mkPrometheusMetrics
    ) where

import Kupo.Prelude

import Data.ByteString.Builder
    ( Builder
    )
import Data.ByteString.Builder.Scientific
    ( formatScientificBuilder
    )
import Data.Ratio
    ( (%)
    )
import Data.Scientific
    ( FPFormat (Fixed)
    , Scientific
    )
import Data.Time
    ( NominalDiffTime
    , UTCTime
    , diffUTCTime
    )
import Kupo.Data.Cardano
    ( Point
    , SlotNo (..)
    , getPointSlotNo
    , slotNoToJson
    )
import Kupo.Data.Configuration
    ( DeferIndexesInstallation (..)
    , NetworkParameters (..)
    , SystemStart (..)
    )
import Kupo.Version
    ( version
    )
import System.Metrics.Prometheus.Encode.Text
    ( encodeMetrics
    )
import System.Metrics.Prometheus.Metric
    ( MetricSample (..)
    )
import System.Metrics.Prometheus.Metric.Counter
    ( CounterSample (..)
    )
import System.Metrics.Prometheus.Metric.Gauge
    ( GaugeSample (..)
    )
import System.Metrics.Prometheus.MetricId
    ( Labels (..)
    , MetricId (..)
    , makeName
    )
import System.Metrics.Prometheus.Registry
    ( RegistrySample (..)
    )
import Ouroboros.Network.Magic
    ( NetworkMagic (..)
    )

import qualified Data.Aeson.Encoding as Json
import qualified Data.Map as Map
import qualified Data.Scientific as Scientific

-- | Information about the overall state of the application.
data Health = Health
    { connectionStatus :: !ConnectionStatus
        -- ^ Condition of the connection with the underlying node.
    , mostRecentCheckpoint :: !(Maybe Point)
        -- ^ Absolute slot number of the most recent database checkpoint.
    , mostRecentNodeTip :: !(Maybe SlotNo)
        -- ^ Absolute slot number of the tip of the node
    , mostRecentClockTick :: !(Maybe UTCTime)
        -- ^ The clock value when the health was last updated
    , configuration :: !(Maybe DeferIndexesInstallation)
        -- ^ Some useful server configuration
    } deriving stock (Generic, Eq, Show)

data SerialisableHealth = SerialisableHealth
    { health :: Health
    , networkParameters :: Maybe NetworkParameters
    , currentTime :: UTCTime
    }

instance ToJSON SerialisableHealth where
    toJSON =
        error "'toJSON' called on 'Health'. This should never happen. Use 'toEncoding' instead."
    toEncoding (SerialisableHealth Health{..} optNetworkParameters now) = Json.pairs $ mconcat
        [ Json.pair
            "connection_status"
            (toEncoding connectionStatus)
        , Json.pair
            "most_recent_checkpoint"
            (maybe Json.null_ (slotNoToJson . getPointSlotNo) mostRecentCheckpoint)
        , Json.pair
            "most_recent_node_tip"
            (maybe Json.null_ slotNoToJson mostRecentNodeTip)
        , Json.pair
            "seconds_since_last_block"
            (maybe Json.null_ (nominalDiffTimeToJson . max 0 . diffUTCTime now) mostRecentClockTick)
        , Json.pair
            "network_synchronization"
            (maybe Json.null_ toEncoding
                (liftA2 (mkNetworkSynchronization now)
                    optNetworkParameters
                    (getPointSlotNo <$> mostRecentCheckpoint)
                )
            )
         , Json.pair
            "configuration"
            (Json.pairs $ mconcat
                [ Json.pair
                    "indexes"
                    (maybe Json.null_ toEncoding configuration)
                ]
            )
        , Json.pair
            "version"
            (toEncoding version)
        ]

-- | Encode a 'NominalDiffTime' as an integer representing number of seconds.
nominalDiffTimeToJson :: NominalDiffTime -> Json.Encoding
nominalDiffTimeToJson =
    Json.integer . nominalDiffTimeToInteger

-- | Encode a 'NominalDiffTime' as an integer representing number of seconds.
nominalDiffTimeToInteger :: NominalDiffTime -> Integer
nominalDiffTimeToInteger =
    (`div` 10^(12::Integer)) . toInteger . fromEnum

emptyHealth :: Health
emptyHealth = Health
    { connectionStatus = Disconnected
    , mostRecentCheckpoint = Nothing
    , mostRecentNodeTip = Nothing
    , mostRecentClockTick = Nothing
    , configuration = Nothing
    }

-- | Reflect the current state of the connection with the underlying node.
data ConnectionStatus
    = Connected
    | Disconnected
    deriving stock (Generic, Eq, Show, Enum, Bounded)

instance ToJSON ConnectionStatus where
    toEncoding = \case
        Connected -> Json.text "connected"
        Disconnected -> Json.text "disconnected"

-- | Captures how far is our underlying node from the network, in percentage.
newtype NetworkSynchronization = NetworkSynchronization Scientific
    deriving stock (Generic, Eq, Ord, Show)

instance ToJSON NetworkSynchronization where
    toJSON _ =
        error "'toJSON' called on 'NetworkSynchronization'. This should never happen. Use 'toEncoding' instead."
    toEncoding (NetworkSynchronization s) =
        -- NOTE: Using a specific encoder here to avoid turning the value into
        -- scientific notation. Indeed, for small decimals values, aeson
        -- automatically turn the representation into scientific notation with
        -- exponent. While this is useful and harmless in many cases, it makes
        -- consuming this value a bit harder from scripts. Since we know (by
        -- construction) that the network value will necessarily have a maximum
        -- of 5 decimals, we encode it as a number with a fixed number (=5) of
        -- decimals.
        --
        -- >>> encode (NetworkSynchronization 1.0)
        -- 1.00000
        --
        -- >>> encode (NetworkSynchronization 1.4e-3)
        -- 0.00140
        --
        -- etc...
        Json.unsafeToEncoding (formatScientificBuilder Fixed (Just 5) s)

mkNetworkSynchronization
    :: UTCTime -- Current Time
    -> NetworkParameters
    -> SlotNo -- Last known tip slot
    -> NetworkSynchronization
mkNetworkSynchronization now NetworkParameters { systemStart = SystemStart systemStart, networkMagic } (SlotNo lastKnownTip) =
    let
        tip = fromIntegral @_ @Integer lastKnownTip

        -- Number of slots that happened in the Byron era, based on the Network Magic
        offset = case networkMagic of
            NetworkMagic 764824073 -> 4492799
            NetworkMagic 1 -> 84242
            NetworkMagic 2 -> 0
            _ -> 0

        byronSlotLength = 20

        preByron
            | tip >= offset = offset * byronSlotLength
            | otherwise = tip * byronSlotLength

        postByron
            | tip >= offset = tip - offset
            | otherwise = 0

        num = preByron + postByron
        den = round @_ @Integer (now `diffUTCTime` systemStart) + offset * (byronSlotLength - 1)

        tolerance = 120
        p = 100000
    in
        if abs (num - den) <= tolerance then
            NetworkSynchronization 1
        else
            NetworkSynchronization
                $ Scientific.unsafeFromRational
                $ min 1 (((num * p) `div` den) % p)

mkPrometheusMetrics :: UTCTime -> Maybe NetworkParameters -> Health -> Builder
mkPrometheusMetrics now optNetworkParameters Health{..} =
    prometheusMetrics
    & Map.fromList
    & Map.mapKeys (\k -> (MetricId (makeName $ "kupo_" <> k) (Labels mempty)))
    & RegistrySample
    & encodeMetrics
  where
    mkGauge :: Double -> MetricSample
    mkGauge = GaugeMetricSample . GaugeSample

    mkCounter :: Int -> MetricSample
    mkCounter = CounterMetricSample . CounterSample

    networkSynchronization :: Maybe NetworkSynchronization
    networkSynchronization = liftA2
        (mkNetworkSynchronization now)
        optNetworkParameters
        (getPointSlotNo <$> mostRecentCheckpoint)

    prometheusMetrics :: [(Text, MetricSample)]
    prometheusMetrics = mconcat
        [ [ ( "connection_status"
            , mkGauge $ case connectionStatus of
                Connected -> 1
                Disconnected -> 0
            )
          ]

        , [ ( "most_recent_checkpoint"
            , mkCounter $ fromEnum $ unSlotNo $ getPointSlotNo s
            ) | Just s <- [mostRecentCheckpoint]
          ]

        , [ ( "most_recent_node_tip"
            , mkCounter $ fromEnum $ unSlotNo s
            ) | Just s <- [mostRecentNodeTip]
          ]

        , [ ( "seconds_since_last_block"
            , mkGauge $ fromIntegral $ nominalDiffTimeToInteger $ max 0 $ diffUTCTime now t
            ) | Just t <- [mostRecentClockTick]
          ]

        , [ ( "network_synchronization"
            , mkGauge $ Scientific.toRealFloat s
            ) | Just (NetworkSynchronization s) <- [networkSynchronization]
          ]

        , [ ( "configuration_indexes"
            , mkGauge $ case indexes of
                SkipNonEssentialIndexes  -> 0
                InstallIndexesIfNotExist -> 1
            ) | Just indexes <- [configuration]
          ]
        ]
