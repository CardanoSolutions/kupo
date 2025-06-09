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
import Data.Scientific
    ( FPFormat (Fixed)
    , Scientific
    )
import Data.Time
    ( UTCTime
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

import qualified Data.Aeson.Encoding as Json
import qualified Data.Map as Map

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

data SerialisableHealth = SerialisableHealth (Maybe NetworkParameters) Health

instance ToJSON SerialisableHealth where
    toJSON =
        error "'toJSON' called on 'Health'. This should never happen. Use 'toEncoding' instead."
    toEncoding (SerialisableHealth _ Health{..}) = Json.pairs $ mconcat
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

mkPrometheusMetrics :: Health -> Builder
mkPrometheusMetrics Health{..} =
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

        , [ ( "configuration_indexes"
            , mkGauge $ case indexes of
                SkipNonEssentialIndexes  -> 0
                InstallIndexesIfNotExist -> 1
            ) | Just indexes <- [configuration]
          ]
        ]
