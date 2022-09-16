--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module Kupo.Data.Health
    ( -- * Health
      Health (..)
    , emptyHealth

      -- * ConnectionStatus
    , ConnectionStatus (..)
    ) where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( SlotNo
    , slotNoToJson
    )

import qualified Data.Aeson.Encoding as Json

-- | Information about the overall state of the application.
data Health = Health
    { connectionStatus :: !ConnectionStatus
        -- ^ Condition of the connection with the underlying node.
    , mostRecentCheckpoint :: !(Maybe SlotNo)
        -- ^ Absolute slot number of the most recent database checkpoint.
    , mostRecentNodeTip :: !(Maybe SlotNo)
        -- ^ Absolute slot number of the tip of the node
    } deriving stock (Generic, Eq, Show)

instance ToJSON Health where
    toEncoding Health{..} = Json.pairs $ mconcat
        [ Json.pair
            "connection_status"
            (toEncoding connectionStatus)
        , Json.pair
            "most_recent_checkpoint"
            (maybe Json.null_ slotNoToJson mostRecentCheckpoint)
        , Json.pair
            "most_recent_node_tip"
            (maybe Json.null_ slotNoToJson mostRecentNodeTip)
        ]

emptyHealth :: Health
emptyHealth = Health
    { connectionStatus = Disconnected
    , mostRecentCheckpoint = Nothing
    , mostRecentNodeTip = Nothing
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
