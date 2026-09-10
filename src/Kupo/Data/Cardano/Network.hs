module Kupo.Data.Cardano.Network where

import Kupo.Prelude

data Network = Mainnet | Preprod | Preview
    deriving (Generic, Eq, Show)

networkFromString :: String -> Maybe Network
networkFromString "mainnet" = Just Mainnet
networkFromString "preprod" = Just Preprod
networkFromString "preview" = Just Preview
networkFromString _         = Nothing
