module Kupo.Data.Cardano.TransactionIndex where

import Kupo.Prelude

import qualified Data.Aeson as Json

type TransactionIndex = Word16

transactionIndexToJson :: TransactionIndex -> Json.Encoding
transactionIndexToJson = toEncoding
