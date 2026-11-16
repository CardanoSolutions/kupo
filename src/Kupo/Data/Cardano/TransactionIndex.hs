module Kupo.Data.Cardano.TransactionIndex where

import Kupo.Prelude

import qualified Data.Aeson as Json

type TransactionIndex = Word16

-- | Position of a sub-transaction among the sub-transactions of its top-level transaction.
type SubTransactionIndex = Word64

transactionIndexToJson :: TransactionIndex -> Json.Encoding
transactionIndexToJson = toEncoding
