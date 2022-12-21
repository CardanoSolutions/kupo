module Kupo.Data.Cardano.OutputIndex where

import Kupo.Prelude

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Text as T
import qualified Data.Text.Read as T

type OutputIndex = Word16

outputIndexToJson :: OutputIndex -> Json.Encoding
outputIndexToJson =
    Json.integer . toInteger
{-# INLINABLE outputIndexToJson #-}

outputIndexFromText :: Text -> Maybe OutputIndex
outputIndexFromText txt = do
    (ix, remIx) <- either (const Nothing) Just (T.decimal txt)
    guard (T.null remIx)
    pure (fromInteger ix)
{-# INLINABLE outputIndexFromText #-}

outputIndexToText :: OutputIndex -> Text
outputIndexToText = show
{-# INLINABLE outputIndexToText #-}
