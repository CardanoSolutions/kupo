--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.PartialBlock
    ( PartialBlock (..)
    , PartialTransaction(..)
    ) where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( BinaryData
    , DatumHash
    , HasTransactionId (..)
    , Input
    , IsBlock (..)
    , Metadata
    , MetadataHash
    , Output
    , OutputReference
    , Point
    , Script
    , ScriptHash
    , StandardCrypto
    , TransactionId
    )

import qualified Data.Set as Set

-- | A partial representation of a Cardano Block. This only contains bits that
-- are relevant to kupo. Rest isn't indexed.
data PartialBlock = PartialBlock
    { blockPoint :: !Point
    , blockBody :: ![PartialTransaction]
    } deriving (Eq, Show)

-- | A partial transaction, analogous to 'PartialBlock', trimmed down to the
-- minimum.
data PartialTransaction = PartialTransaction
    { id :: !TransactionId
    , inputs :: ![Input]
    , outputs :: ![(OutputReference, Output)]
    , datums :: !(Map DatumHash BinaryData)
    , scripts :: !(Map ScriptHash Script)
    , metadata :: !(Maybe (MetadataHash, Metadata))
    } deriving (Eq, Show)

instance HasTransactionId PartialTransaction StandardCrypto where
    getTransactionId = id

instance IsBlock PartialBlock where
    type BlockBody PartialBlock = PartialTransaction

    getPoint =
        blockPoint

    spentInputs =
        Set.fromList . inputs

    foldBlock fn result =
        foldrWithIndex fn result . blockBody

    mapMaybeOutputs fn  =
        mapMaybe (uncurry fn) . outputs

    witnessedDatums =
        datums

    witnessedScripts =
        scripts

    userDefinedMetadata =
        metadata
