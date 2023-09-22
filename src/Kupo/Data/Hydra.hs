{-# LANGUAGE PatternSynonyms #-}

module Kupo.Data.Hydra where

import Kupo.Prelude

import Cardano.Crypto.Hash
    ( hashToBytes
    , hashWith
    )
import Data.Aeson
    ( (.:)
    )
import Kupo.Data.Cardano
    ( BlockNo (..)
    , SlotNo (..)
    , Tip
    , TransactionId
    , pattern BlockPoint
    , pattern Tip
    , unsafeHeaderHashFromBytes
    )
import Kupo.Data.PartialBlock
    ( PartialBlock (..)
    , PartialTransaction (PartialTransaction, datums, id, inputs, metadata, outputs, scripts)
    )

import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Builder as BS
import qualified Data.Map.Strict as Map
import Kupo.Data.Ogmios
    ( decodeTransactionId
    )

-- Types

data HydraMessage
    = HeadIsOpen
    | TxValid { tx :: PartialTransaction }
    | SnapshotConfirmed { snapshot :: Snapshot }
    | SomethingElse

data Snapshot = Snapshot
    { number :: Word64
    , confirmedTransactionIds :: [TransactionId]
    }

-- | Create a hydra "block" given a snapshot number and a list of transactions.
mkHydraBlock :: Word64 -> [PartialTransaction] -> (Tip, PartialBlock)
mkHydraBlock number txs = do
    let
        headerHash = number
            & hashWith @Blake2b_256 (toStrict . BS.toLazyByteString . BS.word64BE)
            & hashToBytes
            & unsafeHeaderHashFromBytes

        slotNo =
            SlotNo number

        blockNo =
            BlockNo number
     in
        ( Tip slotNo headerHash blockNo
        , PartialBlock
            { blockPoint = BlockPoint slotNo headerHash
            , blockBody  = txs
            }
        )

-- Decoders

decodeHydraMessage :: Json.Value -> Json.Parser HydraMessage
decodeHydraMessage =
    Json.withObject "HydraMessage" $ \o -> do
        tag <- o .: "tag"
        case tag of
            ("HeadIsOpen" :: Text) -> pure HeadIsOpen
            ("TxValid" :: Text) -> TxValid <$> decodeTxValid o
            ("SnapshotConfirmed" :: Text) -> SnapshotConfirmed <$> decodeSnapshotConfirmed o
            _ -> pure SomethingElse

decodeTxValid :: Json.Object -> Json.Parser PartialTransaction
decodeTxValid o = do
    tx <- o .: "transaction"
    id <- tx .: "id" >>= decodeTransactionId
    let inputs  = [] -- TODO
    let outputs = [] -- TODO
    let datums = Map.empty -- TODO
    let scripts = Map.empty -- TODO
    let metadata = Nothing -- TODO
    pure PartialTransaction
        { id
        , inputs
        , outputs
        , datums
        , scripts
        , metadata
        }

decodeSnapshotConfirmed :: Json.Object -> Json.Parser Snapshot
decodeSnapshotConfirmed o = do
    snapshot <- o .: "snapshot"
    number <- snapshot .: "snapshotNumber"
    confirmedTransactionIds <- snapshot .: "confirmedTransactions" >>= mapM decodeTransactionId
    pure Snapshot
        { number
        , confirmedTransactionIds
        }
