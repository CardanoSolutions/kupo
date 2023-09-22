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
    , PartialTransaction
    )

import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Builder as BS

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
            ("TxValid" :: Text) -> TxValid <$> undefined
            ("SnapshotConfirmed" :: Text) -> SnapshotConfirmed <$> decodeSnapshotConfirmed o
            _ -> pure SomethingElse

decodeSnapshotConfirmed :: Json.Object -> Json.Parser Snapshot
decodeSnapshotConfirmed o = do
    snapshot <- o .: "snapshot"
    number <- snapshot .: "snapshotNumber"
    pure $ Snapshot { number, confirmedTransactionIds = undefined }
