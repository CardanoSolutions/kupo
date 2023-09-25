{-# LANGUAGE PatternSynonyms #-}

module Kupo.Data.Hydra where

import Kupo.Prelude

import Cardano.Crypto.Hash
    ( hashToBytes
    , hashWith
    )
import Data.Aeson
    ( (.!=)
    , (.:)
    , (.:?)
    )
import Kupo.Data.Cardano
    ( BlockNo (..)
    , Datum (..)
    , Input
    , Output
    , SlotNo (..)
    , Tip
    , TransactionId
    , mkOutput
    , mkOutputReference
    , outputIndexFromText
    , pattern BlockPoint
    , pattern Tip
    , transactionIdFromText
    , unsafeHeaderHashFromBytes
    , withReferences
    )
import Kupo.Data.PartialBlock
    ( PartialBlock (..)
    , PartialTransaction (PartialTransaction, datums, id, inputs, metadata, outputs, scripts)
    )

import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Builder as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Kupo.Data.Ogmios
    ( decodeAddress
    , decodeTransactionId
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
            ("TxValid" :: Text) -> TxValid <$> (o .: "transaction" >>= decodePartialTransaction)
            ("SnapshotConfirmed" :: Text) -> SnapshotConfirmed <$> decodeSnapshotConfirmed o
            _ -> pure SomethingElse

-- | Decoder for a 'PartialTransaction' in the Hydra JSON schema.
decodePartialTransaction :: Json.Value -> Json.Parser PartialTransaction
decodePartialTransaction = Json.withObject "PartialTransaction(Hydra)" $ \o -> do
    id <- o .: "id" >>= decodeTransactionId
    body <- o .: "body"
    inputs <- body .: "inputs" >>= traverse decodeInput
    outputs <- body .:? "outputs" .!= [] >>= traverse decodeOutput
    let datums = Map.empty -- TODO
    let scripts = Map.empty -- TODO
    let metadata = Nothing -- TODO
    pure PartialTransaction
        { id
        , inputs
        , outputs = withReferences 0 id outputs
        , datums
        , scripts
        , metadata
        }

decodeInput
    :: Json.Value
    -> Json.Parser Input
decodeInput = Json.withText "Input(Hydra)" $ \t ->
    maybe (fail $ "failed to parse: " <> show t) pure $ do
        (tId, tIx) <- splitInput t
        id <- transactionIdFromText tId
        ix <- outputIndexFromText tIx
        pure $ mkOutputReference id ix
 where
    splitInput t =
        case Text.split (== '#') t of
            [tId, tIx] -> Just (tId, tIx)
            _ -> Nothing

decodeOutput
    :: Json.Value
    -> Json.Parser Output
decodeOutput = Json.withObject "Output(Hydra)" $ \o -> do
    mkOutput
        <$> (o .: "address" >>= decodeAddress)
        <*> pure mempty -- TODO (o .: "value" >>= decodeValue)
        <*> pure NoDatum -- TODO
        <*> pure Nothing -- TODO (o .:? "script" >>= traverse decodeScript)


decodeSnapshotConfirmed :: Json.Object -> Json.Parser Snapshot
decodeSnapshotConfirmed o = do
    snapshot <- o .: "snapshot"
    number <- snapshot .: "snapshotNumber"
    confirmedTransactionIds <- snapshot .: "confirmedTransactions" >>= mapM decodeTransactionId
    pure Snapshot
        { number
        , confirmedTransactionIds
        }
