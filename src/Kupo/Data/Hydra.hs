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
    , OutputIndex
    , OutputReference
    , SlotNo (..)
    , Tip
    , TransactionId
    , Value
    , getOutputIndex
    , getTransactionId
    , mkOutput
    , mkOutputReference
    , outputIndexFromText
    , pattern BlockPoint
    , pattern Tip
    , transactionIdFromText
    , unsafeHeaderHashFromBytes
    , unsafeValueFromList
    , withReferences
    )
import Kupo.Data.PartialBlock
    ( PartialBlock (..)
    , PartialTransaction (PartialTransaction, datums, id, inputs, metadata, outputs, scripts)
    )

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
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
    = HeadIsOpen { genesisTxs :: [PartialTransaction] }
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
            ("HeadIsOpen" :: Text) -> HeadIsOpen <$> decodeHeadIsOpen o
            ("TxValid" :: Text) -> TxValid <$> (o .: "transaction" >>= decodePartialTransaction)
            ("SnapshotConfirmed" :: Text) -> SnapshotConfirmed <$> decodeSnapshotConfirmed o
            _ -> pure SomethingElse

-- | Decode a 'HeadIsOpen' as a multiple "genesis" transactions producing the
-- UTxO as initially available.
decodeHeadIsOpen :: Json.Object -> Json.Parser [PartialTransaction]
decodeHeadIsOpen o = do
    (Json.Object utxoMap) <- o .: "utxo"
    parsedUTxO <- forM (KeyMap.toList utxoMap) $ \(k,v) -> do
      txId <- decodeInput $ toJSON k
      pure (txId, v)
    let utxoByTxId = groupByTransactionId parsedUTxO
    forM utxoByTxId $ uncurry decodeGenesisTxForUTxO

groupByTransactionId :: [(OutputReference, a)] -> [(TransactionId, [(OutputIndex, a)])]
groupByTransactionId =
    Map.toList . foldr go mempty
  where
    go (oref, a) m =
        Map.unionWith (<>) m $
            Map.singleton (getTransactionId oref) [(getOutputIndex oref, a)]

decodeGenesisTxForUTxO :: TransactionId -> [(OutputIndex, Json.Value)] -> Json.Parser PartialTransaction
decodeGenesisTxForUTxO id indexOutputs = do
    outputs <- forM indexOutputs $ \(ix, v) -> do
      out <- decodeOutput v
      pure (mkOutputReference id ix, out)
    pure PartialTransaction
        { id
        , inputs = []
        , outputs
        , datums = mempty
        , scripts = mempty
        , metadata = Nothing
        }



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
        <*> (o .: "value" >>= decodeValue)
        <*> pure NoDatum -- TODO
        <*> pure Nothing -- TODO (o .:? "script" >>= traverse decodeScript)

-- XXX: Very similar to ogmios API (s/ada/lovelace/g)
decodeValue
    :: Json.Value
    -> Json.Parser Value
decodeValue = Json.withObject "Value(Hydra)" $ \o -> do
    coins <- o .: "lovelace"
    assets <- KeyMap.foldrWithKey
        (\k v accum ->
            if k == "lovelace" then accum else do
                policyId <- decodeBase16' (Key.toText k)
                assets <- decodeAssets policyId v
                xs <- accum
                pure (assets ++ xs)
        )
        (pure mempty)
        o
    pure (unsafeValueFromList coins assets)
  where
    decodeBase16' = either (fail . toString) pure . decodeBase16 . encodeUtf8

    decodeAssets
        :: ByteString
        -> Json.Value
        -> Json.Parser [(ByteString, ByteString, Integer)]
    decodeAssets policyId =
        Json.withObject "Assets" $ KeyMap.foldrWithKey
            (\k v accum -> do
                assetId <- decodeBase16' (Key.toText k)
                quantity <- parseJSON v
                xs <- accum
                pure ((policyId, assetId, quantity) : xs)
            )
            (pure mempty)

decodeSnapshotConfirmed :: Json.Object -> Json.Parser Snapshot
decodeSnapshotConfirmed o = do
    snapshot <- o .: "snapshot"
    number <- snapshot .: "snapshotNumber"
    confirmedTransactionIds <- snapshot .: "confirmedTransactions" >>= mapM decodeTransactionId
    pure Snapshot
        { number
        , confirmedTransactionIds
        }
