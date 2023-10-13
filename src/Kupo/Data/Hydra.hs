{-# LANGUAGE PatternSynonyms #-}

module Kupo.Data.Hydra where

import Kupo.Prelude

import Cardano.Crypto.Hash
    ( hashFromTextAsHex
    , hashToBytes
    , hashWith
    )
import Cardano.Ledger.SafeHash
    ( unsafeMakeSafeHash
    )
import Data.Aeson
    ( (.!=)
    , (.:)
    , (.:?)
    )
import Kupo.Data.Cardano
    ( BinaryData
    , BlockNo (..)
    , Datum (..)
    , DatumHash
    , Input
    , Output
    , OutputIndex
    , OutputReference
    , Script
    , ScriptHash
    , SlotNo (..)
    , Tip
    , TransactionId
    , Value
    , binaryDataFromBytes
    , datumHashFromBytes
    , getOutputIndex
    , getTransactionId
    , mkOutput
    , mkOutputReference
    , outputIndexFromText
    , pattern BlockPoint
    , pattern Tip
    , scriptFromBytes
    , scriptHashFromText
    , transactionIdFromText
    , transactionIdToBytes
    , unsafeHeaderHashFromBytes
    , unsafeValueFromList
    , withReferences
    )
import Kupo.Data.Ogmios
    ( decodeAddress
    , decodeTransactionId
    )
import Kupo.Data.PartialBlock
    ( PartialBlock (..)
    , PartialTransaction (..)
    )

import qualified Codec.CBOR.Decoding as Cbor
import qualified Codec.CBOR.Read as Cbor
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

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

mkHydraBlock :: Word64 -> [PartialTransaction] -> (Tip, PartialBlock)
mkHydraBlock number txs = do
    let
        headerHash = txs
            & foldr (\PartialTransaction{id} -> (B.byteString (transactionIdToBytes id) <>)) mempty
            & hashWith @Blake2b_256 (toStrict . B.toLazyByteString)
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
            , blockBody  = toList txs
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
    forM
        (groupByTransactionId parsedUTxO)
        (uncurry decodeGenesisTxForUTxO)

groupByTransactionId
    :: [(OutputReference, a)]
    -> [(TransactionId, [(OutputIndex, a)])]
groupByTransactionId =
    Map.toList . foldr go mempty
  where
    go (oref, a) m =
        Map.unionWith (<>) m $
            Map.singleton (getTransactionId oref) [(getOutputIndex oref, a)]

decodeGenesisTxForUTxO
    :: TransactionId
    -> [(OutputIndex, Json.Value)]
    -> Json.Parser PartialTransaction
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

decodePartialTransaction :: Json.Value -> Json.Parser PartialTransaction
decodePartialTransaction = Json.withObject "PartialTransaction" $ \o -> do
    id <- o .: "id" >>= decodeTransactionId

    body <- o .: "body"
    inputs <- body .: "inputs" >>= traverse decodeInput
    outputs <- body .:? "outputs" .!= [] >>= traverse decodeOutput

    wits <- o.: "witnesses"
    datums <- wits .:? "datums" .!= Json.Object mempty >>= decodeDatums
    scripts <- wits .:? "scripts" .!= Json.Object mempty >>= decodeScripts

    -- TODO
    -- This is 'acceptable' for now because:
    --
    -- (1) This is only truly required when fetching metadata from a data-source. Kupo does not
    -- itsself store metadata, so they have no effect when folding over blocks.
    --
    -- (2) Hydra does not support fetching metadata of past transactions. If we wanted to support this
    -- feature for Hydra, we would need to first deal with (1) since Hydra doesn't provide a protocol
    -- / API for it.
    let metadata = Nothing

    pure PartialTransaction
        { id
        , inputs
        , outputs = withReferences 0 id outputs
        , datums
        , scripts
        , metadata
        }

decodeDatums :: Json.Value -> Json.Parser (Map DatumHash BinaryData)
decodeDatums = Json.withObject "Datums" $
    KeyMap.foldrWithKey
        (\k v accum -> Map.insert
            <$> decodeDatumHash k
            <*> decodeBinaryData v
            <*> accum
        )
        (pure mempty)

decodeDatumHash
    :: Json.Key
    -> Json.Parser DatumHash
decodeDatumHash k = do
    case datumHashFromBytes <$> decodeBase16 (encodeUtf8 (Key.toText k)) of
        Right (Just hash) ->
            pure hash
        Right Nothing ->
            fail "decodeDatumHash: datumHashFromBytes failed."
        Left e ->
            fail (toString e)

decodeInput
    :: Json.Value
    -> Json.Parser Input
decodeInput = Json.withText "Input" $ \t ->
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
decodeOutput = Json.withObject "Output" $ \o -> do
    datumHash <- o .:? "datumHash" >>=
        traverse (fmap unsafeMakeSafeHash . decodeHash @Blake2b_256)
    datum <- o .:? "datum"
    mkOutput
        <$> (o .: "address" >>= decodeAddress)
        <*> (o .: "value" >>= decodeValue)
        <*> case (datumHash, datum) of
                (Just x, _) ->
                    pure (Reference (Left x))
                (Nothing, Just x) ->
                    Inline . Right <$> decodeBinaryData x
                (Nothing, Nothing) ->
                    pure NoDatum
        <*> (o .:? "script" >>= traverse decodeScript)

decodeHash
    :: HashAlgorithm alg
    => Json.Value
    -> Json.Parser (Hash alg a)
decodeHash =
    Json.parseJSON >=> maybe empty pure . hashFromTextAsHex

decodeBinaryData
    :: Json.Value
    -> Json.Parser BinaryData
decodeBinaryData = Json.withText "BinaryData" $ \t ->
    case binaryDataFromBytes <$> decodeBase16 (encodeUtf8 t) of
        Right (Just bin) ->
            pure bin
        Right Nothing ->
            fail "decodeBinaryData: binaryDataFromBytes failed."
        Left e ->
            fail (toString e)

decodeScript
    :: Json.Value
    -> Json.Parser Script
decodeScript = Json.withText "Script" $ \bytes -> do
    case scriptFromBytes' <$> decodeBase16 (encodeUtf8 @Text bytes) of
        Right (Just s) ->
            pure s
        Right Nothing ->
            fail "decodeScript: malformed script"
        Left e ->
            fail $ "decodeScript: not base16: " <> show e
  where
    scriptFromBytes' (toLazy -> bytes) = do
        (toStrict -> script, tag) <- either (fail . show) pure $
            Cbor.deserialiseFromBytes (Cbor.decodeListLen >> Cbor.decodeWord8) bytes
        maybe (fail "decodeScript: malformed script") pure $
            scriptFromBytes (BS.singleton tag <> script)

decodeScriptInEnvelope
    :: Json.Value
    -> Json.Parser Script
decodeScriptInEnvelope = Json.withObject "ScriptInEnvelope" $ \o -> do
    bytes <- o .: "script" >>= (.: "cborHex") >>= decodeBase16'
    nestedBytes <- either (fail . show) (pure . snd) $
        Cbor.deserialiseFromBytes Cbor.decodeBytes (toLazy bytes)
    o .: "scriptLanguage" >>= \case
        "SimpleScriptLanguage" ->
            scriptFromBytes' (BS.pack [0] <> nestedBytes)
        "PlutusScriptLanguage PlutusScriptV1" ->
            scriptFromBytes' (BS.pack [1] <> nestedBytes)
        "PlutusScriptLanguage PlutusScriptV2" ->
            scriptFromBytes' (BS.pack [2] <> nestedBytes)
        "PlutusScriptLanguage PlutusScriptV3" ->
            scriptFromBytes' (BS.pack [3] <> nestedBytes)
        (_ :: Text) ->
            fail "unrecognized script language"
  where
    scriptFromBytes' =
        maybe (fail "decodeScript: malformed script") pure . scriptFromBytes

decodeScripts :: Json.Value -> Json.Parser (Map ScriptHash Script)
decodeScripts = Json.withObject "Scripts" $
    KeyMap.foldrWithKey
        (\k v accum -> Map.insert
            <$> decodeScriptHash k
            <*> decodeScript v
            <*> accum
        )
        (pure mempty)

decodeScriptHash
    :: Json.Key
    -> Json.Parser ScriptHash
decodeScriptHash k =
    case scriptHashFromText (Key.toText k) of
        Nothing -> fail "decodeScriptHash"
        Just scriptHash -> pure scriptHash

decodeSnapshotConfirmed :: Json.Object -> Json.Parser Snapshot
decodeSnapshotConfirmed o = do
    snapshot <- o .: "snapshot"
    number <- snapshot .: "snapshotNumber"
    confirmedTransactionIds <- snapshot .: "confirmedTransactions" >>= mapM decodeTransactionId
    pure Snapshot
        { number
        , confirmedTransactionIds
        }

decodeValue
    :: Json.Value
    -> Json.Parser Value
decodeValue = Json.withObject "Value" $ \o -> do
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

decodeBase16' :: Text -> Json.Parser ByteString
decodeBase16' =
    either (fail . toString) pure . decodeBase16 . encodeUtf8
