--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}

module Kupo.Data.Ogmios
    ( PartialBlock (..)
    , PartialTransaction (..)

    , NextBlockResponse (..)

    , encodeFindIntersectionRequest
    , encodeNextBlockRequest

    , decodeFindIntersectionResponse
    , decodeNextBlockResponse
    -- ** Cardano decoders
    , decodeTransactionId
    , decodeAddress
    ) where

import Kupo.Prelude

import Cardano.Crypto.Hash.Class
    ( hashFromTextAsHex
    , hashToBytes
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
    ( Address
    , BinaryData
    , BlockNo (..)
    , Datum (..)
    , DatumHash
    , Input
    , InputIndex
    , KeyHash (..)
    , Metadata
    , MetadataHash
    , Metadatum
    , NativeScript
    , Output
    , Point
    , Script
    , ScriptHash
    , SlotNo (..)
    , Tip
    , TransactionId
    , Value
    , WithOrigin (..)
    , binaryDataFromBytes
    , datumHashFromBytes
    , fromNativeScript
    , headerHashToJson
    , metadataHashFromText
    , mkMetadata
    , mkOutput
    , mkOutputReference
    , pattern BlockPoint
    , pattern GenesisPoint
    , pattern GenesisTip
    , pattern RequireAllOf
    , pattern RequireAnyOf
    , pattern RequireMOf
    , pattern RequireSignature
    , pattern RequireTimeExpire
    , pattern RequireTimeStart
    , pattern Tip
    , scriptFromBytes
    , scriptHashFromText
    , slotNoToJson
    , transactionIdFromHash
    , unsafeValueFromList
    , withReferences
    )
import Kupo.Data.PartialBlock
    ( PartialBlock (..)
    , PartialTransaction (..)
    )
import Kupo.Data.Pattern
    ( Pattern (..)
    , patternFromText
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoEras
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( OneEraHash (..)
    )

import qualified Cardano.Ledger.Shelley.TxAuxData as Ledger
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Json
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Text as T
import qualified Data.Text.Read as T

-- RequestNextResponse

data NextBlockResponse
    = RollBackward !Tip !Point
    | RollForward  !Tip !PartialBlock

-- ScriptPurpose

data ScriptPurpose
    = Mint
    | Spend
    | Publish
    | Withdraw
    | Vote
    | Propose
    deriving (Eq)

-- Encoders

encodeFindIntersectionRequest :: [Point] -> Json.Encoding
encodeFindIntersectionRequest pts = Json.pairs $ mempty
    <> Json.pair "jsonrpc" (Json.text "2.0")
    <> Json.pair "method" (Json.text "findIntersection")
    <> Json.pair "params" (Json.pairs $ Json.pair "points" (Json.list encodePoint pts))

encodeNextBlockRequest :: Json.Encoding
encodeNextBlockRequest = Json.pairs $ mempty
    <> Json.pair "jsonrpc" (Json.text "2.0")
    <> Json.pair "method" (Json.text "nextBlock")

encodePoint :: Point -> Json.Encoding
encodePoint = \case
    GenesisPoint ->
        Json.text "origin"
    BlockPoint slotNo headerHash ->
        Json.pairs $ mconcat
            [ Json.pair "slot" (slotNoToJson slotNo)
            , Json.pair "id" (headerHashToJson headerHash)
            ]

-- Decoders

decodeFindIntersectionResponse
    :: (WithOrigin SlotNo -> e)
    -> Json.Value
    -> Json.Parser (Either e (Point, Tip))
decodeFindIntersectionResponse wrapException = do
    Json.withObject "FindIntersectionResponse" $ \o -> do
        o .:? "result" >>= \case
            Just result -> do
                intersection <- result .: "intersection" >>= decodePointOrOrigin
                tip <- result .: "tip" >>= decodeTipOrOrigin
                return (Right (intersection, tip))
            _ -> do
                e <- o .: "error" >>= (.:? "data")
                tip <-  traverse (.: "tip") e .!= Json.String "origin" >>= decodeSlotNoOrOrigin
                return (Left (wrapException tip))

decodeNextBlockResponse
    :: Json.Value
    -> Json.Parser NextBlockResponse
decodeNextBlockResponse =
    Json.withObject "NextBlockResponse" $ \o -> do
        result <- o .: "result"
        result .: "direction" >>= \case
            ("forward" :: Text) -> decodeRollForward result
            _ -> decodeRollBackward result
  where
    decodeRollForward result = do
        block  <- result .: "block"
        tip    <- result .: "tip" >>= decodeTipOrOrigin
        RollForward tip <$> decodeBlock block

    decodeRollBackward result = do
        point  <- result .: "point" >>= decodePointOrOrigin
        tip    <- result .: "tip"   >>= decodeTipOrOrigin
        return (RollBackward tip point)

decodeBlock
    :: Json.Value
    -> Json.Parser PartialBlock
decodeBlock = Json.withObject "Block" $ \o -> do
    id <- o .: "id" >>= decodeOneEraHash
    slot <- o .:? "slot" >>= \case
        Just slot -> pure slot
        Nothing -> o .: "height"
    txs <- o .:? "transactions" .!= []
    PartialBlock (BlockPoint (SlotNo slot) id) <$> traverse decodePartialTransaction txs

decodePartialTransaction
    :: Json.Value
    -> Json.Parser PartialTransaction
decodePartialTransaction = Json.withObject "PartialTransaction" $ \o -> do
    id <- o .: "id" >>= decodeTransactionId

    inputSource <- o .: "spends"

    inputs <- o .:? inputSource .!= [] >>= traverse decodeInput

    metadata <- o .:? "metadata" >>= \case
        Nothing -> pure Nothing
        Just m -> do
            k <- m .: "hash" >>= decodeMetadataHash
            v <- m .: "labels" >>= decodeMetadata
            pure $ Just (k, v)

    datums <- o .:? "datums" .!= Json.Object mempty >>= decodeDatums

    spendRedeemers <- o .:? "redeemers" .!= [] >>= decodeSpendRedeemers

    scripts <- o .:? "scripts" .!= Json.Object mempty >>= decodeScripts

    outputs <- case Key.toText inputSource of
        "collaterals" -> do
            outs <- o .:? "outputs" .!= ([] :: [Json.Value])
            colReturn <- o .:? "collateralReturn" >>= traverse decodeOutput
            pure $ withReferences (fromIntegral $ length outs) id (maybeToList colReturn)
        "inputs" -> do
            outs <- o .:? "outputs" .!= [] >>= traverse decodeOutput
            pure $ withReferences 0 id outs
        (_ :: Text) -> do
            fail "unrecognized input source"

    pure PartialTransaction
        { id
        , inputs
        , outputs
        , datums
        , spendRedeemers
        , scripts
        , metadata
        }

decodeAddress
    :: Text
    -> Json.Parser Address
decodeAddress txt =
    case patternFromText txt of
        Just (MatchExact addr) ->
            pure addr
        Just p ->
            fail ("expected an address but got partial pattern: " <> show p)
        Nothing ->
            fail ("not a valid address: " <> toString txt)

decodeHash
    :: HashAlgorithm alg
    => Json.Value
    -> Json.Parser (Hash alg a)
decodeHash =
    Json.parseJSON >=> maybe (fail "invalid hash") pure . hashFromTextAsHex

decodeMetadataHash
    :: Json.Value
    -> Json.Parser MetadataHash
decodeMetadataHash =
    Json.parseJSON >=> maybe (fail "invalid metadata hash") pure . metadataHashFromText

decodeOneEraHash
    :: Json.Value
    -> Json.Parser (OneEraHash (CardanoEras StandardCrypto))
decodeOneEraHash =
    fmap (OneEraHash . toShort . hashToBytes) . decodeHash @Blake2b_256

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

decodeScripts
    :: Json.Value
    -> Json.Parser (Map ScriptHash Script)
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

decodeScript
    :: Json.Value
    -> Json.Parser Script
decodeScript = Json.withObject "Script" $ \o -> do
    o .: "language" >>= \case
        "native" -> decodeNative =<< o .: "json"
        "plutus:v1" -> decodePlutus "01" =<< o .: "cbor"
        "plutus:v2" -> decodePlutus "02" =<< o .: "cbor"
        "plutus:v3" -> decodePlutus "03" =<< o .: "cbor"
        (_ :: Text) -> fail "unrecognized script language"
  where
    decodeNative =
        fmap fromNativeScript . decodeNativeScript

    decodePlutus tag bytes = do
        case scriptFromBytes <$> decodeBase16 (encodeUtf8 @Text (tag <> bytes)) of
            Right (Just s) ->
                pure s
            Right Nothing ->
                fail "decodeScript: malformed script"
            Left e ->
                fail $ "decodeScript: not base16: " <> show e

decodeNativeScript
    :: Json.Object
    -> Json.Parser NativeScript
decodeNativeScript o = do
    o .: "clause" >>= \case
        "signature" -> do
            sig <- o .: "from"
            RequireSignature <$> fmap KeyHash (decodeHash sig)
        "all" -> do
            xs <- StrictSeq.fromList <$> (o .: "from")
            RequireAllOf <$> traverse decodeNativeScript xs
        "any" -> do
            xs <- StrictSeq.fromList <$> (o .: "from")
            RequireAnyOf <$> traverse decodeNativeScript xs
        "some" -> do
            xs <- StrictSeq.fromList <$> (o .: "from")
            n <- o .: "atLeast"
            RequireMOf n <$> traverse decodeNativeScript xs
        "before" -> do
            RequireTimeExpire . SlotNo <$> (o .: "slot")
        "after" -> do
            RequireTimeStart . SlotNo <$> (o .: "slot")
        (_ :: Text) ->
            fail "unrecognized native script clause"

decodeDatums
    :: Json.Value
    -> Json.Parser (Map DatumHash BinaryData)
decodeDatums = Json.withObject "Datums" $
    KeyMap.foldrWithKey
        (\k v accum -> Map.insert
            <$> decodeDatumHash k
            <*> decodeBinaryData v
            <*> accum
        )
        (pure mempty)

decodeSpendRedeemers
    :: [Json.Object]
    -> Json.Parser (Map InputIndex BinaryData)
decodeSpendRedeemers = foldr
    (\el step -> do
        redeemers <- step
        (purpose, index) <- el .: "validator" >>= decodeValidatorReference
        if purpose == Spend then do
            redeemer <- el .: "redeemer" >>= decodeBinaryData
            pure $ Map.insert index redeemer redeemers
        else
            pure redeemers
    )
    (pure mempty)

decodeValidatorReference
    :: Json.Value
    -> Json.Parser (ScriptPurpose, Word16)
decodeValidatorReference = Json.withObject "ValidatorReference" $ \o -> do
    purpose <- o .: "purpose" >>= decodeScriptPurpose
    index <- o .: "index"
    pure (purpose, index)

decodeScriptPurpose
    :: Text
    -> Json.Parser ScriptPurpose
decodeScriptPurpose = \case
    "mint" -> pure Mint
    "spend" -> pure Spend
    "publish" -> pure Publish
    "withdraw" -> pure Withdraw
    "vote" -> pure Vote
    "propose" -> pure Propose
    purpose -> fail $ "unknown script purpose: " ++ toString purpose

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

decodeInput
    :: Json.Value
    -> Json.Parser Input
decodeInput = Json.withObject "Input" $ \o ->
    mkOutputReference
        <$> (o .: "transaction" >>= (.: "id") >>= decodeTransactionId)
        <*> (o .: "index")

decodePointOrOrigin
    :: Json.Value
    -> Json.Parser Point
decodePointOrOrigin json =
    decodeOrigin json <|> decodePoint json
  where
    decodeOrigin = Json.withText "PointOrigin" $ \case
        txt | txt == "origin" -> pure GenesisPoint
        _notOrigin -> empty
    decodePoint = Json.withObject "PointOrOrigin" $ \o -> do
        slot <- o .: "slot"
        hash <- o .: "id" >>= decodeOneEraHash
        return $ BlockPoint (SlotNo slot) hash

decodeSlotNoOrOrigin
    :: Json.Value
    -> Json.Parser (WithOrigin SlotNo)
decodeSlotNoOrOrigin json =
    decodeOrigin json <|> decodeSlotNo json
  where
    decodeOrigin = Json.withText "SlotNoOrOrigin" $ \case
        txt | txt == "origin" -> pure Origin
        _notOrigin -> empty
    decodeSlotNo = Json.withObject "SlotNoOrOrigin" $ \o -> do
        At . SlotNo <$> (o .: "slot")

decodeTipOrOrigin
    :: Json.Value
    -> Json.Parser Tip
decodeTipOrOrigin json =
    decodeOrigin json <|> decodeTip json
  where
    decodeOrigin = Json.withText "TipOrOrigin" $ \case
        txt | txt == "origin" -> pure GenesisTip
        _notOrigin-> empty
    decodeTip = Json.withObject "TipOrOrigin" $ \o -> do
        slot <- o .: "slot"
        hash <- o .: "id" >>= decodeOneEraHash
        height <- o .: "height"
        pure $ Tip (SlotNo slot) hash (BlockNo height)

decodeTransactionId
    :: Json.Value
    -> Json.Parser TransactionId
decodeTransactionId =
    fmap transactionIdFromHash . decodeHash @Blake2b_256

decodeValue
    :: Json.Value
    -> Json.Parser Value
decodeValue = Json.withObject "Value" $ \o -> do
    coins <- o .: "ada" >>= (.: "lovelace")
    assets <- KeyMap.foldrWithKey
        (\k v accum ->
            if k == "ada" then accum else do
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

decodeMetadata :: Json.Value -> Json.Parser Metadata
decodeMetadata = fmap mkMetadata . Json.withObject "Metadata"
    (KeyMap.foldrWithKey
        (\k v accum -> Map.insert
            <$> metadatumLabelFromJson k
            <*> metadatumFromJson v
            <*> accum
        )
        (pure Map.empty)
    )
  where
    metadatumLabelFromJson :: Json.Key -> Json.Parser Word64
    metadatumLabelFromJson key = do
        (lbl, remLbl) <- either fail pure (T.decimal $ Key.toText key)
        guard (T.null remLbl)
        pure lbl

    metadatumFromJson :: Json.Value -> Json.Parser Metadatum
    metadatumFromJson = Json.withObject "Metadatum" $ \o -> do
        o .:? "cbor" >>= \case
            Just cbor -> do
                bytes <- either (fail . show) pure $ decodeBase16 (encodeUtf8 @Text cbor)
                either (fail . show) pure $ decodeCbor @ShelleyEra "Metadatum" decCBOR (toLazy bytes)
            Nothing -> do
                valueToMetadatum <$> o .: "json"

    valueToMetadatum :: Json.Value -> Metadatum
    valueToMetadatum = \case
        Json.Object m ->
            Ledger.Map $ KeyMap.foldrWithKey
                (\k v ms -> (Ledger.S (Key.toText k), valueToMetadatum v) : ms)
                []
                m
        Json.Array xs ->
            Ledger.List $ foldr (\v ms -> valueToMetadatum v : ms) [] xs
        Json.Number i ->
            Ledger.I (round i)
        Json.String s ->
            Ledger.S (toText s)
        _ -> -- Bool / Null
            error "impossible: unexpected bool or null JSON in metadatum?"
