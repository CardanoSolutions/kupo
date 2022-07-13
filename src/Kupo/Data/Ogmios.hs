--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}

module Kupo.Data.Ogmios
    ( PartialBlock (..)
    , PartialTransaction (..)

    , RequestNextResponse (..)

    , encodeFindIntersect
    , encodeRequestNext

    , decodeFindIntersectResponse
    , decodeRequestNextResponse
    ) where

import Kupo.Prelude

import Cardano.Crypto.Hash.Class
    ( Hash, HashAlgorithm, hashFromTextAsHex, hashToBytes )
import Data.Aeson
    ( (.!=), (.:), (.:?) )
import Kupo.Data.Cardano
    ( Address
    , BinaryData
    , Blake2b_256
    , Block
    , BlockNo (..)
    , pattern BlockPoint
    , DatumHash
    , pattern GenesisPoint
    , Input
    , IsBlock (..)
    , Output
    , OutputReference
    , Point (..)
    , SlotNo (..)
    , StandardCrypto
    , Tip (..)
    , TransactionId
    , Value
    , WithOrigin (..)
    , binaryDataFromBytes
    , datumHashFromBytes
    , fromBinaryData
    , fromDatumHash
    , headerHashToJson
    , mkOutput
    , mkOutputReference
    , noDatum
    , slotNoToJson
    , transactionIdFromHash
    , transactionIdFromHash
    , unsafeMakeSafeHash
    , unsafeValueFromList
    , withReferences
    )
import Kupo.Data.ChainSync
    ( IntersectionNotFoundException (..) )
import Kupo.Data.Pattern
    ( Pattern (..), patternFromText )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoEras )
import Ouroboros.Consensus.HardFork.Combinator
    ( OneEraHash (..) )
import Ouroboros.Network.Block
    ( pointSlot )

import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Json
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

-- PartialBlock / PartialTransaction

data PartialBlock = PartialBlock
    (Point Block)
    [ PartialTransaction ]

data PartialTransaction = PartialTransaction
    { inputs :: [ Input ]
    , outputs :: [ (OutputReference, Output) ]
    , datums :: Map DatumHash BinaryData
    }

instance IsBlock PartialBlock where
    type BlockBody PartialBlock = PartialTransaction

    getPoint (PartialBlock pt _) =
        pt

    spentInputs PartialTransaction{inputs} =
        Set.fromList inputs

    foldBlock fn result (PartialBlock _ txs) =
        foldr fn result txs

    mapMaybeOutputs fn (PartialTransaction{outputs}) =
        mapMaybe (uncurry fn) outputs

    witnessedDatums PartialTransaction{datums} =
        datums

-- RequestNextResponse

data RequestNextResponse
    = RollBackward (Tip Block) (Point Block)
    | RollForward  (Tip Block) PartialBlock


-- Encoders

beginWspRequest :: Json.Series
beginWspRequest = mconcat
    [ Json.pair "type" (Json.text "jsonwsp/request")
    , Json.pair "version" (Json.text "1.0")
    , Json.pair "servicename" (Json.text "ogmios")
    ]

encodeFindIntersect :: [Point Block] -> Json.Encoding
encodeFindIntersect pts = Json.pairs $ beginWspRequest
    <> Json.pair "methodname" (Json.text "FindIntersect")
    <> Json.pair "args" (Json.pairs $ Json.pair "points" (Json.list encodePoint pts))

encodeRequestNext :: Json.Encoding
encodeRequestNext = Json.pairs $ beginWspRequest
    <> Json.pair "methodname" (Json.text "RequestNext")

encodePoint :: Point Block -> Json.Encoding
encodePoint = \case
    GenesisPoint ->
        Json.text "origin"
    BlockPoint slotNo headerHash ->
        Json.pairs $ mconcat
            [ Json.pair "slot" (slotNoToJson slotNo)
            , Json.pair "hash" (headerHashToJson headerHash)
            ]

-- Decoders

decodeFindIntersectResponse
    :: [Point Block]
    -> Json.Value
    -> Json.Parser (Either IntersectionNotFoundException ())
decodeFindIntersectResponse (fmap pointSlot -> requestedPoints) json =
    decodeIntersectionFound json <|> decodeIntersectionNotFound json
  where
    decodeIntersectionFound = Json.withObject "FindIntersectResponse" $ \o -> do
        (_ :: Json.Value) <- o .: "result" >>= (.: "IntersectionFound")
        return (Right ())

    decodeIntersectionNotFound = Json.withObject "FindIntersectResponse" $ \o -> do
        tip <- o .: "result" >>= (.: "IntersectionNotFound") >>= (.: "tip") >>= decodeSlotNoOrOrigin
        return (Left IntersectionNotFound{requestedPoints, tip})

decodeRequestNextResponse
    :: Json.Value
    -> Json.Parser RequestNextResponse
decodeRequestNextResponse json =
    decodeRollForward json <|> decodeRollBackward json
  where
    decodeRollForward =  Json.withObject "RequestNextResponse" $ \o -> do
        result <- o .: "result" >>= (.: "RollForward")
        block  <- result .: "block"
        tip    <- result .: "tip" >>= decodeTipOrOrigin
        RollForward tip <$> asum
            [ decodeByronBlock block
            , decodeShelleyBlock block
            , decodeAllegraBlock block
            , decodeMaryBlock block
            , decodeAlonzoBlock block
            , decodeBabbageBlock block
            ]

    decodeRollBackward = Json.withObject "RequestNextResponse" $ \o -> do
        result <- o .: "result" >>= (.: "RollBackward")
        point  <- result .: "point" >>= decodePointOrOrigin
        tip    <- result .: "tip"   >>= decodeTipOrOrigin
        return (RollBackward tip point)

decodeByronBlock
    :: Json.Value
    -> Json.Parser PartialBlock
decodeByronBlock = Json.withObject "Block[Byron]" $ \o -> do
    block <- o .: "byron"
    decodeStandardBlock block <|> decodeEpochBoundaryBlock block
  where
    decodeStandardBlock = Json.withObject "Block[Byron~Standard]" $ \o -> do
        txs <- o .: "body" >>= (.: "txPayload")
        slot <- o .: "header" >>= (.: "slot")
        headerHash <- o .: "hash" >>= decodeOneEraHash
        let point = BlockPoint (SlotNo slot) headerHash
        PartialBlock point <$> traverse decodePartialTransaction txs

    decodeEpochBoundaryBlock = Json.withObject "Block[Byron~Standard]" $ \o -> do
        slot <- o .: "header" >>= (.: "blockHeight")
        headerHash <- o .: "hash" >>= decodeOneEraHash
        let point = BlockPoint (SlotNo slot) headerHash
        pure (PartialBlock point [])

decodeShelleyBlock
    :: Json.Value
    -> Json.Parser PartialBlock
decodeShelleyBlock = Json.withObject "ShelleyBlock" $ \o -> do
    shelley <- o .: "shelley"
    txs <- shelley .: "body"
    point <- decodeBlockPoint shelley
    PartialBlock point <$> traverse decodePartialTransaction txs

decodeAllegraBlock
    :: Json.Value
    -> Json.Parser PartialBlock
decodeAllegraBlock = Json.withObject "AllegraBlock" $ \o -> do
    allegra <- o .: "allegra"
    txs <- allegra .: "body"
    point <- decodeBlockPoint allegra
    PartialBlock point <$> traverse decodePartialTransaction txs

decodeMaryBlock
    :: Json.Value
    -> Json.Parser PartialBlock
decodeMaryBlock = Json.withObject "MaryBlock" $ \o -> do
    mary <- o .: "mary"
    txs <- mary .: "body"
    point <- decodeBlockPoint mary
    PartialBlock point <$> traverse decodePartialTransaction txs

decodeAlonzoBlock
    :: Json.Value
    -> Json.Parser PartialBlock
decodeAlonzoBlock = Json.withObject "AlonzoBlock" $ \o -> do
    alonzo <- o .: "alonzo"
    txs <- alonzo .: "body"
    point <- decodeBlockPoint alonzo
    PartialBlock point <$> traverse decodePartialTransaction txs

decodeBabbageBlock
    :: Json.Value
    -> Json.Parser PartialBlock
decodeBabbageBlock = Json.withObject "babbageBlock" $ \o -> do
    babbage <- o .: "babbage"
    txs <- babbage .: "body"
    point <- decodeBlockPoint babbage
    PartialBlock point <$> traverse decodePartialTransaction txs


decodeAddress
    :: Text
    -> Json.Parser Address
decodeAddress txt =
    case patternFromText txt of
        Just (MatchExact addr) ->
            pure addr
        _ ->
            empty

decodeBlockPoint
    :: Json.Object
    -> Json.Parser (Point Block)
decodeBlockPoint o = do
    headerHash <- o .: "headerHash" >>= decodeOneEraHash
    slot <- o .: "header" >>= (.: "slot")
    pure (BlockPoint (SlotNo slot) headerHash)

decodeHash
    :: HashAlgorithm alg
    => Json.Value
    -> Json.Parser (Hash alg a)
decodeHash =
    Json.parseJSON >=> maybe empty pure . hashFromTextAsHex

decodeOneEraHash
    :: Json.Value
    -> Json.Parser (OneEraHash (CardanoEras StandardCrypto))
decodeOneEraHash =
    fmap (OneEraHash . toShort . hashToBytes) . decodeHash @Blake2b_256

decodeOutput
    :: Json.Value
    -> Json.Parser Output
decodeOutput = Json.withObject "Output" $ \o -> do
    address <- o .: "address" >>= decodeAddress
    value <- o .: "value" >>= decodeValue
    datumHash <- o .:? "datumHash" >>= traverse (fmap unsafeMakeSafeHash . decodeHash @Blake2b_256)
    datum <- o .:? "datum" >>= traverse decodeBinaryData
    case (datumHash, datum) of
        (Just x, _) ->
            pure $ mkOutput address value (fromDatumHash x)
        (Nothing, Just x) ->
            pure $ mkOutput address value (fromBinaryData x)
        (Nothing, Nothing) ->
            pure $ mkOutput address value noDatum

decodePartialTransaction
    :: Json.Value
    -> Json.Parser PartialTransaction
decodePartialTransaction = Json.withObject "PartialTransaction" $ \o -> do
    txId <- o .: "id" >>= decodeTransactionId
    inputSource <- o .:? "inputSource"
    witness <- o .: "witness"
    datums <- witness .:? "datums" .!= Json.Object mempty >>= decodeDatums
    case inputSource of
        Just ("collaterals" :: Text) -> do
            inputs <- traverse decodeInput =<< (o .: "body" >>= (.: "collaterals"))
            pure PartialTransaction
                { inputs
                , outputs = []
                , datums
                }

        _ -> do
            inputs <- traverse decodeInput =<< (o .: "body" >>= (.: "inputs"))
            outs <- traverse decodeOutput =<< (o .: "body" >>= (.: "outputs"))
            pure PartialTransaction
                { inputs
                , outputs = withReferences txId outs
                , datums
                }

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
        <$> (decodeTransactionId =<< (o .: "txId"))
        <*> (o .: "index")

decodePointOrOrigin
    :: Json.Value
    -> Json.Parser (Point Block)
decodePointOrOrigin json =
    decodeOrigin json <|> decodePoint json
  where
    decodeOrigin = Json.withText "PointOrigin" $ \case
        txt | txt == "origin" -> pure GenesisPoint
        _ -> empty
    decodePoint = Json.withObject "PointOrOrigin" $ \o -> do
        slot <- o .: "slot"
        hash <- o .: "hash" >>= decodeOneEraHash
        return $ BlockPoint (SlotNo slot) hash

decodeSlotNoOrOrigin
    :: Json.Value
    -> Json.Parser (WithOrigin SlotNo)
decodeSlotNoOrOrigin json =
    decodeOrigin json <|> decodeSlotNo json
  where
    decodeOrigin = Json.withText "SlotNoOrOrigin" $ \case
        txt | txt == "origin" -> pure Origin
        _ -> empty
    decodeSlotNo = Json.withObject "SlotNoOrOrigin" $ \o -> do
        At . SlotNo <$> (o .: "slot")

decodeTipOrOrigin
    :: Json.Value
    -> Json.Parser (Tip Block)
decodeTipOrOrigin json =
    decodeOrigin json <|> decodeTip json
  where
    decodeOrigin = Json.withText "TipOrOrigin" $ \case
        txt | txt == "origin" -> pure TipGenesis
        _ -> empty
    decodeTip = Json.withObject "TipOrOrigin" $ \o -> do
        slot <- o .: "slot"
        hash <- o .: "hash" >>= decodeOneEraHash
        blockNo <- o .: "blockNo"
        pure $ Tip (SlotNo slot) hash (BlockNo blockNo)

decodeTransactionId
    :: Json.Value
    -> Json.Parser TransactionId
decodeTransactionId =
    fmap transactionIdFromHash . decodeHash @Blake2b_256

decodeValue
    :: Json.Value
    -> Json.Parser Value
decodeValue = Json.withObject "Value" $ \o -> do
    coins <- o .: "coins"
    assets <- o .:? "assets" .!= mempty >>= traverse decodeAsset . Map.toList
    pure (unsafeValueFromList coins assets)
  where
    decodeBase16' = decodeBase16 . encodeUtf8

    decodeAsset
        :: (Text, Integer)
        -> Json.Parser (ByteString, ByteString, Integer)
    decodeAsset (assetId, quantity) =
        case Text.splitOn "." assetId of
            [ decodeBase16' -> Right policyId, decodeBase16' -> Right assetName ] -> do
                pure (policyId, assetName, quantity)
            [ decodeBase16' -> Right policyId ] -> do
                pure (policyId, mempty, quantity)
            _ ->
                empty
