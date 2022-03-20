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
import Kupo.Control.MonadOuroboros
    ( IntersectionNotFoundException (..) )
import Kupo.Data.Cardano
    ( Address
    , Blake2b_256
    , Block
    , BlockNo (..)
    , pattern BlockPoint
    , pattern GenesisPoint
    , Output
    , OutputReference
    , Point (..)
    , SlotNo (..)
    , StandardCrypto
    , Tip (..)
    , TransactionId
    , Value
    , WithOrigin (..)
    , mkOutput
    , pointToJson
    , transactionIdFromHash
    , transactionIdFromHash
    , unsafeMakeSafeHash
    , unsafeValueFromList
    , withReferences
    )
import Kupo.Data.Pattern
    ( Pattern (..), patternFromText )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoEras )
import Ouroboros.Consensus.HardFork.Combinator
    ( OneEraHash (..) )
import Ouroboros.Network.Block
    ( pointSlot )

import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Map as Map
import qualified Data.Text as Text

-- PartialBlock / PartialTransaction

newtype PartialBlock = PartialBlock
    [ PartialTransaction ]

newtype PartialTransaction = PartialTransaction
    [ (OutputReference, Output) ]

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
    <> Json.pair "args" (Json.pairs $ Json.pair "points" (Json.list pointToJson pts))

encodeRequestNext :: Json.Encoding
encodeRequestNext = Json.pairs $ beginWspRequest
    <> Json.pair "methodname" (Json.text "RequestNext")

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
        return (Left IntersectionNotFoundException{requestedPoints, tip})

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
    decodeStandardBlock block <|> decodeEpochBoundaryBlock
  where
    decodeStandardBlock = Json.withObject "Block[Byron~Standard]" $ \o -> do
        txs <- o .: "body" >>= (.: "txPayload")
        PartialBlock <$> traverse decodePartialTransaction txs

    decodeEpochBoundaryBlock =
        pure (PartialBlock [])

decodeShelleyBlock
    :: Json.Value
    -> Json.Parser PartialBlock
decodeShelleyBlock = Json.withObject "ShelleyBlock" $ \o -> do
    txs <- o .: "shelley" >>= (.: "body")
    PartialBlock <$> traverse decodePartialTransaction txs

decodeAllegraBlock
    :: Json.Value
    -> Json.Parser PartialBlock
decodeAllegraBlock = Json.withObject "AllegraBlock" $ \o -> do
    txs <- o .: "allegra" >>= (.: "body")
    PartialBlock <$> traverse decodePartialTransaction txs

decodeMaryBlock
    :: Json.Value
    -> Json.Parser PartialBlock
decodeMaryBlock = Json.withObject "MaryBlock" $ \o -> do
    txs <- o .: "mary" >>= (.: "body")
    PartialBlock <$> traverse decodePartialTransaction txs

decodeAlonzoBlock
    :: Json.Value
    -> Json.Parser PartialBlock
decodeAlonzoBlock = Json.withObject "AlonzoBlock" $ \o -> do
    txs <- o .: "alonzo" >>= (.: "body")
    PartialBlock <$> traverse decodePartialTransaction txs

decodeAddress
    :: Text
    -> Json.Parser Address
decodeAddress txt =
    case patternFromText txt of
        Just (MatchExact addr) ->
            pure addr
        _ ->
            empty

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
    datumHash <- o .:? "datum" >>= traverse (fmap unsafeMakeSafeHash . decodeHash @Blake2b_256)
    pure (mkOutput address value datumHash)

decodePartialTransaction
    :: Json.Value
    -> Json.Parser PartialTransaction
decodePartialTransaction = Json.withObject "PartialTransaction" $ \o -> do
    txId <- o .: "id" >>= decodeTransactionId
    outs <- traverse decodeOutput =<< (o .: "body" >>= (.: "outputs"))
    pure (PartialTransaction (withReferences txId outs))

decodePointOrOrigin
    :: Json.Value
    -> Json.Parser (Point Block)
decodePointOrOrigin json =
    decodeOrigin json <|> decodePoint json
  where
    decodeOrigin = Json.withText "PointOrigin" $ \case
        txt | txt == "origin" -> pure GenesisPoint
        _ -> empty
    decodePoint = Json.withObject "PointOrOrigin" $ \obj -> do
        slot <- obj .: "slot"
        hash <- obj .: "hash" >>= decodeOneEraHash
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
    decodeSlotNo = Json.withObject "SlotNoOrOrigin" $ \obj -> do
        At . SlotNo <$> (obj .: "slot")

decodeTipOrOrigin
    :: Json.Value
    -> Json.Parser (Tip Block)
decodeTipOrOrigin json =
    decodeOrigin json <|> decodeTip json
  where
    decodeOrigin = Json.withText "TipOrOrigin" $ \case
        txt | txt == "origin" -> pure TipGenesis
        _ -> empty
    decodeTip = Json.withObject "TipOrOrigin" $ \obj -> do
        slot <- obj .: "slot"
        hash <- obj .: "hash" >>= decodeOneEraHash
        blockNo <- obj .: "blockNo"
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

