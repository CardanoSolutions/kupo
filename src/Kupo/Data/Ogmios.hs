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
    ( (.:) )
import Kupo.Control.MonadOuroboros
    ( IntersectionNotFoundException (..) )
import Kupo.Data.Cardano
    ( Blake2b_256
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
    , WithOrigin (..)
    , pointToJson
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoEras )
import Ouroboros.Consensus.HardFork.Combinator
    ( OneEraHash (..) )
import Ouroboros.Network.Block
    ( pointSlot )

import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Types as Json

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
        (_ :: Json.Value) <- o .: "IntersectionFound"
        return (Right ())

    decodeIntersectionNotFound = Json.withObject "FindIntersectResponse" $ \o -> do
        tip <- At <$> (o .: "IntersectionNotFound" >>= (.: "tip") >>= (.: "slot"))
        return (Left IntersectionNotFoundException{requestedPoints, tip})

decodeRequestNextResponse
    :: Json.Value
    -> Json.Parser RequestNextResponse
decodeRequestNextResponse json =
    decodeRollForward json <|> decodeRollBackward json
  where
    decodeRollForward = undefined

    decodeRollBackward = Json.withObject "RequestNextResponse" $ \o -> do
        result <- o .: "RollBackward" >>= (.: "result")
        point  <- result .: "point" >>= decodePointOrOrigin
        tip    <- result .: "tip"   >>= decodeTipOrOrigin
        return (RollBackward tip point)

decodePointOrOrigin
    :: Json.Value
    -> Json.Parser (Point Block)
decodePointOrOrigin json =
    decodeOrigin json <|> decodePoint json
  where
    decodeOrigin = Json.withText "Point" $ \case
        txt | txt == "origin" -> pure GenesisPoint
        _ -> empty

    decodePoint = Json.withObject "Point" $ \obj -> do
        slot <- obj .: "slot"
        hash <- obj .: "hash" >>= decodeOneEraHash
        return $ BlockPoint (SlotNo slot) hash

decodeTipOrOrigin
    :: Json.Value
    -> Json.Parser (Tip Block)
decodeTipOrOrigin json =
    decodeOrigin json <|> decodeTip json
  where
    decodeOrigin = Json.withText "Tip" $ \case
        txt | txt == "origin" -> pure TipGenesis
        _ -> empty

    decodeTip = Json.withObject "Tip" $ \obj -> do
        slot <- obj .: "slot"
        hash <- obj .: "hash" >>= decodeOneEraHash
        blockNo <- obj .: "blockNo"
        pure $ Tip (SlotNo slot) hash (BlockNo blockNo)

decodeOneEraHash
    :: Json.Value
    -> Json.Parser (OneEraHash (CardanoEras StandardCrypto))
decodeOneEraHash =
    fmap (OneEraHash . toShort . hashToBytes) . decodeHash @Blake2b_256

decodeHash
    :: HashAlgorithm alg
    => Json.Value
    -> Json.Parser (Hash alg a)
decodeHash =
    Json.parseJSON >=> maybe empty pure . hashFromTextAsHex
