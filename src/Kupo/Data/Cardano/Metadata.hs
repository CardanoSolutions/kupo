module Kupo.Data.Cardano.Metadata
    ( Metadatum(..)
    , module Kupo.Data.Cardano.Metadata
    ) where

import Kupo.Prelude

import Cardano.Binary
    ( FromCBOR (..)
    , decodeAnnotator
    )
import Cardano.Ledger.Shelley.Metadata
    ( Metadatum (..)
    )
import Data.Aeson
    ( (.:)
    )
import Kupo.Data.Cardano.MetadataHash
    ( MetadataHash
    , metadataHashToJson
    )
import Ouroboros.Consensus.Util
    ( eitherToMaybe
    )

import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.Shelley.Metadata as Ledger
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as Ledger.MaryAllegra
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Key as Json.Key
import qualified Data.Aeson.KeyMap as Json.KeyMap
import qualified Data.Aeson.Types as Json
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Read as T

type Metadata =
    Ledger.Metadata (BabbageEra StandardCrypto)

emptyMetadata :: Metadata
emptyMetadata =
    Ledger.Metadata mempty
{-# INLINABLE emptyMetadata #-}

mkMetadata :: Map Word64 Metadatum -> (MetadataHash, Metadata)
mkMetadata (Ledger.Metadata -> meta) =
    (hashMetadata meta, meta)

hashMetadata :: Metadata -> MetadataHash
hashMetadata = Ledger.hashMetadata
{-# INLINABLE hashMetadata #-}

hasMetadataTag :: Word64 -> Metadata -> Bool
hasMetadataTag tag (Ledger.Metadata metadata) =
    tag `Map.member` metadata

metadataToText :: Metadata -> Text
metadataToText =
    encodeBase16 . Ledger.originalBytes
{-# INLINABLE metadataToText #-}

metadataFromText :: Text -> Maybe Metadata
metadataFromText txt = do
    bytes <- eitherToMaybe $ decodeBase16 (encodeUtf8 txt)
    eitherToMaybe $ decodeAnnotator "Metadata" fromCBOR (toLazy bytes)

metadataToJson :: Metadata -> Json.Encoding
metadataToJson (Ledger.Metadata meta) =
    encodeMap show encodeMetadatum meta
  where
    encodeMetadatum :: Ledger.Metadatum -> Json.Encoding
    encodeMetadatum = \case
        Ledger.I n ->
            encodeObject [("int", Json.integer n)]
        Ledger.S txt ->
            encodeObject [("string", Json.text txt)]
        Ledger.B bytes ->
            encodeObject [("bytes", Json.text $ encodeBase16 bytes)]
        Ledger.List xs ->
            encodeObject [("list", Json.list encodeMetadatum xs)]
        Ledger.Map xs ->
            encodeObject [("map", Json.list encodeKeyPair xs)]

    encodeKeyPair :: (Ledger.Metadatum, Ledger.Metadatum) -> Json.Encoding
    encodeKeyPair (k, v) =
        encodeObject
            [ ( "k", encodeMetadatum k )
            , ( "v", encodeMetadatum v )
            ]

metadataFromJson :: Json.Value -> Json.Parser (MetadataHash, Metadata)
metadataFromJson = fmap mkMetadata . Json.withObject "Metadata"
    (Json.KeyMap.foldrWithKey
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
        (lbl, remLbl) <- either fail pure (T.decimal $ Json.Key.toText key)
        guard (T.null remLbl)
        pure lbl

    metadatumFromJson :: Json.Value -> Json.Parser Metadatum
    metadatumFromJson = Json.withObject "Metadatum" $ \o -> asum
        [ metadatumIntFromJson o
        , metadatumStringFromJson o
        , metadatumBytesFromJson o
        , metadatumListFromJson o
        , metadatumMapFromJson o
        ]
      where
        metadatumIntFromJson :: Json.Object -> Json.Parser Metadatum
        metadatumIntFromJson o = do
            i <- o .: "int"
            pure (Ledger.I i)

        metadatumStringFromJson :: Json.Object -> Json.Parser Metadatum
        metadatumStringFromJson o = do
            s <- o .: "string"
            pure (Ledger.S s)

        metadatumBytesFromJson :: Json.Object -> Json.Parser Metadatum
        metadatumBytesFromJson o = do
            b <- (o .: "bytes")
              >>= either (fail . toString) pure . decodeBase16 . encodeUtf8 @Text
            pure (Ledger.B b)

        metadatumListFromJson :: Json.Object -> Json.Parser Metadatum
        metadatumListFromJson o = do
            xs <- (o .: "list") >>= traverse metadatumFromJson
            pure (Ledger.List xs)

        metadatumMapFromJson :: Json.Object -> Json.Parser Metadatum
        metadatumMapFromJson o = do
            xs <- (o .: "map") >>= traverse
                (Json.withObject "map" $ \m -> do
                    k <- m .: "k"
                    v <- m .: "v"
                    (,) <$> metadatumFromJson k <*> metadatumFromJson v
                )
            pure (Ledger.Map xs)

metadataToJson' :: (MetadataHash, Metadata) -> Json.Encoding
metadataToJson' (hash, meta) =
    encodeObject
        [ ("hash", metadataHashToJson hash)
        , ("raw", Json.text (metadataToText meta))
        , ("schema", metadataToJson meta)
        ]

fromShelleyMetadata :: Ledger.Metadata (ShelleyEra StandardCrypto) -> Metadata
fromShelleyMetadata =
    coerce
{-# INLINABLE fromShelleyMetadata #-}

fromAllegraMetadata :: Ledger.MaryAllegra.AuxiliaryData (AllegraEra StandardCrypto) -> Metadata
fromAllegraMetadata (Ledger.MaryAllegra.AuxiliaryData metadata _) =
    Ledger.Metadata metadata
{-# INLINABLE fromAllegraMetadata #-}

fromMaryMetadata :: Ledger.MaryAllegra.AuxiliaryData (MaryEra StandardCrypto) -> Metadata
fromMaryMetadata (Ledger.MaryAllegra.AuxiliaryData metadata _) =
    Ledger.Metadata metadata
{-# INLINABLE fromMaryMetadata #-}

fromAlonzoMetadata :: Ledger.AuxiliaryData (AlonzoEra StandardCrypto) -> Metadata
fromAlonzoMetadata =
    Ledger.Metadata . Ledger.txMD
{-# INLINABLE fromAlonzoMetadata #-}

fromBabbageMetadata :: Ledger.AuxiliaryData (BabbageEra StandardCrypto) -> Metadata
fromBabbageMetadata =
    Ledger.Metadata . Ledger.txMD
{-# INLINABLE fromBabbageMetadata #-}
