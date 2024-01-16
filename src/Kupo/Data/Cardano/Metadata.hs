module Kupo.Data.Cardano.Metadata
    ( Metadatum(..)
    , module Kupo.Data.Cardano.Metadata
    ) where

import Kupo.Prelude

import Cardano.Ledger.Allegra.TxAuxData
    ( AllegraTxAuxData (..)
    )
import Cardano.Ledger.Alonzo.TxAuxData
    ( AlonzoTxAuxData (..)
    )
import Cardano.Ledger.Shelley.TxAuxData
    ( Metadatum (..)
    , ShelleyTxAuxData (..)
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

import qualified Cardano.Ledger.Allegra.Scripts as Ledger.Allegra
import qualified Cardano.Ledger.Alonzo.TxAuxData as Ledger.Alonzo
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.Shelley.TxAuxData as Ledger
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Key as Json.Key
import qualified Data.Aeson.KeyMap as Json.KeyMap
import qualified Data.Aeson.Types as Json
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Read as T

type Metadata =
    AlonzoTxAuxData (ConwayEra StandardCrypto)

emptyMetadata :: Metadata
emptyMetadata =
    AlonzoTxAuxData mempty mempty mempty
{-# INLINABLE emptyMetadata #-}

mkMetadata :: Map Word64 Metadatum -> Metadata
mkMetadata labels =
    AlonzoTxAuxData labels mempty mempty
{-# INLINABLE mkMetadata #-}

hashMetadata :: Metadata -> MetadataHash
hashMetadata = Ledger.Alonzo.unsafeAuxiliaryDataHash . Ledger.hashTxAuxData
{-# INLINABLE hashMetadata #-}

hasMetadataTag :: Word64 -> Metadata -> Bool
hasMetadataTag tag (AlonzoTxAuxData labels _ _) =
    tag `Map.member` labels

metadataToText :: Metadata -> Text
metadataToText =
    encodeBase16 . Ledger.originalBytes
{-# INLINABLE metadataToText #-}

metadataFromText :: Text -> Maybe Metadata
metadataFromText txt = do
    bytes <- eitherToMaybe $ decodeBase16 (encodeUtf8 txt)
    eitherToMaybe $ decodeCborAnn @BabbageEra "Metadata" decCBOR (toLazy bytes)

metadataToJson :: Metadata -> Json.Encoding
metadataToJson (AlonzoTxAuxData labels _ _) =
    encodeMap show encodeMetadatum labels
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

metadataFromJson :: Json.Value -> Json.Parser Metadata
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

fromShelleyMetadata :: ShelleyTxAuxData (ShelleyEra StandardCrypto) -> Metadata
fromShelleyMetadata (ShelleyTxAuxData labels) =
    AlonzoTxAuxData labels mempty mempty
{-# INLINABLE fromShelleyMetadata #-}

fromAllegraMetadata :: AllegraTxAuxData (AllegraEra StandardCrypto) -> Metadata
fromAllegraMetadata (AllegraTxAuxData labels timelocks) =
    AlonzoTxAuxData labels (Ledger.Allegra.translateTimelock <$> timelocks) mempty
{-# INLINABLE fromAllegraMetadata #-}

fromMaryMetadata :: AllegraTxAuxData (MaryEra StandardCrypto) -> Metadata
fromMaryMetadata (AllegraTxAuxData labels timelocks) =
    AlonzoTxAuxData labels (Ledger.Allegra.translateTimelock <$> timelocks) mempty
{-# INLINABLE fromMaryMetadata #-}

fromAlonzoMetadata :: AlonzoTxAuxData (AlonzoEra StandardCrypto) -> Metadata
fromAlonzoMetadata =
    Ledger.Alonzo.translateAlonzoTxAuxData
{-# INLINABLE fromAlonzoMetadata #-}

fromBabbageMetadata :: AlonzoTxAuxData (BabbageEra StandardCrypto) -> Metadata
fromBabbageMetadata =
    Ledger.upgradeTxAuxData
{-# INLINABLE fromBabbageMetadata #-}

fromConwayMetadata :: AlonzoTxAuxData (ConwayEra StandardCrypto) -> Metadata
fromConwayMetadata =
    identity
{-# INLINABLE fromConwayMetadata #-}
