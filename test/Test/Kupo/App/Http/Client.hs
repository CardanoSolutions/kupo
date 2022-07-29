--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}

module Test.Kupo.App.Http.Client where

import Kupo.Prelude

import Cardano.Crypto.Hash.Class
    ( Hash, HashAlgorithm, hashFromTextAsHex )
import Data.Aeson
    ( (.!=), (.:), (.:?) )
import Data.Aeson.Lens
    ( key, _String )
import Data.List
    ( maximum )
import Kupo.Control.MonadCatch
    ( MonadCatch (..) )
import Kupo.Control.MonadDelay
    ( MonadDelay (..) )
import Kupo.Data.Cardano
    ( Address
    , BinaryData
    , Blake2b_256
    , Datum
    , DatumHash
    , pattern GenesisPoint
    , OutputReference
    , Point
    , Script
    , ScriptHash
    , ScriptReference (..)
    , SlotNo (..)
    , TransactionId
    , Value
    , binaryDataFromBytes
    , datumHashFromBytes
    , datumHashToText
    , fromDatumHash
    , getPointSlotNo
    , mkOutputReference
    , noDatum
    , pointFromText
    , scriptFromBytes
    , scriptHashFromBytes
    , scriptHashToText
    , transactionIdFromHash
    , unsafeValueFromList
    )
import Kupo.Data.Http.GetCheckpointMode
    ( GetCheckpointMode (..) )
import Kupo.Data.Http.StatusFlag
    ( StatusFlag (..) )
import Kupo.Data.Pattern
    ( Pattern (..), Result (..), patternFromText )
import Network.HTTP.Client
    ( HttpException
    , Manager
    , Response (..)
    , defaultManagerSettings
    , httpLbs
    , httpNoBody
    , newManager
    , parseRequest
    )

import Control.Arrow
    ( left )
import qualified Data.Aeson as Json
import qualified Data.Aeson.Internal as Json
import qualified Data.Aeson.KeyMap as Json
import qualified Data.Aeson.Parser as Json
import qualified Data.Aeson.Parser.Internal as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Map as Map
import qualified Data.Text as T

data HttpClient (m :: Type -> Type) = HttpClient
    { waitUntilM
        :: (m Bool) -> m ()
    , waitSlot
        :: (SlotNo -> Bool) -> m ()
    , lookupDatumByHash
        :: DatumHash -> m (Maybe BinaryData)
    , waitDatum
        :: DatumHash -> m BinaryData
    , lookupScriptByHash
        :: ScriptHash -> m (Maybe Script)
    , waitScript
        :: ScriptHash -> m Script
    , listCheckpoints
        :: m [Point]
    , getCheckpointBySlot
        :: GetCheckpointMode -> SlotNo -> m (Maybe Point)
    , getAllMatches
        :: StatusFlag
        -> m [Result]
    }

newHttpClient :: (String, Int) -> IO (HttpClient IO)
newHttpClient config = do
    manager <- newManager defaultManagerSettings
    pure (newHttpClientWith manager config)

newHttpClientWith :: Manager -> (String, Int) -> HttpClient IO
newHttpClientWith manager (serverHost, serverPort) =
    HttpClient
        { waitUntilM =
            \a0 -> waitForServer >> _waitUntilM a0
        , waitSlot =
            \a0 -> waitForServer >> _waitSlot a0
        , lookupDatumByHash =
            \a0 -> waitForServer >> _lookupDatumByHash a0
        , waitDatum =
            \a0 -> waitForServer >> _waitDatum a0
        , lookupScriptByHash =
            \a0 -> waitForServer >> _lookupScriptByHash a0
        , waitScript =
            \a0 -> waitForServer >> _waitScript a0
        , listCheckpoints =
            waitForServer >> _listCheckpoints
        , getCheckpointBySlot =
            \a0 a1 -> waitForServer >> _getCheckpointBySlot a0 a1
        , getAllMatches =
            \a0 -> waitForServer >> _getAllMatches a0
        }
  where
    baseUrl :: String
    baseUrl = "http://" <> serverHost <> ":" <> show serverPort

    waitForServer :: IO ()
    waitForServer = loop 0
      where
        loop (n :: Word)
            | n > 100 =
                fail "waitForServer: timeout."
            | otherwise = do
                req <- parseRequest (baseUrl <> "/v1/health")
                void (httpNoBody req manager) `catch` (\(_ :: HttpException) -> do
                    threadDelay 0.1
                    loop (succ n))

    _waitUntilM :: IO Bool -> IO ()
    _waitUntilM predicate = do
        predicate >>= \case
            True ->
                return ()
            False -> do
                threadDelay 0.25
                _waitUntilM predicate

    _waitSlot :: (SlotNo -> Bool) -> IO ()
    _waitSlot predicate = do
        slots <- fmap getPointSlotNo <$> _listCheckpoints
        unless (not (null slots) && predicate (maximum slots)) $ do
            threadDelay 0.25
            _waitSlot predicate

    _listCheckpoints :: IO [Point]
    _listCheckpoints = do
        req <- parseRequest (baseUrl <> "/v1/checkpoints")
        res <- httpLbs req manager
        let body = responseBody res
        case eitherDecode (Json.listParser decodePoint) body of
            Left e ->
                fail (show body <> " ----> " <> show e)
            Right xs ->
                pure xs

    _getCheckpointBySlot :: GetCheckpointMode -> SlotNo -> IO (Maybe Point)
    _getCheckpointBySlot mode (SlotNo slot) = do
        let qry = case mode of
                GetCheckpointStrict -> "?strict"
                GetCheckpointClosestAncestor -> ""
        req <- parseRequest (baseUrl <> "/v1/checkpoints/" <> show slot <> qry)
        res <- httpLbs req manager
        let body = responseBody res
        pure $ either (const Nothing) Just (eitherDecode decodePoint body)

    _getAllMatches :: StatusFlag -> IO [Result]
    _getAllMatches st = do
        let q = case st of
                 NoStatusFlag -> ""
                 OnlySpent -> "?spent"
                 OnlyUnspent -> "?unspent"
        req <- parseRequest (baseUrl <> "/v1/matches" <> q)
        res <- httpLbs req manager
        let body = responseBody res
        case eitherDecode (Json.listParser decodeResult) body of
            Left e ->
                fail (show e)
            Right xs ->
                pure xs

    _lookupDatumByHash :: DatumHash -> IO (Maybe BinaryData)
    _lookupDatumByHash datumHash = do
        let fragment = toString (datumHashToText datumHash)
        req <- parseRequest (baseUrl <> "/v1/datums/" <> fragment)
        res <- httpLbs req manager
        let body = responseBody res
        case Json.eitherDecode' body of
            Left e ->
                fail (show e)
            Right Json.Null -> do
                pure Nothing
            Right val -> maybe (fail "failed to decode Datum.") (pure . Just) $ do
                bytes <- val ^? key "datum" . _String
                binaryDataFromBytes (unsafeDecodeBase16 bytes)

    _waitDatum :: DatumHash -> IO BinaryData
    _waitDatum datumHash = do
        _lookupDatumByHash datumHash >>= \case
            Nothing -> do
                threadDelay 0.25
                _waitDatum datumHash
            Just bin ->
                pure bin

    _lookupScriptByHash :: ScriptHash -> IO (Maybe Script)
    _lookupScriptByHash scriptHash = do
        let fragment = toString (scriptHashToText scriptHash)
        req <- parseRequest (baseUrl <> "/v1/scripts/" <> fragment)
        res <- httpLbs req manager
        let body = responseBody res
        case Json.eitherDecode' body of
            Left e ->
                fail (show e)
            Right Json.Null -> do
                pure Nothing
            Right val -> maybe (fail "failed to decode Script.") (pure . Just) $ do
                bytes <- val ^? key "script" . _String
                lang <- val ^? key "language" . _String
                prefix <- case lang of
                    "native" -> Just "00"
                    "plutus:v1" -> Just "01"
                    "plutus:v2" -> Just "02"
                    _ -> Nothing
                scriptFromBytes (unsafeDecodeBase16 (prefix <> bytes))

    _waitScript :: ScriptHash -> IO Script
    _waitScript scriptHash = do
        _lookupScriptByHash scriptHash >>= \case
            Nothing -> do
                threadDelay 0.25
                _waitScript scriptHash
            Just script ->
                pure script

--
-- Decoders
--

eitherDecode :: (Json.Value -> Json.Parser a) -> LByteString -> Either String a
eitherDecode decoder =
    left snd . Json.eitherDecodeWith Json.jsonEOF (Json.iparse decoder)

decodeOutputReference :: Json.KeyMap Json.Value -> Json.Parser OutputReference
decodeOutputReference o = mkOutputReference
    <$> (o .: "transaction_id" >>= decodeTransactionId)
    <*> o .: "output_index"

decodePoint :: Json.Value -> Json.Parser Point
decodePoint v = decodePointSlot v <|> decodeGenesisPoint v
  where
    decodeGenesisPoint =
        Json.withText "GenesisPoint" $ \t -> do
            guard (t == "origin")
            pure GenesisPoint

    decodePointSlot =
        Json.withObject "Point" $ \o -> do
            (slotNo :: Word) <- o .: "slot_no"
            headerHash <- o .: "header_hash"
            case pointFromText (show slotNo <> "." <> headerHash) of
                Nothing -> fail "decodePoint"
                Just pt -> pure pt

decodeAddress
    :: Text
    -> Json.Parser Address
decodeAddress txt =
    case patternFromText txt of
        Just (MatchExact addr) ->
            pure addr
        _ ->
            empty

decodeDatumHash
    :: Text
    -> Json.Parser DatumHash
decodeDatumHash k = do
    case datumHashFromBytes <$> decodeBase16 (encodeUtf8 k) of
        Right (Just hash) ->
            pure hash
        Right Nothing ->
            fail "decodeDatumHash: datumHashFromBytes failed."
        Left e ->
            fail (toString e)

decodeDatum
    :: Maybe Text
    -> Json.Parser Datum
decodeDatum = \case
    Nothing ->
        pure noDatum
    Just str ->
        fromDatumHash <$> decodeDatumHash str

decodeScriptHash
    :: Text
    -> Json.Parser ScriptHash
decodeScriptHash k = do
    case scriptHashFromBytes <$> decodeBase16 (encodeUtf8 k) of
        Right (Just hash) ->
            pure hash
        Right Nothing ->
            fail "decodeScriptHash: scriptHashFromBytes failed."
        Left e ->
            fail (toString e)

decodeScriptReference
    :: Maybe Text
    -> Json.Parser ScriptReference
decodeScriptReference = \case
    Nothing ->
        pure NoScript
    Just str ->
        ReferencedScript <$> decodeScriptHash str

decodeTransactionId
    :: Text
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
        case T.splitOn "." assetId of
            [ decodeBase16' -> Right policyId, decodeBase16' -> Right assetName ] -> do
                pure (policyId, assetName, quantity)
            [ decodeBase16' -> Right policyId ] -> do
                pure (policyId, mempty, quantity)
            _ ->
                empty

decodeHash
    :: HashAlgorithm alg
    => Text
    -> Json.Parser (Hash alg a)
decodeHash =
    maybe empty pure . hashFromTextAsHex

decodeResult
    :: Json.Value
    -> Json.Parser Result
decodeResult = Json.withObject "Result" $ \o -> Result
    <$> (decodeOutputReference o)
    <*> (decodeAddress =<< (o .: "address"))
    <*> (decodeValue =<< (o .: "value"))
    <*> (decodeDatum =<< (o .:? "datum_hash"))
    <*> (decodeScriptReference =<< (o .:? "script_hash"))
    <*> (decodePoint =<< (o .: "created_at"))
    <*> (traverse decodePoint =<< (o .:? "spent_at"))
