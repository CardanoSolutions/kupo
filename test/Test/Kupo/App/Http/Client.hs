--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.App.Http.Client where

import Kupo.Prelude

import Cardano.Crypto.Hash.Class
    ( Hash
    , HashAlgorithm
    , hashFromTextAsHex
    )
import Data.Aeson
    ( (.!=)
    , (.:)
    , (.:?)
    )
import Data.Aeson.Lens
    ( _String
    , key
    )
import Data.List
    ( maximum
    )
import Kupo.Control.MonadCatch
    ( MonadCatch (..)
    )
import Kupo.Control.MonadDelay
    ( MonadDelay (..)
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Data.Cardano
    ( Address
    , BinaryData
    , Blake2b_256
    , Datum
    , DatumHash
    , ExtendedOutputReference
    , Metadata
    , MetadataHash
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
    , metadataFromText
    , metadataHashFromText
    , mkOutputReference
    , noDatum
    , pointFromText
    , scriptFromBytes
    , scriptHashFromBytes
    , scriptHashToText
    , slotNoToText
    , transactionIdFromHash
    , transactionIdToText
    , unsafeValueFromList
    )
import Kupo.Data.Http.ForcedRollback
    ( ForcedRollback (..)
    , ForcedRollbackLimit (..)
    , forcedRollbackToJson
    )
import Kupo.Data.Http.GetCheckpointMode
    ( GetCheckpointMode (..)
    )
import Kupo.Data.Http.StatusFlag
    ( StatusFlag (..)
    )
import Kupo.Data.Pattern
    ( Pattern (..)
    , Result (..)
    , patternFromText
    , patternToText
    )
import Network.HTTP.Client
    ( HttpException
    , Manager
    , Request (..)
    , RequestBody (..)
    , Response (..)
    , defaultManagerSettings
    , httpLbs
    , httpNoBody
    , newManager
    , parseRequest
    )
import Network.HTTP.Types.Status
    ( status200
    , status400
    )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.KeyMap as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Lazy.Char8 as BL8
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
    , lookupMetadataBySlotNo
        :: SlotNo -> Maybe TransactionId -> m [(MetadataHash, Metadata)]
    , listCheckpoints
        :: m [Point]
    , getCheckpointBySlot
        :: GetCheckpointMode -> SlotNo -> m (Maybe Point)
    , getAllMatches
        :: StatusFlag
        -> m [Result]
    , putPatternSince
        :: Pattern
        -> Either SlotNo Point
        -> m Bool
    , listPatterns
        :: m [Pattern]
    }

newHttpClient :: (String, Int) -> IO (HttpClient IO)
newHttpClient config = do
    manager <- newManager defaultManagerSettings
    logs <- newTVarIO []
    pure (newHttpClientWith manager config logs)

newHttpClientWith :: Manager -> (String, Int) -> TVar IO [Text] -> HttpClient IO
newHttpClientWith manager (serverHost, serverPort) logs =
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
        , lookupMetadataBySlotNo =
            \a0 a1 -> waitForServer >> _lookupMetadataBySlotNo a0 a1
        , listCheckpoints =
            waitForServer >> _listCheckpoints
        , getCheckpointBySlot =
            \a0 a1 -> waitForServer >> _getCheckpointBySlot a0 a1
        , getAllMatches =
            \a0 -> waitForServer >> _getAllMatches a0
        , putPatternSince =
            \a0 a1 -> waitForServer >> _putPatternSince a0 a1
        , listPatterns =
            waitForServer >> _listPatterns
        }
  where
    log :: Text -> IO ()
    log = atomically . modifyTVar' logs . (:)

    baseUrl :: String
    baseUrl = "http://" <> serverHost <> ":" <> show serverPort

    waitForServer :: IO ()
    waitForServer = loop 0
      where
        loop (n :: Word)
            | n > 100 =
                fail "waitForServer: timeout."
            | otherwise = do
                req <- parseRequest (baseUrl <> "/health")
                void (httpNoBody req manager) `catch` (\(_ :: HttpException) -> do
                    log "waitForServer"
                    threadDelay 0.1
                    loop (next n))

    _waitUntilM :: IO Bool -> IO ()
    _waitUntilM predicate = do
        result <- predicate
        log $ "waitUntilM: " <> show result
        unless result $ do
            threadDelay 0.25
            _waitUntilM predicate

    _waitSlot :: (SlotNo -> Bool) -> IO ()
    _waitSlot predicate = do
        _listCheckpoints >>= \case
            [] -> do
                log "waitSlot: no checkpoint"
                threadDelay 0.05
                _waitSlot predicate
            (maximum . fmap getPointSlotNo -> sl) -> do
                log $ "waitSlot (" <> slotNoToText sl <> "): " <> show (predicate sl)
                unless (predicate sl) $ do
                    threadDelay 0.05
                    _waitSlot predicate

    _listCheckpoints :: IO [Point]
    _listCheckpoints = do
        req <- parseRequest (baseUrl <> "/checkpoints")
        res <- httpLbs req manager
        let body = responseBody res
        case eitherDecodeJson (Json.listParser decodePoint) body of
            Left e ->
                fail (show body <> " ----> " <> show e)
            Right xs ->
                pure xs

    _getCheckpointBySlot :: GetCheckpointMode -> SlotNo -> IO (Maybe Point)
    _getCheckpointBySlot mode (SlotNo slot) = do
        let qry = case mode of
                GetCheckpointStrict -> "?strict"
                GetCheckpointClosestAncestor -> ""
        req <- parseRequest (baseUrl <> "/checkpoints/" <> show slot <> qry)
        res <- httpLbs req manager
        let body = responseBody res
        pure $ either (const Nothing) Just (eitherDecodeJson decodePoint body)

    _getAllMatches :: StatusFlag -> IO [Result]
    _getAllMatches st = do
        let q = case st of
                 NoStatusFlag -> ""
                 OnlySpent -> "?spent"
                 OnlyUnspent -> "?unspent"
        req <- parseRequest (baseUrl <> "/matches" <> q)
        res <- httpLbs req manager
        let body = responseBody res
        case eitherDecodeJson (Json.listParser decodeResult) body of
            Left e ->
                fail (show e)
            Right xs ->
                pure xs

    _lookupDatumByHash :: DatumHash -> IO (Maybe BinaryData)
    _lookupDatumByHash datumHash = do
        let fragment = toString (datumHashToText datumHash)
        req <- parseRequest (baseUrl <> "/datums/" <> fragment)
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
        let datumStr = T.take 8 (datumHashToText datumHash)
        _lookupDatumByHash datumHash >>= \case
            Nothing -> do
                log $ "waitDatum (" <> datumStr <> "): not found"
                threadDelay 0.25
                _waitDatum datumHash
            Just bin -> do
                log $ "waitDatum (" <> datumStr <> "): found"
                pure bin

    _lookupScriptByHash :: ScriptHash -> IO (Maybe Script)
    _lookupScriptByHash scriptHash = do
        let fragment = toString (scriptHashToText scriptHash)
        req <- parseRequest (baseUrl <> "/scripts/" <> fragment)
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
        let scriptStr = T.take 8 (scriptHashToText scriptHash)
        _lookupScriptByHash scriptHash >>= \case
            Nothing -> do
                log $ "waitScript (" <> scriptStr <> "): not found"
                threadDelay 0.25
                _waitScript scriptHash
            Just script -> do
                log $ "waitScript (" <> scriptStr <> "): found"
                pure script

    _lookupMetadataBySlotNo :: SlotNo -> Maybe TransactionId -> IO [(MetadataHash, Metadata)]
    _lookupMetadataBySlotNo slotNo filterBy = do
        let fragment = toString (slotNoToText slotNo)
        let query = case filterBy of
                Nothing -> ""
                Just id -> toString ("?transaction_id=" <> transactionIdToText id)
        req <- parseRequest (baseUrl <> "/metadata/" <> fragment <> query)
        res <- httpLbs req manager
        let body = responseBody res
        if | responseStatus res == status200 ->
                case eitherDecodeJson (Json.listParser decodeMetadata) body of
                    Left e ->
                        fail (show e)
                    Right xs ->
                        pure xs
           | responseStatus res == status400 && "no known-ancestor" `T.isInfixOf` (decodeUtf8 body) ->
                return []
           | otherwise ->
                fail ("Unexpected response from the server: " <> BL8.unpack body)

    _putPatternSince :: Pattern -> Either SlotNo Point -> IO Bool
    _putPatternSince p pt = do
        let fragment = toString (patternToText p)
        req <- parseRequest (baseUrl <> "/patterns/" <> fragment) <&>
            ( \r -> r
                { method = "PUT"
                , requestBody
                    = RequestBodyLBS
                    $ Json.encodingToLazyByteString
                    $ forcedRollbackToJson
                    $ ForcedRollback
                        { since = pt
                        , limit = UnsafeAllowRollbackBeyondSafeZone
                        }
                }
            )
        res <- httpLbs req manager
        if
            | responseStatus res == status200 ->
                return True
            | responseStatus res == status400 ->
                return False
            | otherwise ->
                fail ("Unexpected response from server: " <> show res)

    _listPatterns :: IO [Pattern]
    _listPatterns = do
        req <- parseRequest (baseUrl <> "/patterns")
        res <- httpLbs req manager
        let body = responseBody res
        case traverse patternFromText <$> Json.eitherDecode' body of
            Left e ->
                fail (show body <> " ----> " <> show e)
            Right Nothing ->
                fail "Failed to decode patterns returned by the server"
            Right (Just xs) ->
                pure xs

--
-- Decoders
--

decodeOutputReference :: Json.KeyMap Json.Value -> Json.Parser ExtendedOutputReference
decodeOutputReference o = do
    txIx <- o .: "transaction_index"
    outRef <- mkOutputReference
        <$> (o .: "transaction_id" >>= decodeTransactionId)
        <*> o .: "output_index"
    pure (outRef, txIx)

decodePoint :: Json.Value -> Json.Parser Point
decodePoint =
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
        _notAnAddress ->
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
            _invalidSplit ->
                empty

decodeHash
    :: HashAlgorithm alg
    => Text
    -> Json.Parser (Hash alg a)
decodeHash =
    maybe empty pure . hashFromTextAsHex

decodeMetadata
    :: Json.Value
    -> Json.Parser (MetadataHash, Metadata)
decodeMetadata = Json.withObject "Metadata" $ \o -> do
    hash <- (o .: "hash")
        >>= maybe (fail "failed to decode metadata hash") pure . metadataHashFromText
    meta <- (o .: "raw")
        >>= maybe (fail "failed to decode metadata payload") pure . metadataFromText
    pure (hash, meta)

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
