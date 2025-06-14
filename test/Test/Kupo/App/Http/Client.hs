--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.App.Http.Client where

import Kupo.Prelude

import Cardano.Crypto.Hash.Class
    ( hashFromTextAsHex
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
    ( threadDelay
    )
import Kupo.Data.Cardano
    ( Address
    , BinaryData
    , Datum (..)
    , DatumHash
    , ExtendedOutputReference
    , Metadata
    , MetadataHash
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
    , getPointSlotNo
    , metadataFromText
    , metadataHashFromText
    , mkOutputReference
    , pointFromText
    , scriptFromBytes
    , scriptHashFromBytes
    , scriptHashToText
    , slotNoToText
    , transactionIdFromHash
    , transactionIdToText
    , unsafeValueFromList
    )
import Kupo.Data.Configuration
    ( DeferIndexesInstallation (..)
    )
import Kupo.Data.Health
    ( ConnectionStatus (..)
    , Health (..)
    )
import Kupo.Data.Http.ForcedRollback
    ( ForcedRollback (..)
    , ForcedRollbackLimit (..)
    , forcedRollbackToJson
    )
import Kupo.Data.Http.GetCheckpointMode
    ( GetCheckpointMode (..)
    )
import Kupo.Data.Http.ReferenceFlag
    ( ReferenceFlag (..)
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
        -> ReferenceFlag
        -> m [Result]
    , putPatternSince
        :: Pattern
        -> Either SlotNo Point
        -> m Bool
    , listPatterns
        :: m [Pattern]
    , getHealth
        :: m Health
    }

newHttpClient :: (String, Int) -> IO (HttpClient IO)
newHttpClient config = do
    manager <- newManager defaultManagerSettings
    pure $ newHttpClientWith manager config (\_ -> pure ())

newHttpClientWith :: Manager -> (String, Int) -> (Text -> IO ()) -> HttpClient IO
newHttpClientWith manager (serverHost, serverPort) log =
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
            \a0 a1 -> waitForServer >> _getAllMatches a0 a1
        , putPatternSince =
            \a0 a1 -> waitForServer >> _putPatternSince a0 a1
        , listPatterns =
            waitForServer >> _listPatterns
        , getHealth =
            waitForServer >> _getHealth
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

    _getAllMatches :: StatusFlag -> ReferenceFlag -> IO [Result]
    _getAllMatches st ref = do
        let q = case (st, ref) of
                 (NoStatusFlag, AsReference) -> ""
                 (NoStatusFlag, InlineAll) -> "?resolve_hashes"
                 (OnlySpent, AsReference) -> "?spent"
                 (OnlySpent, InlineAll) -> "?spent&resolve_hashes"
                 (OnlyUnspent, AsReference) -> "?unspent"
                 (OnlyUnspent, InlineAll) -> "?resolve_hashes&unspent"
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
            Right val ->
                Just <$> decodeScript val

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

    _getHealth :: IO Health
    _getHealth = do
        req <- parseRequest (baseUrl <> "/health")
        res <- httpLbs
            (req
                { requestHeaders = [("Accept", "application/json")]
                }
            )
            manager
        let body = responseBody res
        case eitherDecodeJson decodeHealth body of
            Left e ->
                fail (show body <> " ----> " <> show e)
            Right h ->
                pure h

--
-- Decoders
--

decodeHealth :: Json.Value -> Json.Parser Health
decodeHealth = Json.withObject "Health" $ \o -> do
    connectionStatus <- o .: "connection_status" >>= decodeConnectionStatus
    mostRecentNodeTip <- o .: "most_recent_node_tip"
    configuration <- o .: "configuration" >>= (.:? "indexes") >>= traverse decodeDeferIndexesInstallation
    pure Health
        { connectionStatus
        , configuration
        , mostRecentNodeTip = Just (SlotNo mostRecentNodeTip)
        -- NOTE: We only have the point's slot number here. No test should rely on that.
        , mostRecentClockTick = Nothing
        , mostRecentCheckpoint = Nothing
        }
  where
    decodeConnectionStatus = Json.withText "ConnectionStatus" $ \case
        "connected" -> pure Connected
        "disconnected" -> pure Disconnected
        _ -> empty

    decodeDeferIndexesInstallation = Json.withText "DeferIndexesInstallation" $ \case
        "deferred" -> pure SkipNonEssentialIndexes
        "installed" -> pure InstallIndexesIfNotExist
        _ -> empty

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

decodeRedeemer :: Json.Value -> Json.Parser (Maybe BinaryData)
decodeRedeemer =
    Json.withObject "Redeemer" $ \o -> do
        redeemer <- o .:? "redeemer"
        case redeemer of
            Just bytes ->
                maybe (fail "failed to decode Redeemer") (pure . Just)
                    (binaryDataFromBytes $ unsafeDecodeBase16 bytes)
            Nothing ->
                pure Nothing

decodeInputReference :: Json.KeyMap Json.Value -> Json.Parser OutputReference
decodeInputReference o = do
    mkOutputReference
        <$> (o .: "transaction_id" >>= decodeTransactionId)
        <*> o .: "input_index"

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
    -> Maybe Text
    -> Maybe Text
    -> Json.Parser Datum
decodeDatum mDatumType mRef mDatum =
    case (mDatumType, mDatum) of
        (Nothing, Nothing) ->
            pure NoDatum
        (Just "inline", Just bytes) ->
            maybe
                (fail "invalid datum")
                (pure . Inline . Right)
                (binaryDataFromBytes (unsafeDecodeBase16 bytes))
        (Just "hash", Just bytes) ->
            maybe
                (fail "invalid datum")
                (pure . Reference . Right)
                (binaryDataFromBytes (unsafeDecodeBase16 bytes))
        _ -> case (mDatumType, mRef) of
            (Nothing, Nothing) ->
                pure NoDatum
            (Just "inline", Just ref) ->
                Inline . Left <$> decodeDatumHash ref
            (Just "hash", Just ref) ->
                Reference . Left <$> decodeDatumHash ref
            _unexpectedResponse ->
                fail $ "decodeDatum: malformed datum response: " <> show (mDatumType, mRef)

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
    -> Maybe Json.Value
    -> Json.Parser ScriptReference
decodeScriptReference mScriptHash mScript =
    case (mScriptHash, mScript) of
        (Nothing, Nothing) ->
            pure NoScript
        (Just scriptHash, Nothing) ->
            ReferencedScript <$> decodeScriptHash scriptHash
        (_, Just script) ->
            InlineScript <$> decodeScript script

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

decodeScript
    :: MonadFail m
    => Json.Value
    -> m Script
decodeScript obj = do
    maybe (fail "failed to decode Script.") pure $ do
        bytes <- obj ^? key "script" . _String
        lang <- obj ^? key "language" . _String
        prefix <- case lang of
            "native" -> Just "00"
            "plutus:v1" -> Just "01"
            "plutus:v2" -> Just "02"
            "plutus:v3" -> Just "03"
            _ -> Nothing
        scriptFromBytes (unsafeDecodeBase16 (prefix <> bytes))

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
    <*> join (liftA3 decodeDatum (o .:? "datum_type") (o .:? "datum_hash") (o .:? "datum"))
    <*> join (liftA2 decodeScriptReference (o .:? "script_hash") (o .:? "script"))
    <*> (decodePoint =<< (o .: "created_at"))
    <*> (traverse decodePoint =<< (o .:? "spent_at"))
    <*> (traverse decodeInputReference =<< (o .:? "spent_at"))
    <*> (do
            spentAt <- o .:? "spent_at"
            redeemer <- traverse decodeRedeemer spentAt
            pure (join redeemer)
        )
