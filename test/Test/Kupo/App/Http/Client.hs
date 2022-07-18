--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.App.Http.Client where

import Kupo.Prelude

import Data.Aeson.Lens
    ( key, _Integer, _String )
import Data.List
    ( maximum )
import Kupo.Control.MonadCatch
    ( MonadCatch (..) )
import Kupo.Control.MonadDelay
    ( MonadDelay (..) )
import Kupo.Data.Cardano
    ( BinaryData
    , Block
    , DatumHash
    , Point
    , SlotNo (..)
    , binaryDataFromBytes
    , datumHashToText
    , getPointSlotNo
    , pointFromText
    )
import Kupo.Data.Configuration
    ( Configuration (..) )
import Kupo.Data.Http.GetCheckpointMode
    ( GetCheckpointMode (..) )
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

import qualified Data.Aeson as Json

data HttpClient (m :: Type -> Type) = HttpClient
    { waitUntilM
        :: (m Bool) -> m ()
    , waitSlot
        :: (SlotNo -> Bool) -> m ()
    , lookupDatum
        :: DatumHash -> m BinaryData
    , listCheckpoints
        :: m [Point Block]
    , getCheckpointBySlot
        :: GetCheckpointMode -> SlotNo -> m (Maybe (Point Block))
    , getAllMatches
        :: m [Json.Value]
    }

newHttpClient :: Configuration -> IO (HttpClient IO)
newHttpClient config = do
    manager <- newManager defaultManagerSettings
    newHttpClientWith manager config

newHttpClientWith :: Manager -> Configuration -> IO (HttpClient IO)
newHttpClientWith manager cfg = do
    waitForServer $> HttpClient
        { waitUntilM
        , waitSlot
        , lookupDatum
        , listCheckpoints
        , getCheckpointBySlot
        , getAllMatches
        }
  where
    baseUrl :: String
    baseUrl = "http://" <> serverHost cfg <> ":" <> show (serverPort cfg)

    waitForServer :: IO ()
    waitForServer = do
        req <- parseRequest (baseUrl <> "/v1/health")
        void (httpNoBody req manager) `catch` (\(_ :: HttpException) -> do
            threadDelay 0.1
            waitForServer)

    waitUntilM :: IO Bool -> IO ()
    waitUntilM predicate = do
        predicate >>= \case
            True ->
                return ()
            False -> do
                threadDelay 0.25
                waitUntilM predicate

    waitSlot :: (SlotNo -> Bool) -> IO ()
    waitSlot predicate = do
        slots <- fmap getPointSlotNo <$> listCheckpoints
        unless (not (null slots) && predicate (maximum slots)) $ do
            threadDelay 0.25
            waitSlot predicate

    listCheckpoints :: IO [Point Block]
    listCheckpoints = do
        req <- parseRequest (baseUrl <> "/v1/checkpoints")
        res <- httpLbs req manager
        let body = responseBody res
        case Json.eitherDecode' body of
            Left e ->
                fail (show e)
            Right (xs :: [Json.Value]) ->
                traverse decodePoint xs

    getCheckpointBySlot :: GetCheckpointMode -> SlotNo -> IO (Maybe (Point Block))
    getCheckpointBySlot mode (SlotNo slot) = do
        let qry = case mode of
                GetCheckpointStrict -> "?strict"
                GetCheckpointClosestAncestor -> ""
        req <- parseRequest (baseUrl <> "/v1/checkpoints/" <> show slot <> qry)
        res <- httpLbs req manager
        let body = responseBody res
        case Json.eitherDecode' body of
            Left e ->
                fail (show e)
            Right (val :: Json.Value) ->
                pure (decodePointMaybe val)

    getAllMatches :: IO [Json.Value]
    getAllMatches = do
        req <- parseRequest (baseUrl <> "/v1/matches")
        res <- httpLbs req manager
        let body = responseBody res
        case Json.eitherDecode' body of
            Left e ->
                fail (show e)
            Right xs ->
                pure xs

    lookupDatum :: DatumHash -> IO BinaryData
    lookupDatum datumHash = do
        let fragment = toString (datumHashToText datumHash)
        req <- parseRequest (baseUrl <> "/v1/datums/" <> fragment)
        res <- httpLbs req manager
        let body = responseBody res
        case Json.eitherDecode' body of
            Left e ->
                fail (show e)
            Right Json.Null -> do
                threadDelay 0.25
                lookupDatum datumHash
            Right val -> maybe (fail "failed to decode Datum.") pure $ do
                bytes <- val ^? key "datum" . _String
                binaryDataFromBytes (unsafeDecodeBase16 bytes)

--
-- Decoders
--

decodePointMaybe :: Json.Value -> Maybe (Point Block)
decodePointMaybe val = do
    slotNo <- val ^? key "slot_no" . _Integer
    headerHash <- val ^? key "header_hash" . _String
    pointFromText (show slotNo <> "." <> toText headerHash)

decodePoint :: MonadFail m => Json.Value -> m (Point Block)
decodePoint val = do
    case decodePointMaybe val of
        Nothing ->
            fail "failed to decode Point."
        Just point ->
            pure point
