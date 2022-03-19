--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Kupo.Data.Pattern
    ( -- * Pattern
      Pattern (..)
    , wildcard
    , patternFromText
    , matching

      -- ** MatchBootstrap
    , MatchBootstrap (OnlyShelley, IncludingBootstrap)

      -- * Result
    , Result (..)
    , resultToJson
    , matchBlock
    ) where

import Kupo.Prelude

import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Kupo.Data.ChainSync
    ( Address
    , Blake2b_224
    , Blake2b_256
    , Block
    , CardanoHardForkConstraints
    , Crypto
    , DatumHash
    , HasHeader
    , Output
    , OutputReference
    , Point
    , PraosCrypto
    , Transaction
    , Value
    , addressFromBytes
    , addressToJson
    , datumHashToJson
    , digest
    , digestSize
    , foldBlock
    , getAddress
    , getDatumHash
    , getDelegationPartBytes
    , getOutputIndex
    , getPaymentPartBytes
    , getPoint
    , getPointSlotNo
    , getTransactionId
    , getValue
    , headerHashToJson
    , isBootstrap
    , mapMaybeOutputs
    , outputIndexToJson
    , slotNoToJson
    , transactionIdToJson
    , unsafeGetPointHeaderHash
    , valueToJson
    , valueToJson
    )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Aeson.Encoding as Json
import qualified Data.ByteString as BS
import qualified Data.Text as T

data Pattern crypto
    = MatchAny MatchBootstrap
    | MatchExact (Address crypto)
    | MatchPayment ByteString
    | MatchDelegation ByteString
    | MatchPaymentAndDelegation ByteString ByteString
    deriving (Generic, Eq, Show)

wildcard :: Text
wildcard = "*"
{-# INLINEABLE wildcard #-}

patternFromText :: Crypto crypto => Text -> Maybe (Pattern crypto)
patternFromText txt =
    readerAny <|> readerExact <|> readerPaymentOrDelegation
  where
    readerAny = MatchAny IncludingBootstrap
        <$ guard (txt == wildcard)

    readerExact =
        readerBase16 txt
            (\bytes -> do
                MatchExact <$> addressFromBytes bytes
            )
        <|>
        readerBech32 txt
            (\bytes hrp -> do
                if | hrp `elem` [ [humanReadablePart|addr|], [humanReadablePart|addr_test|] ] -> do
                        MatchExact <$> addressFromBytes bytes
                   | hrp `elem` [ [humanReadablePart|stake|], [humanReadablePart|stake_test|] ] -> do
                        pure $ MatchDelegation (BS.drop 1 bytes)
                   | otherwise -> do
                        empty
            )
        <|>
        readerBase58 txt
            (\bytes -> do
                MatchExact <$> addressFromBytes bytes
            )

    readerPaymentOrDelegation =
        case T.splitOn "/" txt of
            [payment, delegation] -> do
                if  | payment == wildcard && delegation == wildcard ->
                        pure (MatchAny OnlyShelley)
                    | payment == wildcard ->
                        MatchDelegation
                            <$> readerCredential delegation
                    | delegation == wildcard ->
                        MatchPayment
                            <$> readerCredential payment
                    | otherwise -> do
                        MatchPaymentAndDelegation
                            <$> readerCredential payment
                            <*> readerCredential delegation
            _ ->
                empty

    -- NOTE: byte strings are interpreted either as verification key or
    -- verification key hashes depending on their length. In case they are
    -- verification key, they're hashed to produce key hashes.
    readerCredential str =
        readerBase16 str (\bytes -> do
            if | BS.length bytes == digestSize @Blake2b_256 -> do
                    pure (digest (Proxy @Blake2b_224) bytes)
               | BS.length bytes == digestSize @Blake2b_224 -> do
                    pure bytes
               | otherwise ->
                    empty
        )
        <|>
        readerBech32 str (\bytes hrp -> do
            if | BS.length bytes == digestSize @Blake2b_256 -> do
                    guard $ hrp `elem`
                        [ [humanReadablePart|vk|]
                        , [humanReadablePart|addr_vk|]
                        , [humanReadablePart|stake_vk|]
                        ]
                    pure (digest (Proxy @Blake2b_224) bytes)
               | BS.length bytes == digestSize @Blake2b_224 -> do
                    guard $ hrp `elem`
                        [ [humanReadablePart|vkh|]
                        , [humanReadablePart|addr_vkh|]
                        , [humanReadablePart|stake_vkh|]
                        , [humanReadablePart|script|]
                        ]
                    pure bytes
               | otherwise ->
                    empty
        )

    readerBase16 str action = do
        bytes <- either (const Nothing) Just . decodeBase16 . encodeUtf8 $ str
        action bytes

    readerBase58 str action = do
        bytes <- either (const Nothing) Just . decodeBase58 . encodeUtf8 $ str
        action bytes

    readerBech32 str action = do
        (hrp, dataPart) <- either (const Nothing) Just . Bech32.decodeLenient $ str
        bytes <- Bech32.dataPartToBytes dataPart
        action bytes hrp

matching :: (Monad f, Alternative f) => Address crypto -> Pattern crypto -> f ()
matching addr = \case
    MatchAny (MatchBootstrap matchBootstrap) ->
        guard (not (isBootstrap addr) || matchBootstrap)
    MatchExact addr' ->
        guard (addr' == addr)
    MatchPayment payment ->
        guard (Just payment == getPaymentPartBytes addr)
    MatchDelegation delegation ->
        guard (Just delegation == getDelegationPartBytes addr)
    MatchPaymentAndDelegation payment delegation -> do
        matching addr (MatchPayment payment)
        matching addr (MatchDelegation delegation)

data MatchBootstrap
    = MatchBootstrap Bool
    deriving (Generic, Eq, Show)

pattern IncludingBootstrap :: MatchBootstrap
pattern IncludingBootstrap <- MatchBootstrap True
  where
    IncludingBootstrap = MatchBootstrap True

pattern OnlyShelley :: MatchBootstrap
pattern OnlyShelley <- MatchBootstrap False
  where
    OnlyShelley = MatchBootstrap False

{-# COMPLETE OnlyShelley, IncludingBootstrap #-}

data Result crypto = Result
    { outputReference :: OutputReference crypto
    , address :: Address crypto
    , value :: Value crypto
    , datumHash :: Maybe (DatumHash crypto)
    , point :: Point (Block crypto)
    }
deriving instance
    ( Show (Point (Block crypto))
    ) => Show (Result crypto)
deriving instance
    ( Eq (Point (Block crypto))
    , Crypto crypto
    ) => Eq (Result crypto)

resultToJson
    :: forall crypto.
        ( CardanoHardForkConstraints crypto
        )
    => Result crypto
    -> Json.Encoding
resultToJson Result{..} = Json.pairs $ mconcat
    [ Json.pair "transaction_id"
        (transactionIdToJson (getTransactionId outputReference))
    , Json.pair "output_index"
        (outputIndexToJson (getOutputIndex outputReference))
    , Json.pair "address"
        (addressToJson address)
    , Json.pair "value"
        (valueToJson value)
    , Json.pair "datum_hash"
        (maybe Json.null_ datumHashToJson datumHash)
    , Json.pair "slot_no"
        (slotNoToJson (getPointSlotNo point))
    , Json.pair "header_hash"
        (headerHashToJson (unsafeGetPointHeaderHash point))
    ]

-- | Match all outputs in transactions from a block that match any of the given
-- pattern.
--
-- Note that this may yield multiple time the same result if being matched by
-- multiple patterns. This is to facilitate building an index of matches to
-- results.
matchBlock
    :: forall crypto result.
        ( PraosCrypto crypto
        , HasHeader (Block crypto)
        )
    => (Result crypto -> result)
    -> [Pattern crypto]
    -> Block crypto
    -> [result]
matchBlock transform ms blk =
    let pt = getPoint blk in foldBlock (fn pt) [] blk
  where
    fn :: Point (Block crypto) -> Transaction crypto -> [result] -> [result]
    fn pt tx results =
        concatMap (flip mapMaybeOutputs tx . match pt) ms ++ results

    match
        :: Point (Block crypto)
        -> Pattern crypto
        -> OutputReference crypto
        -> Output crypto
        -> Maybe result
    match pt m outputReference out = do
        getAddress out `matching` m
        pure $ transform Result
            { outputReference
            , address = getAddress out
            , value = getValue out
            , datumHash = getDatumHash out
            , point = pt
            }
