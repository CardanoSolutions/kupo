--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE QuasiQuotes #-}

module Kupo.Data.Pattern
    ( -- * Pattern
      Pattern (..)
    , patternFromText
    , matching
    , Result (..)
    , matchBlock

      -- ** MatchBootstrap
    , MatchBootstrap
    , onlyShelley
    , includingBootstrap
    ) where

import Kupo.Prelude

import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Kupo.Data.ChainSync
    ( Address
    , Blake2b_224
    , Blake2b_256
    , Block
    , Crypto
    , DatumHash
    , Output
    , OutputReference
    , Value
    , addressFromBytes
    , digest
    , digestSize
    , foldBlock
    , getAddress
    , getDatumHash
    , getDelegationPartBytes
    , getPaymentPartBytes
    , getValue
    , isBootstrap
    , mapMaybeOutputs
    )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString as BS
import qualified Data.Text as T

data Pattern crypto
    = MatchAny MatchBootstrap
    | MatchExact (Address crypto)
    | MatchPayment ByteString
    | MatchDelegation ByteString
    | MatchPaymentAndDelegation ByteString ByteString
    deriving (Generic, Eq, Show)

patternFromText :: Crypto crypto => Text -> Maybe (Pattern crypto)
patternFromText txt =
    readerAny <|> readerExact <|> readerPaymentOrDelegation
  where
    wildcard = "*"

    readerAny = MatchAny includingBootstrap
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

    readerPaymentOrDelegation =
        case T.splitOn "/" txt of
            [payment, delegation] -> do
                if  | payment == wildcard && delegation == wildcard ->
                        pure (MatchAny onlyShelley)
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

includingBootstrap :: MatchBootstrap
includingBootstrap = MatchBootstrap True

onlyShelley :: MatchBootstrap
onlyShelley = MatchBootstrap False

data Result crypto = Result
    { address :: Address crypto
    , value :: Value crypto
    , datumHash :: Maybe (DatumHash crypto)
    , reference :: OutputReference crypto
    }

-- | Match all outputs in transactions from a block that match any of the given
-- pattern.
--
-- Note that this may yield multiple time the same result if being matched by
-- multiple patterns. This is to facilitate building an index of matches to
-- results.
matchBlock
    :: forall crypto. (Crypto crypto)
    => [Pattern crypto]
    -> Block crypto
    -> [(Pattern crypto, Result crypto)]
matchBlock ms = flip foldBlock [] $ \tx result ->
    concatMap (flip mapMaybeOutputs tx . match) ms ++ result
  where
    match
        :: Pattern crypto
        -> OutputReference crypto
        -> Output crypto
        -> Maybe (Pattern crypto, Result crypto)
    match m reference out = do
        getAddress out `matching` m
        pure
            ( m
            , Result
                { address = getAddress out
                , value = getValue out
                , datumHash = getDatumHash out
                , reference
                }
            )
