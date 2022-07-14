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
    , MatchBootstrap (OnlyShelley, IncludingBootstrap)
    , overlaps
    , includes
    , included
    , patternFromText
    , patternToText
    , patternFromPath
    , wildcard

      -- * Matching
    , matching
    , matchBlock

      -- * Result
    , Result (..)
    , resultToJson
    ) where

import Kupo.Prelude

import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Kupo.Data.Cardano
    ( Address
    , Blake2b_224
    , Blake2b_256
    , Block
    , DatumHash
    , Input
    , IsBlock (..)
    , Output
    , OutputReference
    , Point
    , SlotNo
    , Value
    , addressFromBytes
    , addressToBytes
    , addressToJson
    , datumHashToJson
    , digest
    , digestSize
    , getAddress
    , getDatumHash
    , getDelegationPartBytes
    , getOutputIndex
    , getPaymentPartBytes
    , getPointSlotNo
    , getTransactionId
    , getValue
    , headerHashToJson
    , isBootstrap
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
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

data Pattern
    = MatchAny MatchBootstrap
    | MatchExact Address
    | MatchPayment ByteString
    | MatchDelegation ByteString
    | MatchPaymentAndDelegation ByteString ByteString
    deriving (Generic, Eq, Ord, Show)

-- | Checks whether a given pattern overlaps with a list of other patterns. The
-- inclusion is a deep inclusion such that for instance, (MatchPayment "a") is
-- considered included in [MatchAny OnlyShelley].
overlaps :: Pattern -> [Pattern] -> Bool
overlaps p = \case
    [] -> False
    p':rest ->
        overlapTwo (p, p') || overlapTwo (p', p) || overlaps p rest
  where
    overlapTwo = \case
        (MatchAny{}, _) ->
            True
        (MatchExact addr, p') ->
            isJust (matching addr p')
        (MatchPayment a, MatchPayment a') ->
            a == a'
        (MatchPayment a, MatchPaymentAndDelegation a' _) ->
            a == a'
        (MatchDelegation b, MatchDelegation b') ->
            b == b'
        (MatchDelegation b, MatchPaymentAndDelegation _ b') ->
            b == b'
        (MatchPaymentAndDelegation a b, MatchPaymentAndDelegation a' b') ->
            a == a' || b == b'
        _ ->
            False


-- | Checks whether a pattern x fully includes pattern y.
-- If any result matched by y is also matched by x, then the function returns
-- 'True'. Otherwise it returns 'False'. The converse may not be true:
-- x may match more results than y.
includes :: Pattern -> Pattern -> Bool
includes x y = case (x, y) of
    (p, MatchExact addr') ->
        isJust (matching addr' p)
    (MatchAny IncludingBootstrap, _) ->
        True
    (MatchAny OnlyShelley, _) ->
        y /= MatchAny IncludingBootstrap
    (MatchPayment a, MatchPayment a') ->
        a == a'
    (MatchPayment a, MatchPaymentAndDelegation a' _) ->
        a == a'
    (MatchDelegation b, MatchDelegation b') ->
        b == b'
    (MatchDelegation b, MatchPaymentAndDelegation _ b') ->
        b == b'
    (MatchPaymentAndDelegation a b, MatchPaymentAndDelegation a' b') ->
        a == a' && b == b'
    _ ->
        False

-- | All patterns in the list that are fully covering the given pattern
-- according to the definition given by 'includes'
included :: Pattern -> [Pattern] -> [Pattern]
included p = filter (`includes` p)

wildcard :: Text
wildcard = "*"
{-# INLINEABLE wildcard #-}

patternFromPath :: [Text] -> Maybe Text
patternFromPath = \case
    [] ->
        Just wildcard
    [arg0] ->
        Just arg0
    [arg0, arg1] ->
        Just (arg0 <> "/" <> arg1)
    _ ->
        Nothing

patternToText :: Pattern -> Text
patternToText = \case
    MatchAny IncludingBootstrap ->
        wildcard
    MatchAny OnlyShelley ->
        wildcard <> "/" <> wildcard
    MatchExact addr ->
        encodeBase16 (addressToBytes addr)
    MatchPayment bytes ->
        encodeBase16 bytes <> "/" <> wildcard
    MatchDelegation bytes ->
        wildcard <> "/" <> encodeBase16 bytes
    MatchPaymentAndDelegation a b ->
        encodeBase16 a <> "/" <> encodeBase16 b

patternFromText :: Text -> Maybe Pattern
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

matching :: (Monad f, Alternative f) => Address -> Pattern -> f ()
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
    deriving (Generic, Eq, Ord, Show)

pattern IncludingBootstrap :: MatchBootstrap
pattern IncludingBootstrap <- MatchBootstrap True
  where
    IncludingBootstrap = MatchBootstrap True

pattern OnlyShelley :: MatchBootstrap
pattern OnlyShelley <- MatchBootstrap False
  where
    OnlyShelley = MatchBootstrap False

{-# COMPLETE OnlyShelley, IncludingBootstrap #-}

data Result = Result
    { outputReference :: OutputReference
    , address :: Address
    , value :: Value
    , datumHash :: Maybe DatumHash
    , createdAt :: Point Block
    , spentAt :: Maybe (Point Block)
    } deriving (Show, Eq)

resultToJson
    :: Result
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
    , Json.pair "created_at"
        (Json.pairs $ mconcat
            [ Json.pair "slot_no"
                (slotNoToJson (getPointSlotNo createdAt))
            , Json.pair "header_hash"
                (headerHashToJson (unsafeGetPointHeaderHash createdAt))
            ]
        )
    , Json.pair "spent_at"
        (case spentAt of
            Nothing -> Json.null_
            Just point ->
                Json.pairs $ mconcat
                    [ Json.pair "slot_no"
                        (slotNoToJson (getPointSlotNo point))
                    , Json.pair "header_hash"
                        (headerHashToJson (unsafeGetPointHeaderHash point))
                    ]
        )
    ]

-- | Match all outputs in transactions from a block that match any of the given
-- pattern.
--
-- Note that this may yield multiple time the same result if being matched by
-- multiple patterns. This is to facilitate building an index of matches to
-- results.
matchBlock
    :: forall block result slotNo input.
        ( IsBlock block
        , Ord input
        , Ord slotNo
        )
    => (Result -> result)
    -> (SlotNo -> slotNo)
    -> (Input -> input)
    -> [Pattern]
    -> block
    -> (Map slotNo (Set input), [result])
matchBlock toResult toSlotNo toInput patterns blk =
    let pt = getPoint blk in foldBlock (fn pt) (mempty, mempty) blk
  where
    fn
        :: Point Block
        -> BlockBody block
        -> (Map slotNo (Set input), [result])
        -> (Map slotNo (Set input), [result])
    fn pt tx (consumed, produced) =
        ( Map.alter
            (\st -> Just (Set.foldr (Set.insert . toInput) (fromMaybe mempty st) (spentInputs @block tx)))
            (toSlotNo (getPointSlotNo pt))
            consumed
        , concatMap (flip (mapMaybeOutputs @block) tx . match pt) patterns ++ produced
        )

    match
        :: Point Block
        -> Pattern
        -> OutputReference
        -> Output
        -> Maybe result
    match pt m outputReference out = do
        getAddress out `matching` m
        pure $ toResult Result
            { outputReference
            , address = getAddress out
            , value = getValue out
            , datumHash = getDatumHash out
            , createdAt = pt
            , spentAt = Nothing
            }
