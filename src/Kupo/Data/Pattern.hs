--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}
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
    , wildcard

      -- * Matching
    , Codecs (..)
    , Match (..)
    , matching
    , matchBlock

      -- * Result
    , Result (..)
    , resultToJson
    ) where

import Kupo.Prelude

import Codec.Binary.Bech32.TH
    ( humanReadablePart
    )
import Kupo.Data.Cardano
    ( Address
    , AssetId
    , BinaryData
    , Blake2b_224
    , Blake2b_256
    , Datum
    , DatumHash
    , Input
    , IsBlock (..)
    , Output
    , OutputReference
    , Point
    , PolicyId
    , Script
    , ScriptHash
    , ScriptReference (..)
    , SlotNo
    , TransactionId
    , Value
    , addressFromBytes
    , addressToBytes
    , addressToJson
    , assetNameFromText
    , assetNameToText
    , datumHashToJson
    , digest
    , digestSize
    , getAddress
    , getDatum
    , getDelegationPartBytes
    , getOutputIndex
    , getPaymentPartBytes
    , getPointSlotNo
    , getScript
    , getTransactionId
    , getValue
    , hasAssetId
    , hasPolicyId
    , hashDatum
    , hashScriptReference
    , headerHashToJson
    , isBootstrap
    , mkOutputReference
    , mkScriptReference
    , outputIndexFromText
    , outputIndexToJson
    , outputReferenceToText
    , policyIdFromText
    , policyIdToText
    , scriptHashToJson
    , slotNoToJson
    , transactionIdFromText
    , transactionIdToJson
    , transactionIdToText
    , unsafeGetPointHeaderHash
    , valueToJson
    )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Aeson.Encoding as Json
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

data Pattern
    = MatchAny !MatchBootstrap
    | MatchExact !Address
    | MatchPayment !ByteString
    | MatchDelegation !ByteString
    | MatchPaymentAndDelegation !ByteString !ByteString
    | MatchTransactionId !TransactionId
    | MatchOutputReference !OutputReference
    | MatchPolicyId !PolicyId
    | MatchAssetId !AssetId
    deriving (Generic, Eq, Ord, Show)

-- | Checks whether a given pattern overlaps with a list of other patterns. The
-- inclusion is a deep inclusion such that for instance, (MatchPayment "a") is
-- considered included in [MatchAny OnlyShelley].
overlaps :: Pattern -> Set Pattern -> Bool
overlaps p =
    Set.foldr (\p' rest -> rest || overlapTwo (p, p') || overlapTwo (p', p)) False
  where
    overlapTwo = \case
        (MatchAny{}, _) ->
            True
        (MatchExact addr, p') ->
            p' `matchingAddress` addr
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
        (MatchOutputReference a, MatchOutputReference a') ->
            a == a'
        (MatchOutputReference a, MatchTransactionId a') ->
            getTransactionId a == a'
        (MatchTransactionId a, MatchTransactionId a') ->
            a == a'
        (MatchPolicyId a, MatchPolicyId a') ->
            a == a'
        (MatchAssetId a, MatchAssetId a') ->
            a == a'
        (MatchAssetId a, MatchPolicyId a') ->
            fst a == a'
        _nonOverlappingPatterns ->
            False

-- | Checks whether a pattern x fully includes pattern y.
-- If any result matched by y is also matched by x, then the function returns
-- 'True'. Otherwise it returns 'False'. The converse may not be true:
-- x may match more results than y.
includes :: Pattern -> Pattern -> Bool
includes x y = case (x, y) of
    (p, MatchExact addr) ->
        p `matchingAddress` addr
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
    (MatchOutputReference a, MatchOutputReference a') ->
        a == a'
    (MatchOutputReference a, MatchTransactionId a') ->
        getTransactionId a == a'
    (MatchTransactionId a, MatchTransactionId a') ->
        a == a'
    _nonIncludedPatterns ->
        False

-- | All patterns in the list that are fully covering the given pattern
-- according to the definition given by 'includes'
included :: Pattern -> Set Pattern -> Set Pattern
included p = Set.filter (`includes` p)

wildcard :: Text
wildcard = "*"
{-# INLINEABLE wildcard #-}

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
    MatchTransactionId txId ->
        wildcard <> "@" <> transactionIdToText txId
    MatchOutputReference outRef ->
        outputReferenceToText outRef
    MatchPolicyId policyId ->
        policyIdToText policyId <> "." <> wildcard
    MatchAssetId (policyId, name) ->
        policyIdToText policyId <> "." <> assetNameToText name

patternFromText :: Text -> Maybe Pattern
patternFromText txt = asum
    [ readerAny
    , readerExact
    , readerPaymentOrDelegation
    , readerOutputReference
    , readerAssetId
    ]
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
            _unexpectedSplit ->
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

    readerOutputReference = do
        case T.splitOn "@" txt of
            [star, txId] | star == wildcard ->
                MatchTransactionId <$> transactionIdFromText txId
            [ix, txId] -> do
                outRef <- mkOutputReference
                    <$> transactionIdFromText txId
                    <*> outputIndexFromText ix
                pure (MatchOutputReference outRef)
            _unexpectedSplit ->
                Nothing

    readerAssetId = do
        case T.splitOn "." txt of
            [policyId, star] | star == wildcard ->
                MatchPolicyId <$> policyIdFromText policyId
            [policyId, name] -> do
                assetId <- (,)
                    <$> policyIdFromText policyId
                    <*> assetNameFromText name
                pure (MatchAssetId assetId)
            _unexpectedSplit ->
                Nothing

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

matching :: (Alternative f) => OutputReference -> Output -> Pattern -> f ()
matching outRef out = \case
    MatchTransactionId txId ->
        guard (txId == getTransactionId outRef)
    MatchOutputReference outRef' ->
        guard (outRef == outRef')
    MatchPolicyId policyId ->
        guard (hasPolicyId (getValue out) policyId)
    MatchAssetId assetId ->
        guard (hasAssetId (getValue out) assetId)
    other ->
        guard (matchingAddress other (getAddress out))

matchingAddress :: Pattern -> Address -> Bool
matchingAddress = \case
    MatchAny (MatchBootstrap matchBootstrap) ->
        \addr ->
            not (isBootstrap addr) || matchBootstrap
    MatchExact addr' ->
        (== addr')
    MatchPayment payment ->
        \addr ->
            Just payment == getPaymentPartBytes addr
    MatchDelegation delegation ->
        \addr ->
            Just delegation == getDelegationPartBytes addr
    MatchPaymentAndDelegation payment delegation -> do
        \addr ->
            Just payment == getPaymentPartBytes addr
            &&
            Just delegation == getDelegationPartBytes addr
    _nonAddressPattern ->
        const False

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
    { outputReference :: !OutputReference
    , address :: !Address
    , value :: !Value
    , datum :: !Datum
    , scriptReference :: !ScriptReference
    , createdAt :: !Point
    , spentAt :: !(Maybe Point)
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
        (maybe Json.null_ datumHashToJson (hashDatum datum))
    , Json.pair "script_hash"
        (maybe Json.null_ scriptHashToJson (hashScriptReference scriptReference))
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

-- | Codecs to encode data-type to some target structure. This allows to encode
-- on-the-fly as we traverse the structure, rather than traversing all results a
-- second time.
data Codecs result slotNo input bin script = Codecs
    { toResult :: !(Result -> result)
    , toSlotNo :: !(SlotNo -> slotNo)
    , toInput :: !(Input -> input)
    , toBinaryData :: !(DatumHash -> BinaryData -> bin)
    , toScript :: !(ScriptHash -> Script -> script)
    }

-- | A higher-level record to represent the aggregation of matched results.
data Match result slotNo input bin script = Match
    { consumed :: !(Map slotNo (Set input))
    , produced :: ![result]
    , datums :: ![bin]
    , scripts :: ![script]
    }

instance Ord slotNo => Semigroup (Match result slotNo input bin script) where
    a <> b = Match
        { consumed = consumed a <> consumed b
        , produced = produced a <> produced b
        , datums = datums a <> datums b
        , scripts = scripts a <> scripts b
        }

instance Ord slotNo => Monoid (Match result slotNo input bin script) where
    mempty = Match mempty mempty mempty mempty


-- | Match all outputs in transactions from a block that match any of the given
-- pattern.
--
-- Note that this may yield multiple time the same result if being matched by
-- multiple patterns. This is to facilitate building an index of matches to
-- results.
matchBlock
    :: forall block result slotNo input bin script.
        ( IsBlock block
        , Ord input
        , Ord slotNo
        )
    => Codecs result slotNo input bin script
    -> Set Pattern
    -> block
    -> Match result slotNo input bin script
matchBlock Codecs{..} patterns blk =
    let pt = getPoint blk in foldBlock (fn pt) (Match mempty mempty mempty mempty) blk
  where
    fn
        :: Point
        -> BlockBody block
        -> Match result slotNo input bin script
        -> Match result slotNo input bin script
    fn pt tx Match{consumed, produced, datums, scripts} = Match
        { consumed = Map.alter
            (\st -> Just $ Set.foldr
                (Set.insert . toInput)
                (fromMaybe mempty st)
                (spentInputs @block tx)
            )
            (toSlotNo (getPointSlotNo pt))
            consumed

        , produced =
            matched ++ produced

        , datums = Map.foldrWithKey
            (\k v accum -> toBinaryData k v : accum)
            datums
            (witnessedDatums @block tx)

        , scripts = Map.foldrWithKey
            (\k v accum -> toScript k v : accum)
            scripts
            (witnessedScripts @block tx)
        }
      where
        matched =
            concatMap
                (flip (mapMaybeOutputs @block) tx . match pt)
                patterns

    match
        :: Point
        -> Pattern
        -> OutputReference
        -> Output
        -> Maybe result
    match pt m outputReference output = do
        matching outputReference output m
        pure $ toResult Result
            { outputReference
            , address = getAddress output
            , value = getValue output
            , datum = getDatum output
            , scriptReference = mkScriptReference (getScript output)
            , createdAt = pt
            , spentAt = Nothing
            }
