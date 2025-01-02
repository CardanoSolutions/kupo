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
    , Datum (..)
    , DatumHash
    , ExtendedOutputReference
    , InputIndex
    , IsBlock (..)
    , Metadata
    , Output
    , OutputReference
    , Point
    , PolicyId
    , Script
    , ScriptHash
    , ScriptReference (..)
    , SlotNo
    , TransactionId
    , TransactionIndex
    , Value
    , addressFromBytes
    , addressToBytes
    , addressToJson
    , assetNameFromText
    , assetNameToText
    , binaryDataToJson
    , datumHashToJson
    , foldrValue
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
    , hasMetadataTag
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
    , transactionIndexToJson
    , unsafeGetPointHeaderHash
    , valueToJson
    )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Aeson.Encoding as Json
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Read as T

data Pattern
    = MatchAny !MatchBootstrap
        -- ^ Wildcard patterns, but allow filtering out outputs associated to a bootstrap addresses.
    | MatchExact !Address
        -- ^ Match only outputs whose address is a given one, including its network discriminant.
    | MatchPayment !ByteString
        -- ^ Match only outputs whose address has a specific payment part.
    | MatchDelegation !ByteString
        -- ^ Match only outputs whose address has a specific delegation part.
    | MatchPaymentAndDelegation !ByteString !ByteString
        -- ^ Match only outputs whose address has the specific payment and delegation part (in this
        -- order). This is roughly equivalent to 'MatchExact' but only works for type XX, YY, ZZ
        -- addresses and ignore the network discriminant.
    | MatchTransactionId !TransactionId
        -- ^ Match only outputs parts of a given transaction, referenced by its id.
    | MatchOutputReference !OutputReference
        -- ^ Match only a specific output of a transaction, referenced by its full output reference.
    | MatchPolicyId !PolicyId
        -- ^ Match only outputs that carry any positive number of tokens of a given policy.
    | MatchAssetId !AssetId
        -- ^ Match only outputs that carry any positive number of tokens of a given asset.
    | MatchMetadataTag !Word64
        -- ^ Match only outputs of transactions that have a specific metadata tag.
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
        (MatchPolicyId a, MatchAssetId (a', _b')) ->
            a == a'
        (MatchAssetId a, MatchAssetId a') ->
            a == a'
        (MatchMetadataTag a, MatchMetadataTag a') ->
            a == a'
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
    (MatchPolicyId a, MatchPolicyId a') ->
        a == a'
    (MatchAssetId a, MatchAssetId a') ->
        a == a'
    (MatchPolicyId a, MatchAssetId (a', _b')) ->
        a == a'
    (MatchMetadataTag a, MatchMetadataTag a') ->
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
    MatchMetadataTag tag ->
        "{" <> show tag <> "}"

patternFromText :: Text -> Maybe Pattern
patternFromText txt = asum
    [ readerAny
    , readerExact
    , readerPaymentOrDelegation
    , readerOutputReference
    , readerAssetId
    , readerMetadataTag
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

    readerMetadataTag = do
        case concatMap (T.splitOn "{") (T.splitOn "}" txt) of
            ["", str, ""] -> do
                (tag, remTag) <- either (const Nothing) Just (T.decimal str)
                guard (T.null remTag)
                pure (MatchMetadataTag tag)
            _ ->
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

matching :: (Alternative f) => OutputReference -> Output -> Metadata -> Pattern -> f ()
matching outRef out metadata = \case
    MatchTransactionId txId ->
        guard (txId == getTransactionId outRef)
    MatchOutputReference outRef' ->
        guard (outRef == outRef')
    MatchPolicyId policyId ->
        guard (hasPolicyId (getValue out) policyId)
    MatchAssetId assetId ->
        guard (hasAssetId (getValue out) assetId)
    MatchMetadataTag tag ->
        guard (hasMetadataTag tag metadata)
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
    { outputReference :: !ExtendedOutputReference
    , address :: !Address
    , value :: !Value
    , datum :: !Datum
    , scriptReference :: !ScriptReference
    , createdAt :: !Point
    , spentAt :: !(Maybe Point)
    , spentBy :: !(Maybe OutputReference)
    , spentWith :: !(Maybe BinaryData)
    } deriving (Show, Eq)

resultToJson
    :: Result
    -> Json.Encoding
resultToJson Result{..} = Json.pairs $ mconcat
    [ Json.pair "transaction_index"
        (transactionIndexToJson (snd outputReference))
    , Json.pair "transaction_id"
        (transactionIdToJson (getTransactionId $ fst outputReference))
    , Json.pair "output_index"
        (outputIndexToJson (getOutputIndex $ fst outputReference))
    , Json.pair "address"
        (addressToJson address)
    , Json.pair "value"
        (valueToJson value)
    , Json.pair "datum_hash"
        (maybe Json.null_ datumHashToJson (hashDatum datum))
    , case datum of
        NoDatum ->
            mempty
        Inline{} ->
            Json.pair "datum_type" (Json.text "inline")
        Reference{} ->
            Json.pair "datum_type" (Json.text "hash")
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
                    , Json.pair "transaction_id"
                        (maybe Json.null_ (transactionIdToJson . getTransactionId) spentBy)
                    , Json.pair "input_index"
                        (maybe Json.null_ (outputIndexToJson . getOutputIndex) spentBy)
                    , Json.pair "redeemer"
                        (maybe Json.null_ binaryDataToJson spentWith)
                    ]
        )
    ]

-- | Codecs to encode data-type to some target structure. This allows to encode
-- on-the-fly as we traverse the structure, rather than traversing all results a
-- second time.
data Codecs result bin script policy = Codecs
    { toResult :: !(Result -> result)
    , toBinaryData :: !(DatumHash -> BinaryData -> bin)
    , toScript :: !(ScriptHash -> Script -> script)
    , toPolicy :: !(OutputReference -> PolicyId -> policy)
    }

-- | A higher-level record to represent the aggregation of matched results.
data Match result bin script policy = Match
    { consumed :: !(Map (TransactionId, SlotNo) [(Pattern, InputIndex, Maybe BinaryData)])
    , produced :: ![result]
    , datums :: ![bin]
    , scripts :: ![script]
    , policies :: !(Set policy)
    }

instance (Ord policy) => Semigroup (Match result bin script policy) where
    a <> b = Match
        { consumed = consumed a <> consumed b
        , produced = produced a <> produced b
        , datums = datums a <> datums b
        , scripts = scripts a <> scripts b
        , policies = policies a <> policies b
        }

instance (Ord policy) => Monoid (Match result bin script policy) where
    mempty = Match mempty mempty mempty mempty mempty


-- | Match all outputs in transactions from a block that match any of the given
-- pattern.
--
-- Note that this may yield multiple time the same result if being matched by
-- multiple patterns. This is to facilitate building an index of matches to
-- results.
matchBlock
    :: forall block result bin script policy.
        ( IsBlock block
        , Ord policy
        )
    => Codecs result bin script policy
    -> Set Pattern
    -> block
    -> Match result bin script policy
matchBlock Codecs{..} patterns blk =
    let pt = getPoint blk in foldBlock (fn pt) mempty blk
  where
    fn
        :: Point
        -> TransactionIndex
        -> BlockBody block
        -> Match result bin script policy
        -> Match result bin script policy
    fn pt ix tx Match{consumed, produced, datums, scripts, policies} = Match
        { consumed = Map.alter
            (\st -> Just $ fst $ foldr
                -- FIXME: Replace 'Nothing' here with redeemer pulled from the transaction
                (\ref (refs, i) -> ((MatchOutputReference ref, i, Nothing):refs, i + 1))
                (fromMaybe mempty st, 0)
                (spentInputs @block tx)
            )
            (getTransactionId tx, getPointSlotNo pt)
            consumed

        , produced =
            newProduced

        , datums = Map.foldrWithKey
            (\k v accum -> toBinaryData k v : accum)
            datums
            (witnessedDatums @block tx)

        , scripts = Map.foldrWithKey
            (\k v accum -> toScript k v : accum)
            scripts
            (witnessedScripts @block tx)

        , policies =
            newPolicies
        }
      where
        (newProduced, newPolicies) =
            foldr
                (\pattern_ ->
                    flip
                        (foldr (\(r, p) (rs, ps) -> (r:rs, Set.union p ps)))
                        (mapMaybeOutputs @block (match pt ix pattern_) tx)
                )
                (produced, policies)
                patterns

    match
        :: Point
        -> TransactionIndex
        -> Pattern
        -> OutputReference
        -> Output
        -> Metadata
        -> Maybe (result, Set policy)
    match pt ix m outRef output metadata = do
        matching outRef output metadata m $>
            ( toResult Result
                { outputReference = (outRef, ix)
                , address = getAddress output
                , value = getValue output
                , datum = getDatum output
                , scriptReference = mkScriptReference (getScript output)
                , createdAt = pt
                , spentAt = Nothing
                , spentBy = Nothing
                , spentWith = Nothing
                }
            , foldrValue
                (\policy _ -> Set.insert (toPolicy outRef policy))
                Set.empty
                (getValue output)
            )
