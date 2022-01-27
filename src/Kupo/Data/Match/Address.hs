--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.Match.Address
    ( Match (..)
    , matching
    , Result (..)
    , matchBlock
    ) where

import Kupo.Prelude

import Kupo.Data.ChainSync
    ( Address
    , Block
    , Crypto
    , DatumHash
    , Output
    , OutputReference
    , Value
    , foldBlock
    , getAddress
    , getDatumHash
    , getDelegationPartBytes
    , getPaymentPartBytes
    , getValue
    , mapMaybeOutputs
    , serialiseAddress
    )

data Match
    = MatchAny
    | MatchPaymentPart ByteString
    | MatchDelegationPart ByteString
    | MatchExact ByteString
    deriving Show

matching :: Alternative f => Address crypto -> Match -> f ()
matching addr = guard . \case
    MatchAny -> True
    MatchPaymentPart bytes -> bytes == getPaymentPartBytes addr
    MatchDelegationPart bytes -> Just bytes == getDelegationPartBytes addr
    MatchExact bytes -> bytes == serialiseAddress addr

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
matchBlock :: Crypto crypto => [Match] -> Block crypto -> [(Match, Result crypto)]
matchBlock ms = flip foldBlock [] $ \tx result ->
    concatMap (flip mapMaybeOutputs tx . mkMatch) ms ++ result
  where
    mkMatch
        :: Match
        -> OutputReference crypto
        -> Output crypto
        -> Maybe (Match, Result crypto)
    mkMatch m reference out = do
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
