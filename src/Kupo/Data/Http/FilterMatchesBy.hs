--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.Http.FilterMatchesBy
    ( FilterMatchesBy (..)
    , filterMatchesBy
    ) where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( AssetId, PolicyId, assetNameFromText, policyIdFromText, TransactionId, OutputReference, mkOutputReference, outputIndexFromText, transactionIdFromText )

import qualified Network.HTTP.Types as Http

data FilterMatchesBy
    = FilterByAssetId AssetId
    | FilterByPolicyId PolicyId
    | FilterByTransactionId TransactionId
    | FilterByOutputReference OutputReference
    | NoFilter
    deriving (Eq, Show, Generic)

-- | Creates a 'FilterMatchesBy' from query parameters. This search for zero,
-- one or two query params:
--
-- - 'asset_name'
-- - 'policy_id'
--
-- The latter can be specified alone, whereas the former can only be specified
-- if 'policy_id' is also present.
--
-- For examples:
--
-- ✓ ?policy_id=123
-- ✓ ?foo=bar&policy_id=123
-- ✓ ?policy_id=123&asset_name=456
-- ✓ ?asset_name=456&policy_id=123
-- x ?asset_name=456
-- x ?policy_id=123&policy_id=456
-- x ?policy_id=123&asset_name=456&asset_name=456
--
filterMatchesBy
    :: Http.Query
    -> Maybe FilterMatchesBy
filterMatchesBy = search Nothing
  where
    search byPolicyId = \case
        [] ->
            byPolicyId <|> Just NoFilter
        ("policy_id", Just bytes):rest -> do
            str <- either (const Nothing) pure (decodeUtf8' bytes)
            guard (isNothing byPolicyId)
            policyId <- policyIdFromText str
            search (Just (FilterByPolicyId policyId)) rest
        ("asset_name", Just bytes):rest -> do
            str <- either (const Nothing) pure (decodeUtf8' bytes)
            policyId <- case search byPolicyId rest of
                Just (FilterByPolicyId policyId) ->
                    Just policyId
                _ ->
                    Nothing
            assetName <- assetNameFromText str
            pure $ FilterByAssetId (policyId, assetName)
        ("output_index", Just bytes):rest -> do
            str <- either (const Nothing) pure (decodeUtf8' bytes)
            guard (isNothing byPolicyId)
            txId <- case search byPolicyId rest of
                Just (FilterByTransactionId tId) ->
                    Just tId
                _ ->
                    Nothing
            outputIndex <- outputIndexFromText str
            pure $ FilterByOutputReference (mkOutputReference txId outputIndex)
        ("transaction_id", Just bytes):rest -> do
            str <- either (const Nothing) pure (decodeUtf8' bytes)
            guard (isNothing byPolicyId)
            outputRef <- transactionIdFromText str
            search (Just (FilterByTransactionId outputRef)) rest
        _:rest ->
            search byPolicyId rest
