--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.Http.FilterMatchesBy
    ( FilterMatchesBy (..)
    , filterMatchesBy
    ) where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( AssetId
    , OutputReference
    , PolicyId
    , TransactionId
    , assetNameFromText
    , mkOutputReference
    , outputIndexFromText
    , policyIdFromText
    , transactionIdFromText
    )

import qualified Network.HTTP.Types as Http

data FilterMatchesBy
    = FilterByAssetId !AssetId
    | FilterByPolicyId !PolicyId
    | FilterByTransactionId !TransactionId
    | FilterByTopLevelTransactionId !TransactionId
    | FilterByOutputReference !OutputReference
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
-- 'top_level_transaction_id' selects the outputs of the sub-transactions of a
-- transaction. It cannot be combined with 'policy_id', 'transaction_id' or
-- 'output_index'.
--
-- For examples:
--
-- ✓ ?policy_id=123
-- ✓ ?foo=bar&policy_id=123
-- ✓ ?policy_id=123&asset_name=456
-- ✓ ?asset_name=456&policy_id=123
-- ✓ ?transaction_id=d0ae0c50209357e1356333b10d3a59ff4732e493f06c0ed9e82d1beacdd0343f
-- ✓ ?foo=bar&transaction_id=d0ae0c50209357e1356333b10d3a59ff4732e493f06c0ed9e82d1beacdd0343f
-- ✓ ?transaction_id=d0ae0c50209357e1356333b10d3a59ff4732e493f06c0ed9e82d1beacdd0343f&output_index=14
-- ✓ ?top_level_transaction_id=d0ae0c50209357e1356333b10d3a59ff4732e493f06c0ed9e82d1beacdd0343f
--
-- x ?asset_name=456
-- x ?policy_id=123&policy_id=456
-- x ?policy_id=123&asset_name=456&asset_name=456
-- x ?transaction_id=d0ae0c50209357e1356333b10d3a59ff4732e493f06c0ed9e82d1beacdd0343f&transaction_id=8beca7a84b0ed6d6c22d5f0edb8964a48b9566db20ef2a42c5ae609553a3fd89
-- x ?transaction_id=d0ae0c50209357e1356333b10d3a59ff4732e493f06c0ed9e82d1beacdd0343f&output_index=14&output_index=42
-- x ?output_index=14
-- x ?policy_id=d0ae0c50209357e1356333b10d3a59ff4732e493f06c0ed9e82d1bea&transaction_id=d0ae0c50209357e1356333b10d3a59ff4732e493f06c0ed9e82d1beacdd0343f
-- x ?transaction_id=d0ae0c50209357e1356333b10d3a59ff4732e493f06c0ed9e82d1beacdd0343f&top_level_transaction_id=8beca7a84b0ed6d6c22d5f0edb8964a48b9566db20ef2a42c5ae609553a3fd89
-- x ?top_level_transaction_id=d0ae0c50209357e1356333b10d3a59ff4732e493f06c0ed9e82d1beacdd0343f&output_index=14
--
filterMatchesBy
    :: Http.Query
    -> Maybe FilterMatchesBy
filterMatchesBy = search Nothing Nothing Nothing
  where
    search byPolicyId byTransactionId byTopLevelTransactionId = \case
        [] ->
            case catMaybes [byPolicyId, byTransactionId, byTopLevelTransactionId] of
                []  -> Just NoFilter
                [f] -> Just f
                _   -> Nothing
        ("policy_id", Just bytes):rest -> do
            str <- either (const Nothing) pure (decodeUtf8' bytes)
            guard (isNothing byPolicyId)
            policyId <- policyIdFromText str
            search (Just (FilterByPolicyId policyId)) byTransactionId byTopLevelTransactionId rest
        ("asset_name", Just bytes):rest -> do
            str <- either (const Nothing) pure (decodeUtf8' bytes)
            policyId <- case search byPolicyId byTransactionId byTopLevelTransactionId rest of
                Just (FilterByPolicyId policyId) ->
                    Just policyId
                _noPolicyId ->
                    Nothing
            assetName <- assetNameFromText str
            pure $ FilterByAssetId (policyId, assetName)
        ("output_index", Just bytes):rest -> do
            str <- either (const Nothing) pure (decodeUtf8' bytes)
            txId <- case search byPolicyId byTransactionId byTopLevelTransactionId rest of
                Just (FilterByTransactionId tId) ->
                    Just tId
                _noTransactionId ->
                    Nothing
            outputIndex <- outputIndexFromText str
            pure $ FilterByOutputReference (mkOutputReference txId outputIndex)
        ("transaction_id", Just bytes):rest -> do
            str <- either (const Nothing) pure (decodeUtf8' bytes)
            guard (isNothing byTransactionId)
            outputRef <- transactionIdFromText str
            search byPolicyId (Just (FilterByTransactionId outputRef)) byTopLevelTransactionId rest
        ("top_level_transaction_id", Just bytes):rest -> do
            str <- either (const Nothing) pure (decodeUtf8' bytes)
            guard (isNothing byTopLevelTransactionId)
            transactionId <- transactionIdFromText str
            search byPolicyId byTransactionId (Just (FilterByTopLevelTransactionId transactionId)) rest
        _:rest ->
            search byPolicyId byTransactionId byTopLevelTransactionId rest
