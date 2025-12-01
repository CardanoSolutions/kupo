module Kupo.Data.Cardano.PolicyId where

import Kupo.Prelude

import qualified Cardano.Ledger.Mary.Value as Ledger
import Kupo.Data.Cardano.ScriptHash
    ( scriptHashFromBytes
    , scriptHashFromText
    , scriptHashToBytes
    , scriptHashToText
    )

type PolicyId = Ledger.PolicyID

policyIdToBytes :: PolicyId -> ByteString
policyIdToBytes (Ledger.PolicyID h) =
    scriptHashToBytes h
{-# INLINABLE policyIdToBytes #-}

unsafePolicyIdFromBytes :: ByteString -> PolicyId
unsafePolicyIdFromBytes =
    maybe (error "unsafePolicyIdFromBytes") Ledger.PolicyID . scriptHashFromBytes
{-# INLINABLE unsafePolicyIdFromBytes #-}

policyIdFromText :: Text -> Maybe PolicyId
policyIdFromText =
    fmap Ledger.PolicyID . scriptHashFromText
{-# INLINABLE policyIdFromText #-}

policyIdToText :: PolicyId -> Text
policyIdToText =
    scriptHashToText . Ledger.policyID
{-# INLINABLE policyIdToText #-}
