module Kupo.Data.Cardano.Address where

import Kupo.Prelude

import Data.Binary.Put
    ( runPut
    )
import Data.ByteString.Bech32
    ( HumanReadablePart (..)
    , encodeBech32
    )

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json

-- Address

type Address =
    Address' StandardCrypto

type Address' crypto =
    Ledger.Addr crypto

addressToJson :: Address -> Json.Encoding
addressToJson = \case
    addr@Ledger.AddrBootstrap{} ->
        (Json.text . encodeBase58 . addressToBytes) addr
    addr@(Ledger.Addr network _ _) ->
        (Json.text . encodeBech32 (hrp network) . addressToBytes) addr
  where
    hrp = \case
        Ledger.Mainnet -> HumanReadablePart "addr"
        Ledger.Testnet -> HumanReadablePart "addr_test"

addressToBytes :: Address -> ByteString
addressToBytes =
    Ledger.serialiseAddr
{-# INLINABLE addressToBytes #-}

unsafeAddressFromBytes :: HasCallStack => ByteString -> Address
unsafeAddressFromBytes =
    fromMaybe (error "unsafeAddressFromBytes") . addressFromBytes
{-# INLINABLE unsafeAddressFromBytes #-}

addressFromBytes :: ByteString -> Maybe Address
addressFromBytes =
    Ledger.deserialiseAddr
{-# INLINABLE addressFromBytes #-}

isBootstrap :: Address -> Bool
isBootstrap = \case
    Ledger.AddrBootstrap{} -> True
    Ledger.Addr{} -> False

getPaymentPartBytes :: Address -> Maybe ByteString
getPaymentPartBytes = \case
    Ledger.Addr _ payment _ ->
        Just $ toStrict $ runPut $ Ledger.putCredential payment
    Ledger.AddrBootstrap{} ->
        Nothing

getDelegationPartBytes :: Address -> Maybe ByteString
getDelegationPartBytes = \case
    Ledger.Addr _ _ (Ledger.StakeRefBase delegation) ->
        Just $ toStrict $ runPut $ Ledger.putCredential delegation
    Ledger.Addr{} ->
        Nothing
    Ledger.AddrBootstrap{} ->
        Nothing

