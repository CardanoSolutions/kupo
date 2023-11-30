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
addressFromBytes bytes =
    Ledger.deserialiseAddr bytes
  <|>
    -- NOTE: Since around Babbage / Conway, the ledger team decided to drop support for pointer
    -- addresses from new decoders. Yet, those addresses still exist on-chain (and forever will).
    --
    -- See also: https://github.com/input-output-hk/cardano-ledger/issues/3898 for details.
    --
    -- In principle, the left side of the following alternative is sufficient given that there hasn't
    -- been any new type of addresses since the launch of Shelley. The Alonzo decoder is just more
    -- lenient than the more recent ones. The right side of the alternative thus exist as a safety net
    -- in case new types of addresses gets added. There's a high chance that I'll just forget to
    -- update that function here if that happens.
    eitherToMaybe (decodeCbor @AlonzoEra "Address" Ledger.fromCborAddr (toLazy (serializeCbor @AlonzoEra encCBOR bytes)))
  <|>
    eitherToMaybe (decodeCborLatest "Address" Ledger.fromCborAddr (toLazy (serializeCborLatest encCBOR bytes)))
  where
    eitherToMaybe = either (const Nothing) pure
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

