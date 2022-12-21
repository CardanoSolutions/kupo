module Kupo.Data.Cardano.AssetName where

import Kupo.Prelude

import Ouroboros.Consensus.Util
    ( eitherToMaybe
    )

import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Data.ByteString as BS

type AssetName = Ledger.AssetName

assetNameMaxLength :: Int
assetNameMaxLength = 32
{-# INLINABLE assetNameMaxLength #-}

unsafeAssetNameFromBytes :: HasCallStack => ByteString -> Ledger.AssetName
unsafeAssetNameFromBytes bytes
  | BS.length bytes > assetNameMaxLength =
      error $ "tried to construct an asset name that is too long, from: " <> encodeBase16 bytes
  | otherwise =
    Ledger.AssetName (toShort bytes)
{-# INLINABLE unsafeAssetNameFromBytes #-}

assetNameFromText :: Text -> Maybe AssetName
assetNameFromText (encodeUtf8 -> bytes) = do
    -- NOTE: assuming base16 encoding, hence '2 *'
    guard (BS.length bytes <= 2 * assetNameMaxLength)
    unsafeAssetNameFromBytes <$> eitherToMaybe (decodeBase16 bytes)

assetNameToText :: AssetName -> Text
assetNameToText =
    encodeBase16 . fromShort . Ledger.assetName
{-# INLINABLE assetNameToText #-}
