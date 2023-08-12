module Kupo.Data.Cardano.Value where
import Kupo.Prelude

import Kupo.Data.Cardano.AssetId
    ( AssetId
    )
import Kupo.Data.Cardano.AssetName
    ( AssetName
    , unsafeAssetNameFromBytes
    )
import Kupo.Data.Cardano.PolicyId
    ( PolicyId
    , unsafePolicyIdFromBytes
    )

import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Key as Json
import qualified Data.ByteString.Short as SBS
import qualified Data.Map as Map
import qualified Data.Set as Set

type Value =
    Value' StandardCrypto

type Value' crypto =
    Ledger.MaryValue crypto

foldrValue
    :: (PolicyId -> Map AssetName Integer -> a -> a)
    -> a
    -> Value
    -> a
foldrValue fn a0 (Ledger.MaryValue _ (Ledger.MultiAsset assets)) =
    Map.foldrWithKey fn a0 assets

hasAssetId :: Value -> AssetId -> Bool
hasAssetId value (policyId, assetName) =
    Ledger.lookupMultiAsset policyId assetName value > 0

hasPolicyId :: Value -> PolicyId -> Bool
hasPolicyId (Ledger.MaryValue _ assets) policyId =
    policyId `Set.member` Ledger.policies assets

unsafeValueFromList
    :: Integer
    -> [(ByteString, ByteString, Integer)]
    -> Value
unsafeValueFromList ada assets =
    Ledger.valueFromList
        ada
        [ ( unsafePolicyIdFromBytes pid, unsafeAssetNameFromBytes name, q)
        | (pid, name, q) <- assets
        ]

valueToJson :: Value -> Json.Encoding
valueToJson (Ledger.MaryValue coins (Ledger.MultiAsset assets)) = Json.pairs $ mconcat
    [ Json.pair "coins"  (Json.integer coins)
    , Json.pair "assets" (assetsToJson assets)
    ]
  where
    assetsToJson
        :: Map (Ledger.PolicyID StandardCrypto) (Map Ledger.AssetName Integer)
        -> Json.Encoding
    assetsToJson =
        Json.pairs
        .
        Map.foldrWithKey
            (\k v r -> Json.pair (assetIdToKey k) (Json.integer v) <> r)
            mempty
        .
        flatten

    flatten :: (Ord k1, Ord k2) => Map k1 (Map k2 a) -> Map (k1, k2) a
    flatten = Map.foldrWithKey
        (\k inner -> Map.union (Map.mapKeys (k,) inner))
        mempty

    assetIdToKey :: (Ledger.PolicyID StandardCrypto, Ledger.AssetName) -> Json.Key
    assetIdToKey (Ledger.PolicyID (Ledger.ScriptHash (UnsafeHash pid)), Ledger.AssetName bytes)
        | SBS.null bytes = Json.fromText
            (encodeBase16 (fromShort pid))
        | otherwise     = Json.fromText
            (encodeBase16 (fromShort pid) <> "." <> encodeBase16 (fromShort bytes))

-- ComparableValue

data ComparableValue = ComparableValue
    { comparableValueAda :: !Integer
    , comparableValueAssets :: !(Map PolicyId (Map AssetName Integer))
    } deriving (Generic, Eq, Show, Ord)

fromComparableValue :: ComparableValue -> Value
fromComparableValue (ComparableValue ada assets) =
    Ledger.MaryValue ada (Ledger.MultiAsset assets)

toComparableValue :: Value -> ComparableValue
toComparableValue (Ledger.MaryValue ada (Ledger.MultiAsset assets)) =
    ComparableValue ada assets
