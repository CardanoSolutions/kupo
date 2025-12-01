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

import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Key as Json
import qualified Data.ByteString.Short as SBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy.Builder as Builder

type Value =
    Ledger.MaryValue

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
        (Ledger.Coin ada)
        [ ( unsafePolicyIdFromBytes pid, unsafeAssetNameFromBytes name, q)
        | (pid, name, q) <- assets
        ]

valueToJson :: Value -> Json.Encoding
valueToJson (Ledger.MaryValue (Ledger.Coin coins) (Ledger.MultiAsset assets)) =
    Json.pairs $
        Json.pair "coins"  (Json.integer coins)
      <>
        Json.pair "assets" (assetsToJson assets)
  where
    shortBytesToKey =
        Builder.fromText . encodeBase16 . fromShort

    assetsToJson
        :: Map PolicyId (Map AssetName Integer)
        -> Json.Encoding
    assetsToJson =
        Json.pairs
        .
        Map.foldrWithKey
            (\(Ledger.PolicyID (Ledger.ScriptHash (UnsafeHash policyId))) inner r ->
                let fieldName = shortBytesToKey policyId
                 in r <> Map.foldrWithKey
                        (\(Ledger.AssetName assetName) quantity json ->
                            let
                                k = ( if SBS.null assetName then
                                        fieldName
                                      else
                                        fieldName <> "." <> shortBytesToKey assetName
                                    ) & Json.fromText . toStrict . Builder.toLazyText

                                v = Json.integer quantity
                             in
                                Json.pair k v <> json
                        )
                        (mempty :: Json.Series)
                        inner
            )
            mempty


-- ComparableValue

data ComparableValue = ComparableValue
    { comparableValueAda :: !Integer
    , comparableValueAssets :: !(Map PolicyId (Map AssetName Integer))
    } deriving (Generic, Eq, Show, Ord)

fromComparableValue :: ComparableValue -> Value
fromComparableValue (ComparableValue ada assets) =
    Ledger.MaryValue (Ledger.Coin ada) (Ledger.MultiAsset assets)

toComparableValue :: Value -> ComparableValue
toComparableValue (Ledger.MaryValue (Ledger.Coin ada) (Ledger.MultiAsset assets)) =
    ComparableValue ada assets
