module Kupo.Data.Cardano.AssetId where

import Kupo.Data.Cardano.AssetName
    ( AssetName
    )
import Kupo.Data.Cardano.PolicyId
    ( PolicyId
    )

type AssetId = (PolicyId, AssetName)
