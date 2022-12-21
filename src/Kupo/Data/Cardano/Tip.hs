{-# LANGUAGE PatternSynonyms #-}

module Kupo.Data.Cardano.Tip  where

import Kupo.Prelude

import Cardano.Slotting.Block
    ( BlockNo (..)
    )
import Kupo.Data.Cardano.Block
    ( Block
    )
import Kupo.Data.Cardano.HeaderHash
    ( HeaderHash
    )
import Kupo.Data.Cardano.SlotNo
    ( SlotNo (..)
    , WithOrigin (..)
    , distanceToSlot
    )

import qualified Ouroboros.Network.Block as Ouroboros

type Tip = Ouroboros.Tip Block
{-# COMPLETE GenesisTip, Tip #-}

pattern GenesisTip :: Tip
pattern GenesisTip <-
    Ouroboros.TipGenesis
  where
    GenesisTip =
        Ouroboros.TipGenesis

pattern Tip :: SlotNo -> HeaderHash Block -> BlockNo -> Tip
pattern Tip s h b <-
    Ouroboros.Tip s h b
  where
    Tip =
        Ouroboros.Tip

getTipSlotNo :: Tip -> SlotNo
getTipSlotNo tip =
    case Ouroboros.getTipSlotNo tip of
        Origin -> SlotNo 0
        At sl  -> sl

distanceToTip :: Tip -> SlotNo -> Word64
distanceToTip =
    distanceToSlot . getTipSlotNo
{-# INLINABLE distanceToTip #-}

