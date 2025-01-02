module Kupo.Data.Cardano.Redeemers where

import Kupo.Prelude

import Kupo.Data.Cardano.BinaryData
    ( BinaryData
    , fromAlonzoData
    , fromBabbageData
    , fromConwayData
    )
import Kupo.Data.Cardano.OutputIndex
    ( InputIndex
    )

import qualified Data.Map as Map
import qualified Cardano.Ledger.Alonzo.TxWits as Ledger
import qualified Cardano.Ledger.Conway.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger

data Redeemers
    = RedeemersAlonzo (Ledger.Redeemers (AlonzoEra StandardCrypto))
    | RedeemersBabbage (Ledger.Redeemers (BabbageEra StandardCrypto))
    | RedeemersConway (Ledger.Redeemers (ConwayEra StandardCrypto))

lookupSpendRedeemer
    :: InputIndex
    -> Redeemers
    -> Maybe BinaryData
lookupSpendRedeemer ix = \case
    RedeemersAlonzo (Ledger.Redeemers redeemers) ->
        let purpose = Ledger.AlonzoSpending (Ledger.AsIx (fromIntegral ix))
         in fromAlonzoData . fst <$> Map.lookup purpose redeemers
    RedeemersBabbage (Ledger.Redeemers redeemers) ->
        let purpose = Ledger.AlonzoSpending (Ledger.AsIx (fromIntegral ix))
         in fromBabbageData . fst <$> Map.lookup purpose redeemers
    RedeemersConway (Ledger.Redeemers redeemers) ->
        let purpose = Ledger.ConwaySpending (Ledger.AsIx (fromIntegral ix))
         in fromConwayData . fst <$> Map.lookup purpose redeemers
