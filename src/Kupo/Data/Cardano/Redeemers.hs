module Kupo.Data.Cardano.Redeemers where

import Kupo.Prelude

import Kupo.Data.Cardano.BinaryData
    ( BinaryData
    , fromAlonzoData
    , fromBabbageData
    , fromConwayData
    , fromDijkstraData
    )
import Kupo.Data.Cardano.OutputIndex
    ( InputIndex
    )

import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.TxWits as Ledger
import qualified Cardano.Ledger.Conway.Scripts as Ledger
import qualified Cardano.Ledger.Dijkstra.Scripts as Ledger
import qualified Data.Map as Map

data Redeemers
    = RedeemersAlonzo (Ledger.Redeemers AlonzoEra)
    | RedeemersBabbage (Ledger.Redeemers BabbageEra)
    | RedeemersConway (Ledger.Redeemers ConwayEra)
    | RedeemersDisjkstra (Ledger.Redeemers DijkstraEra)

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
    RedeemersDisjkstra (Ledger.Redeemers redeemers) ->
        let purpose = Ledger.DijkstraSpending (Ledger.AsIx (fromIntegral ix))
         in fromDijkstraData . fst <$> Map.lookup purpose redeemers
