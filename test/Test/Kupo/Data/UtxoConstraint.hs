module Test.Kupo.Data.UtxoConstraint where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( Address
    , Output
    , OutputReference
    , TransactionId
    , mkOutput
    , mkOutputReference
    )
import Test.Kupo.Data.Generators
    ( genAddress
    , genDatum
    , genNonBootstrapAddress
    , genOutputReference
    , genOutputValue
    , genScript
    )
import Test.QuickCheck
    ( Gen
    , choose
    , frequency
    )

data UtxoConstraint
    = MustHaveAddress !Address
    | MustHaveShelleyAddress
    | MustHaveTransactionId !TransactionId
    | MustHaveOutputReference !OutputReference

class ArbitrarySatisfying a where
    genSatisfying :: [UtxoConstraint] -> Gen a

    shrinkSatisfying :: [UtxoConstraint] -> a -> [a]
    shrinkSatisfying _ _ = []

instance ArbitrarySatisfying (OutputReference, Output) where
    genSatisfying constraints = do
        k <- genSatisfying constraints
        v <- mkOutput
            <$> genSatisfying constraints
            <*> genOutputValue
            <*> genDatum
            <*> frequency [(5, pure Nothing), (1, Just <$> genScript)]
        pure (k, v)

instance ArbitrarySatisfying Address where
    genSatisfying = \case
        [] ->
            genAddress
        (MustHaveShelleyAddress):_ ->
            genNonBootstrapAddress
        (MustHaveAddress addr):_ ->
            pure addr
        _:rest ->
            genSatisfying rest

instance ArbitrarySatisfying OutputReference where
    genSatisfying = \case
        [] ->
            genOutputReference
        (MustHaveOutputReference outRef):_ ->
            pure outRef
        (MustHaveTransactionId txId):_ ->
            mkOutputReference txId <$> choose (0, 255)
        _:rest ->
            genSatisfying rest
