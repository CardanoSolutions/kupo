module Test.Kupo.Data.UtxoConstraint where

import Kupo.Prelude

import Kupo.Data.Cardano
    ( Address
    , ExtendedOutputReference
    , Output
    , OutputReference
    , PolicyId
    , TransactionId
    , Value
    , mkOutput
    , mkOutputReference
    )
import Test.Kupo.Data.Generators
    ( genAddress
    , genDatum
    , genNonBootstrapAddress
    , genOutputReference
    , genOutputValue
    , genOutputValueWith
    , genScript
    , genTransactionIndex
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
    | MustHavePolicyId !PolicyId

class ArbitrarySatisfying a where
    genSatisfying :: [UtxoConstraint] -> Gen a

    shrinkSatisfying :: [UtxoConstraint] -> a -> [a]
    shrinkSatisfying _ _ = []

instance ArbitrarySatisfying (ExtendedOutputReference, Output) where
    genSatisfying constraints = do
        k <- genSatisfying constraints
        v <- mkOutput
            <$> genSatisfying constraints
            <*> genSatisfying constraints
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

instance ArbitrarySatisfying Value where
    genSatisfying = \case
        [] ->
            genOutputValue
        (MustHavePolicyId policy):_ -> do
            genOutputValueWith policy
        _:rest ->
            genSatisfying rest

instance ArbitrarySatisfying ExtendedOutputReference where
    genSatisfying constraints = do
        txIx <- genTransactionIndex
        (,txIx) <$> genSatisfying constraints

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
