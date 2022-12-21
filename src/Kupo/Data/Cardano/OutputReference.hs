module Kupo.Data.Cardano.OutputReference where

import Kupo.Prelude

import Kupo.Data.Cardano.Output
    ( Output
    )
import Kupo.Data.Cardano.OutputIndex
    ( OutputIndex
    , outputIndexFromText
    , outputIndexToText
    )
import Kupo.Data.Cardano.TransactionId
    ( HasTransactionId (..)
    , TransactionId
    , transactionIdFromText
    , transactionIdToText
    )
import Kupo.Data.Cardano.TransactionIndex
    ( TransactionIndex
    )

import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Data.Text as T

-- Input

type Input =
    Input' StandardCrypto

type Input' crypto =
    Ledger.TxIn crypto

-- ExtendedOutputReference

type ExtendedOutputReference =
    (OutputReference, TransactionIndex)

-- OutputReference

type OutputReference =
    OutputReference' StandardCrypto

type OutputReference' crypto =
    Input' crypto

getOutputIndex :: OutputReference' crypto -> OutputIndex
getOutputIndex (Ledger.TxIn _ (Ledger.TxIx ix)) =
    fromIntegral ix
{-# INLINABLE getOutputIndex #-}

mkOutputReference
    :: TransactionId
    -> OutputIndex
    -> OutputReference
mkOutputReference i =
    Ledger.TxIn i . Ledger.TxIx . fromIntegral
{-# INLINABLE mkOutputReference #-}

withReferences
    :: TransactionId
    -> [Output]
    -> [(OutputReference, Output)]
withReferences txId = loop 0
  where
    loop ix = \case
        [] -> []
        out:rest ->
            let
                results = loop (next ix) rest
             in
                (mkOutputReference txId ix, out) : results

outputReferenceFromText :: Text -> Maybe OutputReference
outputReferenceFromText txt =
    case T.splitOn "@" txt of
        [outputIndex, txId] -> do
            mkOutputReference
                <$> transactionIdFromText txId
                <*> outputIndexFromText outputIndex
        _malformedTextString ->
            Nothing

outputReferenceToText :: OutputReference -> Text
outputReferenceToText outRef =
    outputIndexToText (getOutputIndex outRef)
    <>
    "@"
    <>
    transactionIdToText (getTransactionId outRef)
