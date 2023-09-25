-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Kupo.Data.HydraSpec
    ( spec
    ) where

import Kupo.Prelude

import qualified Data.Aeson as Json
import Data.Aeson.Lens
    ( _Array
    , key
    )
import qualified Data.Aeson.Types as Json
import qualified Data.Set as Set
import Kupo.App.ChainSync.Hydra
    ( TransactionStore (..)
    , TransactionStoreException (TransactionNotInStore)
    , newTransactionStore
    , pushTx
    )
import Kupo.Data.Hydra
    ( decodeHydraMessage
    )
import Kupo.Data.PartialBlock
    ( PartialTransaction (..)
    )
import Test.Hspec
    ( Spec
    , context
    , parallel
    , shouldBe
    , shouldThrow
    )
import Test.Hspec.QuickCheck
    ( prop
    )
import Test.Kupo.AppSpec
    ( genPartialTransactions
    )
import Test.Kupo.Data.Generators
    ( genOutputReference
    )
import Test.QuickCheck
    ( Property
    , counterexample
    , label
    , listOf1
    , shuffle
    , withMaxSuccess
    )
import Test.QuickCheck.Monadic
    ( assert
    , monadicIO
    , monitor
    , pick
    , run
    )

spec :: Spec
spec = parallel $ do

  context "TransactionStore" $ do

    prop "can retrieve transactions in any order" $ monadicIO $ do
      TransactionStore{pushTx, popTxById} <- run newTransactionStore
      txs <- pick $ do
           txIns <- listOf1 genOutputReference
           evalStateT genPartialTransactions (Set.fromList txIns)
      txsWithIds <- forM txs $ \tx@PartialTransaction{id} -> do
            run $ pushTx tx
            pure (tx, id)
      monitor (label $ "Generated list length is " <> show (length txsWithIds))
      shuffledTxs <- pick $ shuffle txsWithIds
      pure $
          forM_ shuffledTxs $ \(tx, txId) -> do
              tx' <- popTxById txId
              tx' `shouldBe` tx
              popTxById txId `shouldThrow` \case
                  TransactionNotInStore txId' -> txId' == txId


  context "JSON decoders" $ do
      context "decodeHydraMessage" $ do
          prop "can decode test vectors" $ withMaxSuccess 1 $ do
              prop_canDecodeFile
                  (mapM decodeHydraMessage . getSamples)
                  "./test/vectors/hydra/hydra-node/golden/ReasonablySized (ServerOutput (Tx BabbageEra)).json"

getSamples :: Json.Value -> [Json.Value]
getSamples v =
  toList $ v ^. key "samples" . _Array

prop_canDecodeFile ::
  (Json.Value -> Json.Parser b)
  -> FilePath
  -> Property
prop_canDecodeFile decoder vector = monadicIO $ do
    let errDecode = "Failed to decode JSON"
    value <- maybe (fail errDecode) pure =<< run (Json.decodeFileStrict vector)
    case Json.parse decoder value of
        Json.Error str -> do
            monitor $ counterexample (decodeUtf8 (Json.encode value))
            monitor $ counterexample str
            assert False
        Json.Success{} -> do
            assert True
