-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Kupo.Data.HydraSpec
    ( spec
    ) where

import qualified Data.Set as Set
import Kupo.App.ChainSync.Hydra
    ( TransactionStore (..)
    , TransactionStoreException (TransactionNotInStore)
    , newTransactionStore
    , pushTx
    )
import Kupo.Data.PartialBlock
    ( PartialTransaction (..)
    )
import Kupo.Prelude
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
    ( label
    , listOf1
    , shuffle
    )
import Test.QuickCheck.Monadic
    ( monadicIO
    , monitor
    , pick
    , run
    )

spec :: Spec
spec = parallel $ context "TransactionStore" $ do

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



