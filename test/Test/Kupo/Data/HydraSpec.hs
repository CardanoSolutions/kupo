-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Kupo.Data.HydraSpec
    ( spec
    ) where

import Kupo.Prelude

import Data.Aeson.Lens
    ( _Array
    , key
    )
import Kupo.App.ChainSync.Hydra
    ( TransactionStore (..)
    , TransactionStoreException (..)
    , newTransactionStore
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

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Set as Set

spec :: Spec
spec = parallel $ do
  context "TransactionStore" $ do
    prop "can retrieve transactions in any order" prop_canRetrieveTxInAnyOrder

  context "JSON decoders" $ do
      context "decodeHydraMessage" $ do
          prop "can decode test vectors" $ withMaxSuccess 1 $ do
              prop_canDecodeFile
                  (mapM decodeHydraMessage . getSamples)
                  "./test/vectors/hydra/hydra-node/golden/ReasonablySized (ServerOutput (Tx BabbageEra)).json"

prop_canRetrieveTxInAnyOrder
    :: Property
prop_canRetrieveTxInAnyOrder = monadicIO $ do
      TransactionStore{push, pop} <- run newTransactionStore
      txs <- pick $ do
           txIns <- listOf1 genOutputReference
           evalStateT genPartialTransactions (Set.fromList txIns)
      txsWithIds <- forM txs $ \tx@PartialTransaction{id} -> do
            run $ push tx
            pure (tx, id)
      monitor (label $ "Generated list length is " <> show (length txsWithIds))
      shuffledTxs <- pick $ shuffle txsWithIds
      pure $
          forM_ shuffledTxs $ \(tx, txId) -> do
              txs' <- pop [txId]
              txs' `shouldBe` [tx]
              pop [txId] `shouldThrow` \case
                  TransactionNotInStore txId' -> txId' == txId

prop_canDecodeFile
    :: (Json.Value -> Json.Parser b)
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

getSamples :: Json.Value -> [Json.Value]
getSamples v =
  toList $ v ^. key "samples" . _Array
