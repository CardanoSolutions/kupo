-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module Test.Kupo.Data.SubTransactionSpec
    ( spec
    ) where

import Kupo.Prelude

import Cardano.Ledger.Alonzo.TxWits
    ( Redeemers (..)
    , TxDats (..)
    )
import Cardano.Ledger.BaseTypes
    ( StrictMaybe (..)
    )
import Cardano.Ledger.Plutus.Data
    ( binaryDataToData
    )
import Cardano.Ledger.Plutus.ExUnits
    ( ExUnits (..)
    )
import Control.Lens
    ( (.~)
    )
import Kupo.App.Database
    ( newDBPool
    )
import Kupo.App.Database.Types
    ( DBPool (..)
    , Database (..)
    )
import Kupo.Control.MonadLog
    ( nullTracer
    )
import Kupo.Data.Cardano
    ( BinaryData
    , Block
    , DatumHash
    , HasTransactionId (..)
    , IsBlock (..)
    , Output
    , OutputReference
    , Point
    , Script
    , ScriptHash
    , Transaction (..)
    , TransactionId
    , getPointSlotNo
    , hashBinaryData
    , hashScript
    , mkOutputReference
    )
import Kupo.Data.Configuration
    ( DatabaseLocation (..)
    , DeferIndexesInstallation (..)
    , LongestRollback (..)
    )
import Kupo.Data.Database
    ( SortDirection (..)
    , binaryDataToRow
    , policyToRow
    , resultToRow
    , scriptToRow
    )
import Kupo.Data.Http.ReferenceFlag
    ( ReferenceFlag (..)
    )
import Kupo.Data.Http.SlotRange
    ( Range (..)
    )
import Kupo.Data.Http.StatusFlag
    ( StatusFlag (..)
    )
import Kupo.Data.Pattern
    ( Codecs (..)
    , Match (..)
    , MatchBootstrap (..)
    , Pattern (..)
    , Result (..)
    , matchBlock
    )
import Test.Hspec
    ( Spec
    , context
    , parallel
    , shouldBe
    , specify
    )
import Test.Kupo.Data.Generators
    ( genBinaryData
    , genNonGenesisPoint
    , genNonGenesisPointBetween
    , genOutput
    , genOutputReference
    , genScript
    )
import Test.QuickCheck
    ( Gen
    , generate
    , suchThat
    , vectorOf
    )

import qualified Cardano.Ledger.Dijkstra.Core as Ledger
import qualified Data.Map as Map

spec :: Spec
spec = parallel $ do
    context "matchBlock" $ do
        specify "matches the outputs of sub-transactions, in ledger order, before those of their top-level transaction" $ do
            pt <- generate genNonGenesisPoint
            [plainInput, subInput0, subInput1, input] <- generate (vectorOf 4 genOutputReference)
            [plainOutput, subOutput0, subOutput1, subOutput2, output] <- generate (vectorOf 5 genOutput)
            let plain = mkTransaction [plainInput] [plainOutput]
            let sub0 = mkTransaction [subInput0] [subOutput0]
            let sub1 = mkTransaction [subInput1] [subOutput1, subOutput2]
            let top = mkTransaction [input] [output] & withSubTransactions [sub0, sub1]
            let Match{produced} = matchBlock codecs matchAll (DijkstraBlock pt [plain, top])
            [ (outputReference, subTransactionOf) | Result{outputReference, subTransactionOf} <- produced ]
                `shouldBe`
                [ ((mkOutputReference (topLevelId plain) 0, 0), Nothing)
                , ((mkOutputReference (subId sub0) 0, 1), Just (topLevelId top, 0))
                , ((mkOutputReference (subId sub1) 0, 1), Just (topLevelId top, 1))
                , ((mkOutputReference (subId sub1) 1, 1), Just (topLevelId top, 1))
                , ((mkOutputReference (topLevelId top) 0, 1), Nothing)
                ]

        specify "records the spends of a sub-transaction under its own id, input order and redeemers" $ do
            pt <- generate genNonGenesisPoint
            [subInput0, subInput1, input] <- generate (vectorOf 3 genOutputReference)
            (redeemer, subRedeemer) <- generate genDistinctPair
            let (lo, hi) = (min subInput0 subInput1, max subInput0 subInput1)
            let sub = mkTransaction [hi, lo] [] & withSpendRedeemer 1 subRedeemer
            let top = mkTransaction [input] []
                    & withSpendRedeemer 0 redeemer
                    & withSubTransactions [sub]
            let Match{consumed} = matchBlock codecs matchAll (DijkstraBlock pt [top])
            let slotNo = getPointSlotNo pt
            consumed `shouldBe` fromList
                [ ( (subId sub, Just (topLevelId top), slotNo)
                  , [ (MatchOutputReference lo, 0, Nothing)
                    , (MatchOutputReference hi, 1, Just subRedeemer)
                    ]
                  )
                , ( (topLevelId top, Nothing, slotNo)
                  , [ (MatchOutputReference input, 0, Just redeemer)
                    ]
                  )
                ]

        specify "collects only the datums and scripts of the sub-transactions of a phase-2 invalid transaction" $ do
            pt <- generate genNonGenesisPoint
            [subInput, input, collateral] <- generate (vectorOf 3 genOutputReference)
            [subOutput, output, collateralReturn] <- generate (vectorOf 3 genOutput)
            subDatum <- generate genBinaryData
            script <- generate genScript
            let sub = mkTransaction [subInput] [subOutput] & withDatum subDatum & withScript script
            let top = mkTransaction [input] [output]
                    & withSubTransactions [sub]
                    & Ledger.isPhase2ValidTxL .~ Ledger.Phase2Invalid
                    & Ledger.bodyTxL . Ledger.collateralInputsTxBodyL .~ fromList [collateral]
                    & Ledger.bodyTxL . Ledger.collateralReturnTxBodyL .~ SJust collateralReturn
            let Match{consumed, produced, datums, scripts} = matchBlock codecs matchAll (DijkstraBlock pt [top])
            consumed `shouldBe` fromList
                [ ( (topLevelId top, Nothing, getPointSlotNo pt)
                  , [ (MatchOutputReference collateral, 0, Nothing) ]
                  )
                ]
            [ (outputReference, subTransactionOf) | Result{outputReference, subTransactionOf} <- produced ]
                `shouldBe`
                [ ((mkOutputReference (topLevelId top) 1, 0), Nothing) ]
            datums `shouldBe` [(hashBinaryData subDatum, subDatum)]
            -- The outputs may carry reference scripts too.
            Map.lookup (hashScript script) (fromList scripts) `shouldBe` Just script

        specify "matches *@<transaction id> on the own id of a sub-transaction or top-level transaction" $ do
            pt <- generate genNonGenesisPoint
            [subInput, input] <- generate (vectorOf 2 genOutputReference)
            [subOutput, output] <- generate (vectorOf 2 genOutput)
            let sub = mkTransaction [subInput] [subOutput]
            let top = mkTransaction [input] [output] & withSubTransactions [sub]
            let matchedBy txId =
                    [ outputReference
                    | Result{outputReference} <- produced $
                        matchBlock codecs (fromList [MatchTransactionId txId]) (DijkstraBlock pt [top])
                    ]
            matchedBy (subId sub) `shouldBe` [(mkOutputReference (subId sub) 0, 0)]
            matchedBy (topLevelId top) `shouldBe` [(mkOutputReference (topLevelId top) 0, 0)]

        specify "collects the datums and scripts of the witness sets of sub-transactions" $ do
            pt <- generate genNonGenesisPoint
            [subInput, input] <- generate (vectorOf 2 genOutputReference)
            (datum, subDatum) <- generate genDistinctPair
            script <- generate genScript
            let sub = mkTransaction [subInput] [] & withDatum subDatum & withScript script
            let top = mkTransaction [input] [] & withDatum datum & withSubTransactions [sub]
            let Match{datums, scripts} = matchBlock codecs matchAll (DijkstraBlock pt [top])
            (fromList datums :: Map DatumHash BinaryData) `shouldBe` fromList
                [ (hashBinaryData datum, datum)
                , (hashBinaryData subDatum, subDatum)
                ]
            scripts `shouldBe` [(hashScript script, script)]

    context "database" $ do
        specify "stores the outputs and spends of sub-transactions with their top-level transaction" $ do
            (pt1, pt2) <- generate $ (,)
                <$> genNonGenesisPointBetween (1, 10)
                <*> genNonGenesisPointBetween (11, 20)
            [fundingInput, subInput, input] <- generate (vectorOf 3 genOutputReference)
            [spent0, spent1, subOutput0, subOutput1, output] <- generate (vectorOf 5 genOutput)
            let funding = mkTransaction [fundingInput] [spent0, spent1]
            let sub0 = mkTransaction [mkOutputReference (topLevelId funding) 0] [subOutput0]
            let sub1 = mkTransaction [mkOutputReference (topLevelId funding) 1, subInput] [subOutput1]
            let top = mkTransaction [input] [output] & withSubTransactions [sub0, sub1]
            let blocks = [DijkstraBlock pt1 [funding], DijkstraBlock pt2 [top]]
            let databaseCodecs = Codecs
                    { toResult = resultToRow
                    , toBinaryData = binaryDataToRow
                    , toScript = scriptToRow
                    , toPolicy = policyToRow
                    }
            results <- withInMemoryDatabase $ \Database{..} -> do
                let Match{consumed, produced} =
                        foldMap (matchBlock databaseCodecs matchAll) blocks
                runTransaction $ do
                    insertCheckpoints (getPoint <$> blocks)
                    insertInputs produced
                    void $ Map.traverseWithKey markInputs consumed
                acc <- newIORef []
                runTransaction $ foldInputs (MatchAny IncludingBootstrap) Whole NoStatusFlag AsReference Asc
                    (\result -> modifyIORef' acc (result :))
                reverse <$> readIORef acc
            let spendIndex = if mkOutputReference (topLevelId funding) 1 < subInput then 0 else 1
            [ (outputReference, subTransactionOf, spentBy, spentByTopLevelTransactionId)
                | Result{outputReference, subTransactionOf, spentBy, spentByTopLevelTransactionId} <- results
                ]
                `shouldBe`
                [ ( (mkOutputReference (topLevelId funding) 0, 0)
                  , Nothing
                  , Just (mkOutputReference (subId sub0) 0)
                  , Just (topLevelId top)
                  )
                , ( (mkOutputReference (topLevelId funding) 1, 0)
                  , Nothing
                  , Just (mkOutputReference (subId sub1) spendIndex)
                  , Just (topLevelId top)
                  )
                , ((mkOutputReference (subId sub0) 0, 0), Just (topLevelId top, 0), Nothing, Nothing)
                , ((mkOutputReference (subId sub1) 0, 0), Just (topLevelId top, 1), Nothing, Nothing)
                , ((mkOutputReference (topLevelId top) 0, 0), Nothing, Nothing, Nothing)
                ]

        specify "spends the outputs that sub-transactions create within the same block" $ do
            pt <- generate genNonGenesisPoint
            [fundingInput, input] <- generate (vectorOf 2 genOutputReference)
            [subOutput0, subOutput1, output] <- generate (vectorOf 3 genOutput)
            let sub0 = mkTransaction [fundingInput] [subOutput0]
            let sub1 = mkTransaction [mkOutputReference (subId sub0) 0] [subOutput1]
            let top = mkTransaction [input, mkOutputReference (subId sub1) 0] [output]
                    & withSubTransactions [sub0, sub1]
            let databaseCodecs = Codecs
                    { toResult = resultToRow
                    , toBinaryData = binaryDataToRow
                    , toScript = scriptToRow
                    , toPolicy = policyToRow
                    }
            results <- withInMemoryDatabase $ \Database{..} -> do
                let Match{consumed, produced} =
                        matchBlock databaseCodecs matchAll (DijkstraBlock pt [top])
                runTransaction $ do
                    insertCheckpoints [pt]
                    insertInputs produced
                    void $ Map.traverseWithKey markInputs consumed
                acc <- newIORef []
                runTransaction $ foldInputs (MatchAny IncludingBootstrap) Whole NoStatusFlag AsReference Asc
                    (\result -> modifyIORef' acc (result :))
                reverse <$> readIORef acc
            let inputIndex = if mkOutputReference (subId sub1) 0 < input then 0 else 1
            [ (outputReference, subTransactionOf, spentBy, spentByTopLevelTransactionId)
                | Result{outputReference, subTransactionOf, spentBy, spentByTopLevelTransactionId} <- results
                ]
                `shouldBe`
                [ ( (mkOutputReference (subId sub0) 0, 0)
                  , Just (topLevelId top, 0)
                  , Just (mkOutputReference (subId sub1) 0)
                  , Just (topLevelId top)
                  )
                , ( (mkOutputReference (subId sub1) 0, 0)
                  , Just (topLevelId top, 1)
                  , Just (mkOutputReference (topLevelId top) inputIndex)
                  , Nothing
                  )
                , ((mkOutputReference (topLevelId top) 0, 0), Nothing, Nothing, Nothing)
                ]

--
-- Helpers
--

matchAll :: Set Pattern
matchAll =
    fromList [MatchAny IncludingBootstrap]

codecs :: Codecs Result (DatumHash, BinaryData) (ScriptHash, Script) ()
codecs = Codecs
    { toResult = identity
    , toBinaryData = (,)
    , toScript = (,)
    , toPolicy = \_ _ -> ()
    }

withInMemoryDatabase
    :: (Database IO -> IO a)
    -> IO a
withInMemoryDatabase action = do
    pool <- newDBPool nullTracer False (InMemory (Just ":memory:")) (LongestRollback 10)
    withDatabaseExclusiveWriter pool InstallIndexesIfNotExist action

topLevelId :: Ledger.Tx Ledger.TopTx DijkstraEra -> TransactionId
topLevelId =
    getTransactionId . TransactionDijkstra

subId :: Ledger.Tx Ledger.SubTx DijkstraEra -> TransactionId
subId =
    getTransactionId . SubTransactionDijkstra

--
-- Fixtures
--

-- | A Dijkstra block reduced to its point and transactions, read like a 'Block'.
data DijkstraBlock = DijkstraBlock
    { blockPoint :: !Point
    , blockTransactions :: ![Ledger.Tx Ledger.TopTx DijkstraEra]
    }

instance IsBlock DijkstraBlock where
    type BlockBody DijkstraBlock = Transaction
    getPoint =
        blockPoint
    foldBlock fn result =
        foldrWithIndex (\ix -> fn ix . TransactionDijkstra) result . blockTransactions
    checkSubTransactions =
        const (Right ())
    subTransactions =
        subTransactions @Block
    discardedSubTransactions =
        discardedSubTransactions @Block
    spentInputs =
        spentInputs @Block
    mapMaybeOutputs =
        mapMaybeOutputs @Block
    witnessedDatums =
        witnessedDatums @Block
    witnessedScripts =
        witnessedScripts @Block
    userDefinedMetadata =
        userDefinedMetadata @Block
    spendRedeemer =
        spendRedeemer @Block

mkTransaction
    :: Typeable level
    => [OutputReference]
    -> [Output]
    -> Ledger.Tx level DijkstraEra
mkTransaction inputs outputs =
    Ledger.mkBasicTx $ Ledger.mkBasicTxBody
        & Ledger.inputsTxBodyL .~ fromList inputs
        & Ledger.outputsTxBodyL .~ fromList outputs

withSubTransactions
    :: [Ledger.Tx Ledger.SubTx DijkstraEra]
    -> Ledger.Tx Ledger.TopTx DijkstraEra
    -> Ledger.Tx Ledger.TopTx DijkstraEra
withSubTransactions subTxs =
    Ledger.bodyTxL . Ledger.subTransactionsTxBodyL .~ fromList subTxs

withDatum
    :: BinaryData
    -> Ledger.Tx level DijkstraEra
    -> Ledger.Tx level DijkstraEra
withDatum datum =
    Ledger.witsTxL . Ledger.datsTxWitsL .~ TxDats (fromList [(hashBinaryData datum, binaryDataToData datum)])

withScript
    :: Script
    -> Ledger.Tx level DijkstraEra
    -> Ledger.Tx level DijkstraEra
withScript script =
    Ledger.witsTxL . Ledger.scriptTxWitsL .~ fromList [(hashScript script, script)]

withSpendRedeemer
    :: Word32
    -> BinaryData
    -> Ledger.Tx level DijkstraEra
    -> Ledger.Tx level DijkstraEra
withSpendRedeemer ix redeemer =
    Ledger.witsTxL . Ledger.rdmrsTxWitsL .~ Redeemers
        (fromList [(Ledger.SpendingPurpose (Ledger.AsIx ix), (binaryDataToData redeemer, ExUnits 0 0))])

--
-- Generators
--

genDistinctPair :: Gen (BinaryData, BinaryData)
genDistinctPair =
    ((,) <$> genBinaryData <*> genBinaryData) `suchThat` uncurry (/=)
