-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module Test.Kupo.Data.SubTransactionSpec
    ( spec
    ) where

import Kupo.Prelude

import Cardano.Crypto.DSIGN
    ( deriveVerKeyDSIGN
    , genKeyDSIGN
    , signedDSIGN
    )
import Cardano.Crypto.KES
    ( unsoundPureDeriveVerKeyKES
    , unsoundPureGenKeyKES
    , unsoundPureSignedKES
    )
import Cardano.Crypto.Seed
    ( mkSeedFromBytes
    )
import Cardano.Crypto.VRF
    ( deriveVerKeyVRF
    , evalCertified
    , genKeyVRF
    )
import Cardano.Ledger.Alonzo.TxWits
    ( Redeemers (..)
    , TxDats (..)
    )
import Cardano.Ledger.BaseTypes
    ( Nonce (..)
    , ProtVer (..)
    , StrictMaybe (..)
    , strictMaybe
    )
import Cardano.Ledger.Keys
    ( VKey (..)
    )
import Cardano.Ledger.Plutus.Data
    ( binaryDataToData
    )
import Cardano.Ledger.Plutus.ExUnits
    ( ExUnits (..)
    )
import Cardano.Protocol.Praos.BlockHeader
    ( Header (Header)
    , HeaderBody (..)
    )
import Cardano.Protocol.Praos.VRF
    ( mkInputVRF
    )
import Cardano.Protocol.TPraos.BlockHeader
    ( PrevHash (..)
    )
import Cardano.Protocol.TPraos.OCert
    ( KESPeriod (..)
    , OCert (..)
    , OCertSignable (..)
    )
import Control.Lens
    ( (.~)
    )
import Data.Aeson.Lens
    ( key
    , _String
    )
import Kupo.App.Database
    ( newDBPool
    )
import Kupo.App.Database.Types
    ( DBPool (..)
    , Database (..)
    )
import Kupo.App.Http
    ( app
    )
import Kupo.Control.MonadLog
    ( nullTracer
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Data.Cardano
    ( BinaryData
    , Block
    , BlockNo (..)
    , DatumHash
    , HasTransactionId (..)
    , IsBlock (..)
    , Metadata
    , Output
    , OutputReference
    , Point
    , Script
    , ScriptHash
    , SlotNo (..)
    , SubTransactionIndex
    , Transaction (..)
    , TransactionId
    , getPointSlotNo
    , hashBinaryData
    , hashMetadata
    , hashScript
    , metadataHashToText
    , mkOutputReference
    , slotNoToText
    , transactionIdToText
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
import Ouroboros.Consensus.Cardano.Block
    ( HardForkBlock (..)
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( mkShelleyBlock
    )
import Test.Hspec
    ( Spec
    , context
    , parallel
    , shouldBe
    , shouldReturn
    , specify
    )
import Test.Kupo.App.Http.Client
    ( decodeResult
    )
import Test.Kupo.App.HttpSpec
    ( healthStub
    )
import Test.Kupo.Data.Generators
    ( genBinaryData
    , genMetadata
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

import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Dijkstra.Core as Ledger
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai

spec :: Spec
spec = parallel $ do
    context "IsBlock Block" $ do
        specify "points at the slot of a Dijkstra block" $ do
            SubTransactionBlock slot block _ _ _ _ _ _ <- newSubTransactionBlock
            getPointSlotNo (getPoint block) `shouldBe` slot

        specify "folds over the top-level transactions of a Dijkstra block only, in block order" $ do
            SubTransactionBlock _ block plain parent _ invalid _ _ <- newSubTransactionBlock
            foldBlock (\ix tx -> ((ix, getTransactionId tx) :)) [] block
                `shouldBe`
                [ (0, topLevelId plain)
                , (1, topLevelId parent)
                , (2, topLevelId invalid)
                ]

        specify "lists the sub-transactions of a phase-2 valid transaction in ledger order, and those of an invalid one as discarded" $ do
            SubTransactionBlock _ block plain parent subs invalid invalidSubs _ <- newSubTransactionBlock
            let listed tx =
                    ( getTransactionId tx
                    , getTransactionId <$> subTransactions @Block tx
                    , getTransactionId <$> discardedSubTransactions @Block tx
                    )
            foldBlock (\_ tx -> (listed tx :)) [] block
                `shouldBe`
                [ (topLevelId plain, [], [])
                , (topLevelId parent, subId <$> subs, [])
                , (topLevelId invalid, [], subId <$> invalidSubs)
                ]

        specify "accepts a Dijkstra block with sub-transactions" $ do
            SubTransactionBlock _ block _ _ _ _ _ _ <- newSubTransactionBlock
            first displayException (checkSubTransactions block) `shouldBe` Right ()

        specify "matches the outputs and spends of sub-transactions, and only the collateral of an invalid transaction" $ do
            SubTransactionBlock slot block plain parent [sub0, sub1] invalid _ collateral <- newSubTransactionBlock
            let Match{consumed, produced} = matchBlock codecs matchAll block
            [ (outputReference, subTransactionOf) | Result{outputReference, subTransactionOf} <- produced ]
                `shouldBe`
                [ ((mkOutputReference (topLevelId plain) 0, 0), Nothing)
                , ((mkOutputReference (subId sub0) 0, 1), Just (topLevelId parent, 0))
                , ((mkOutputReference (subId sub1) 0, 1), Just (topLevelId parent, 1))
                , ((mkOutputReference (topLevelId parent) 0, 1), Nothing)
                -- The collateral return, indexed after the single output of the invalid transaction.
                , ((mkOutputReference (topLevelId invalid) 1, 2), Nothing)
                ]
            Map.keys consumed `shouldBe` sort
                [ (topLevelId plain, Nothing, slot)
                , (subId sub0, Just (topLevelId parent), slot)
                , (subId sub1, Just (topLevelId parent), slot)
                , (topLevelId parent, Nothing, slot)
                , (topLevelId invalid, Nothing, slot)
                ]
            Map.lookup (topLevelId invalid, Nothing, slot) consumed
                `shouldBe` Just [(MatchOutputReference collateral, 0, Nothing)]

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

    context "http" $ do
        specify "filters matches by the top-level transaction of the sub-transactions that created them" $ do
            withIndexedBlocks $ \(Indexed funding sub0 sub1 top) application -> do
                let path filter_ = "/matches?order=oldest_first&" <> filter_ <> "="
                creatorsAt application (path "top_level_transaction_id" <> transactionIdToText (topLevelId top))
                    `shouldReturn`
                    [ (subId sub0, Just (topLevelId top, 0))
                    , (subId sub1, Just (topLevelId top, 1))
                    ]
                creatorsAt application (path "top_level_transaction_id" <> transactionIdToText (topLevelId funding))
                    `shouldReturn` []
                creatorsAt application (path "top_level_transaction_id" <> transactionIdToText (subId sub0))
                    `shouldReturn` []

        specify "rejects the top-level transaction filter combined with other filters or given twice" $ do
            withIndexedBlocks $ \(Indexed _ _ _ top) application -> do
                let topLevel = "top_level_transaction_id=" <> transactionIdToText (topLevelId top)
                let policyId = T.replicate 56 "0"
                forM_
                    [ topLevel <> "&transaction_id=" <> transactionIdToText (topLevelId top)
                    , topLevel <> "&policy_id=" <> policyId
                    , topLevel <> "&policy_id=" <> policyId <> "&asset_name=00"
                    , topLevel <> "&output_index=0"
                    , "output_index=0&" <> topLevel
                    , topLevel <> "&" <> topLevel
                    , "top_level_transaction_id=foo"
                    ] $ \query -> do
                        response <- getResponse application ("/matches?" <> query)
                        (query, Http.statusCode (Wai.simpleStatus response)) `shouldBe` (query, 400)

        specify "matches by the id of a sub-transaction through the transaction_id filter and patterns" $ do
            withIndexedBlocks $ \(Indexed _ _ sub1 top) application -> do
                let expectedSub = [(subId sub1, Just (topLevelId top, 1))]
                let subIdText = transactionIdToText (subId sub1)
                creatorsAt application ("/matches?transaction_id=" <> subIdText) `shouldReturn` expectedSub
                creatorsAt application ("/matches/*@" <> subIdText) `shouldReturn` expectedSub
                creatorsAt application ("/matches/0@" <> subIdText) `shouldReturn` expectedSub
                -- The id of the top-level transaction only matches its own outputs.
                let topIdText = transactionIdToText (topLevelId top)
                creatorsAt application ("/matches?transaction_id=" <> topIdText)
                    `shouldReturn` [(topLevelId top, Nothing)]
                creatorsAt application ("/matches/*@" <> topIdText)
                    `shouldReturn` [(topLevelId top, Nothing)]

        specify "reports the sub-transaction of a match, and of its spender, as null or a value" $ do
            withIndexedBlocks $ \(Indexed funding sub0 sub1 top) application -> do
                let topIdText = transactionIdToText (topLevelId top)
                let field name = (^? key name)
                [subMatch] <- getJson application ("/matches/*@" <> transactionIdToText (subId sub1))
                field "sub_transaction_index" subMatch `shouldBe` Just (Json.Number 1)
                field "top_level_transaction_id" subMatch `shouldBe` Just (Json.String topIdText)
                [topMatch] <- getJson application ("/matches/*@" <> topIdText)
                field "sub_transaction_index" topMatch `shouldBe` Just Json.Null
                field "top_level_transaction_id" topMatch `shouldBe` Just Json.Null
                [fundingMatch] <- getJson application
                    ("/matches?spent&transaction_id=" <> transactionIdToText (topLevelId funding) <> "&output_index=0")
                (fundingMatch ^? key "spent_at" . key "transaction_id")
                    `shouldBe` Just (Json.String $ transactionIdToText $ subId sub0)
                (fundingMatch ^? key "spent_at" . key "top_level_transaction_id")
                    `shouldBe` Just (Json.String topIdText)

        specify "reports an ordinary spender after a spending sub-transaction is rolled back" $ do
            (pt1, pt2, pt3) <- generate $ (,,)
                <$> genNonGenesisPointBetween (1, 10)
                <*> genNonGenesisPointBetween (11, 20)
                <*> genNonGenesisPointBetween (21, 30)
            [fundingInput, input, otherInput] <- generate (vectorOf 3 genOutputReference)
            [spent, subOutput, output] <- generate (vectorOf 3 genOutput)
            let funding = mkTransaction [fundingInput] [spent]
            let fundingRef = mkOutputReference (topLevelId funding) 0
            let sub = mkTransaction [fundingRef] [subOutput]
            let top = mkTransaction [input] [output] & withSubTransactions [sub]
            let ordinary = mkTransaction [fundingRef, otherInput] []
            let rowCodecs = Codecs
                    { toResult = resultToRow
                    , toBinaryData = binaryDataToRow
                    , toScript = scriptToRow
                    , toPolicy = policyToRow
                    }
            withInMemoryDatabase $ \database@Database{..} -> do
                let index block = do
                        let Match{consumed, produced} = matchBlock rowCodecs matchAll block
                        runTransaction $ do
                            insertCheckpoints [getPoint block]
                            insertInputs produced
                            void $ Map.traverseWithKey markInputs consumed
                index (DijkstraBlock pt1 [funding])
                index (DijkstraBlock pt2 [top])
                let matchesPath = "/matches?spent&transaction_id=" <> transactionIdToText (topLevelId funding)
                application2 <- newApplication database (DijkstraBlock pt2 [top])
                [before] <- getJson application2 matchesPath
                (before ^? key "spent_at" . key "transaction_id")
                    `shouldBe` Just (Json.String $ transactionIdToText $ subId sub)
                void $ runTransaction $ rollbackTo (getPointSlotNo pt1)
                index (DijkstraBlock pt3 [ordinary])
                application3 <- newApplication database (DijkstraBlock pt3 [ordinary])
                [after] <- getJson application3 matchesPath
                (after ^? key "spent_at" . key "transaction_id")
                    `shouldBe` Just (Json.String $ transactionIdToText $ topLevelId ordinary)
                (after ^? key "spent_at" . key "top_level_transaction_id")
                    `shouldBe` Just Json.Null

        specify "lists the metadata of sub-transactions before that of their top-level transaction" $ do
            withMetadataBlock $ \(MetadataBlock plain sub0 sub2 top invalidSub) slot application -> do
                entriesAt application slot "" `shouldReturn`
                    [ entry (topLevelId plain) Nothing plain
                    , entry (subId sub0) (Just (topLevelId top)) sub0
                    , entry (subId sub2) (Just (topLevelId top)) sub2
                    , entry (topLevelId top) Nothing top
                    ]
                entriesAt application slot ("?transaction_id=" <> transactionIdToText (subId sub2))
                    `shouldReturn` [entry (subId sub2) (Just (topLevelId top)) sub2]
                entriesAt application slot ("?transaction_id=" <> transactionIdToText (topLevelId top))
                    `shouldReturn` [entry (topLevelId top) Nothing top]
                -- The sub-transactions of a phase-2 invalid transaction are not applied.
                entriesAt application slot ("?transaction_id=" <> transactionIdToText (subId invalidSub))
                    `shouldReturn` []

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

-- | A 'Block' at a slot, with a plain transaction, a transaction with two
-- sub-transactions, and a phase-2 invalid transaction with two sub-transactions,
-- in this order, and the collateral input of the invalid transaction.
data SubTransactionBlock = SubTransactionBlock
    !SlotNo
    !Block
    !(Ledger.Tx Ledger.TopTx DijkstraEra)
    !(Ledger.Tx Ledger.TopTx DijkstraEra)
    ![Ledger.Tx Ledger.SubTx DijkstraEra]
    !(Ledger.Tx Ledger.TopTx DijkstraEra)
    ![Ledger.Tx Ledger.SubTx DijkstraEra]
    !OutputReference

newSubTransactionBlock :: IO SubTransactionBlock
newSubTransactionBlock = do
    slot <- getPointSlotNo <$> generate genNonGenesisPoint
    [plainInput, input, subInput0, subInput1, invalidInput, collateral, invalidSubInput0, invalidSubInput1] <-
        generate (vectorOf 8 genOutputReference)
    [plainOutput, output, subOutput0, subOutput1, invalidOutput, collateralReturn, invalidSubOutput0, invalidSubOutput1] <-
        generate (vectorOf 8 genOutput)
    let plain = mkTransaction [plainInput] [plainOutput]
    -- Descending ids, so that the ledger order of the sub-transactions is not their id order.
    let subs = sortOn (Down . subId)
            [ mkTransaction [subInput0] [subOutput0]
            , mkTransaction [subInput1] [subOutput1]
            ]
    let parent = mkTransaction [input] [output] & withSubTransactions subs
    let invalidSubs =
            [ mkTransaction [invalidSubInput0] [invalidSubOutput0]
            , mkTransaction [invalidSubInput1] [invalidSubOutput1]
            ]
    let invalid = mkTransaction [invalidInput] [invalidOutput]
            & withSubTransactions invalidSubs
            & Ledger.isPhase2ValidTxL .~ Ledger.Phase2Invalid
            & Ledger.bodyTxL . Ledger.collateralInputsTxBodyL .~ fromList [collateral]
            & Ledger.bodyTxL . Ledger.collateralReturnTxBodyL .~ SJust collateralReturn
    let block = mkDijkstraBlock slot [plain, parent, invalid]
    pure $ SubTransactionBlock slot block plain parent subs invalid invalidSubs collateral

-- | A Dijkstra 'Block' as cardano-node serves it. Its header is signed with keys
-- derived from fixed seeds, since Kupo does not verify headers.
mkDijkstraBlock
    :: SlotNo
    -> [Ledger.Tx Ledger.TopTx DijkstraEra]
    -> Block
mkDijkstraBlock slot txs =
    BlockDijkstra . mkShelleyBlock $ Ledger.Block (Header headerBody signature) body
  where
    body = Ledger.mkBasicBlockBody & Ledger.txSeqBlockBodyL .~ fromList txs
    protocolVersion = ProtVer (Ledger.eraProtVerLow @DijkstraEra) 0
    coldKey = genKeyDSIGN (seed 0)
    vrfKey = genKeyVRF (seed 1)
    kesKey = unsoundPureGenKeyKES (seed 2)
    hotKey = unsoundPureDeriveVerKeyKES kesKey
    headerBody = HeaderBody
        { hbBlockNo = BlockNo (unSlotNo slot)
        , hbSlotNo = slot
        , hbPrev = GenesisHash
        , hbVk = VKey (deriveVerKeyDSIGN coldKey)
        , hbVrfVk = deriveVerKeyVRF vrfKey
        , hbVrfRes = evalCertified () (mkInputVRF slot NeutralNonce) vrfKey
        , hbBodySize = fromIntegral (Ledger.blockBodySize protocolVersion body)
        , hbBodyHash = Ledger.hashBlockBody body
        , hbOCert = OCert hotKey 0 (KESPeriod 0) $
            signedDSIGN () (OCertSignable hotKey 0 (KESPeriod 0)) coldKey
        , hbProtVer = protocolVersion
        }
    signature = unsoundPureSignedKES () 0 headerBody kesKey
    seed = mkSeedFromBytes . BS.replicate 32

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

-- | A funding transaction, a top-level transaction spending its outputs from
-- two sub-transactions, all indexed in an in-memory database behind the HTTP
-- application.
data Indexed = Indexed
    !(Ledger.Tx Ledger.TopTx DijkstraEra)
    !(Ledger.Tx Ledger.SubTx DijkstraEra)
    !(Ledger.Tx Ledger.SubTx DijkstraEra)
    !(Ledger.Tx Ledger.TopTx DijkstraEra)

withIndexedBlocks
    :: (Indexed -> Wai.Application -> IO a)
    -> IO a
withIndexedBlocks action = do
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
    let rowCodecs = Codecs
            { toResult = resultToRow
            , toBinaryData = binaryDataToRow
            , toScript = scriptToRow
            , toPolicy = policyToRow
            }
    withInMemoryDatabase $ \database@Database{..} -> do
        let Match{consumed, produced} = foldMap (matchBlock rowCodecs matchAll) blocks
        runTransaction $ do
            insertCheckpoints (getPoint <$> blocks)
            insertInputs produced
            void $ Map.traverseWithKey markInputs consumed
        application <- newApplication database (DijkstraBlock pt2 [top])
        action (Indexed funding sub0 sub1 top) application

-- | A block with metadata on a transaction, on two of the three
-- sub-transactions of another one, and on the sub-transaction of a phase-2
-- invalid transaction.
data MetadataBlock = MetadataBlock
    !(Ledger.Tx Ledger.TopTx DijkstraEra)
    !(Ledger.Tx Ledger.SubTx DijkstraEra)
    !(Ledger.Tx Ledger.SubTx DijkstraEra)
    !(Ledger.Tx Ledger.TopTx DijkstraEra)
    !(Ledger.Tx Ledger.SubTx DijkstraEra)

withMetadataBlock
    :: (MetadataBlock -> SlotNo -> Wai.Application -> IO a)
    -> IO a
withMetadataBlock action = do
    pt <- generate genNonGenesisPoint
    [plainInput, subInput0, subInput1, subInput2, input, invalidInput, invalidSubInput] <-
        generate (vectorOf 7 genOutputReference)
    [plainMeta, meta0, meta2, topMeta, invalidSubMeta] <- generate (vectorOf 5 (snd <$> genMetadata))
    let plain = mkTransaction [plainInput] [] & withMetadata plainMeta
    let sub0 = mkTransaction [subInput0] [] & withMetadata meta0
    let sub1 = mkTransaction [subInput1] []
    let sub2 = mkTransaction [subInput2] [] & withMetadata meta2
    let top = mkTransaction [input] []
            & withMetadata topMeta
            & withSubTransactions [sub0, sub1, sub2]
    let invalidSub = mkTransaction [invalidSubInput] [] & withMetadata invalidSubMeta
    let invalid = mkTransaction [invalidInput] []
            & withSubTransactions [invalidSub]
            & Ledger.isPhase2ValidTxL .~ Ledger.Phase2Invalid
    withInMemoryDatabase $ \database -> do
        application <- newApplication database (DijkstraBlock pt [plain, top, invalid])
        action (MetadataBlock plain sub0 sub2 top invalidSub) (getPointSlotNo pt) application

newApplication
    :: Database IO
    -> DijkstraBlock
    -> IO Wai.Application
newApplication database block = do
    patternsVar <- newTVarIO mempty
    pure $ app
        (pure Nothing)
        (\_send _mode callback -> callback database)
        (\_point _handler -> pure ())
        (\_point reply -> reply (Just block))
        patternsVar
        healthStub

getResponse
    :: Wai.Application
    -> Text
    -> IO Wai.SResponse
getResponse application path =
    Wai.runSession
        (Wai.request $ Wai.setPath Wai.defaultRequest (encodeUtf8 path))
        application

getJson
    :: Wai.Application
    -> Text
    -> IO [Json.Value]
getJson application path = do
    response <- getResponse application path
    Http.statusCode (Wai.simpleStatus response) `shouldBe` 200
    either fail pure $ Json.eitherDecode (Wai.simpleBody response)

-- | The transaction that created each match, with its position in its top-level transaction.
creatorsAt
    :: Wai.Application
    -> Text
    -> IO [(TransactionId, Maybe (TransactionId, SubTransactionIndex))]
creatorsAt application path = do
    response <- getResponse application path
    Http.statusCode (Wai.simpleStatus response) `shouldBe` 200
    results <- either fail pure $
        eitherDecodeJson (Json.listParser decodeResult) (Wai.simpleBody response)
    pure
        [ (getTransactionId (fst outputReference), subTransactionOf)
        | Result{outputReference, subTransactionOf} <- results
        ]

-- | The transaction id, top-level transaction id and metadata hash of each /metadata entry.
entriesAt
    :: Wai.Application
    -> SlotNo
    -> Text
    -> IO [(Maybe Text, Maybe Json.Value, Maybe Text)]
entriesAt application slot query = do
    entries <- getJson application ("/metadata/" <> slotNoToText slot <> query)
    pure
        [ (item ^? key "transaction_id" . _String, item ^? key "top_level_transaction_id", item ^? key "hash" . _String)
        | item <- entries
        ]

entry
    :: TransactionId
    -> Maybe TransactionId
    -> Ledger.Tx level DijkstraEra
    -> (Maybe Text, Maybe Json.Value, Maybe Text)
entry transactionId topLevel tx =
    ( Just (transactionIdToText transactionId)
    , Just (maybe Json.Null (Json.String . transactionIdToText) topLevel)
    , strictMaybe Nothing (Just . metadataHashToText . hashMetadata) (tx ^. Ledger.auxDataTxL)
    )

withMetadata
    :: Metadata
    -> Ledger.Tx level DijkstraEra
    -> Ledger.Tx level DijkstraEra
withMetadata metadata =
    Ledger.auxDataTxL .~ SJust metadata

--
-- Generators
--

genDistinctPair :: Gen (BinaryData, BinaryData)
genDistinctPair =
    ((,) <$> genBinaryData <*> genBinaryData) `suchThat` uncurry (/=)
