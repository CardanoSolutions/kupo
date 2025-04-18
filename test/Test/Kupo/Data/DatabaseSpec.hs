-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module Test.Kupo.Data.DatabaseSpec
    ( spec
    ) where

import Kupo.Prelude

import Data.List
    ( maximum
    )
import Database.SQLite.Simple
    ( Connection
    , Query (..)
    , SQLData (..)
    , executeMany
    , execute_
    , query_
    , withConnection
    , withTransaction
    )
import Kupo.App.Database
    ( deleteInputsQry
    , foldInputsQry
    , foldPoliciesQry
    , getBinaryDataQry
    , getScriptQry
    , installIndex
    , listAncestorQry
    , listCheckpointsQry
    , markInputsQry
    , newDBPool
    , pruneBinaryDataQry
    , pruneInputsQry
    , rollbackQryDeleteCheckpoints
    , rollbackQryDeleteInputs
    , rollbackQryUpdateInputs
    , selectMaxCheckpointQry
    )
import Kupo.App.Database.Types
    ( ConnectionType (..)
    , DBPool (..)
    , Database (..)
    )
import Kupo.Control.MonadAsync
    ( mapConcurrently_
    )
import Kupo.Control.MonadCatch
    ( MonadCatch (..)
    )
import Kupo.Control.MonadDelay
    ( threadDelay
    )
import Kupo.Control.MonadLog
    ( nullTracer
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Control.MonadThrow
    ( MonadThrow (..)
    )
import Kupo.Control.MonadTime
    ( millisecondsToDiffTime
    )
import Kupo.Data.Cardano
    ( Address
    , Output
    , Point
    , PolicyId
    , SlotNo (..)
    , foldrValue
    , getAddress
    , getOutputIndex
    , getPointSlotNo
    , getValue
    , policyIdToBytes
    , slotNoToText
    )
import Kupo.Data.Configuration
    ( DatabaseLocation (..)
    , DeferIndexesInstallation (..)
    , LongestRollback (..)
    , getLongestRollback
    )
import Kupo.Data.Database
    ( SortDirection (..)
    , addressFromRow
    , addressToRow
    , datumFromRow
    , datumToRow
    , extendedOutputReferenceFromRow
    , extendedOutputReferenceToRow
    , outputReferenceToRow
    , patternFromRow
    , patternToRow
    , patternToSql
    , pointFromRow
    , pointToRow
    , resultFromRow
    , resultToRow
    , scriptReferenceFromRow
    , scriptReferenceToRow
    )
import Kupo.Data.Http.ReferenceFlag
    ( ReferenceFlag (..)
    )
import Kupo.Data.Http.SlotRange
    ( Range (..)
    , RangeField (..)
    )
import Kupo.Data.Http.StatusFlag
    ( StatusFlag (..)
    )
import Kupo.Data.Pattern
    ( MatchBootstrap (..)
    , Pattern (..)
    , Result (..)
    )
import System.IO.Temp
    ( withSystemTempDirectory
    )
import Test.Hspec
    ( Expectation
    , Spec
    , around
    , context
    , parallel
    , shouldBe
    , specify
    )
import Test.Hspec.QuickCheck
    ( prop
    )
import Test.Kupo.Data.Generators
    ( chooseVector
    , genAddress
    , genBytes
    , genDatum
    , genExtendedOutputReference
    , genNonGenesisPoint
    , genNonGenesisPointBetween
    , genOutputReference
    , genPattern
    , genPointsBetween
    , genPolicyId
    , genQueryablePattern
    , genResult
    , genResultWith
    , genScriptReference
    , genTransactionId
    , generateWith
    )
import Test.Kupo.Data.Pattern.Fixture
    ( matches
    , patterns
    )
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , conjoin
    , counterexample
    , elements
    , forAllBlind
    , forAllShow
    , forAllShrinkShow
    , frequency
    , generate
    , label
    , listOf1
    , property
    , scale
    , shrinkList
    , withMaxSuccess
    , (.&&.)
    )
import Test.QuickCheck.Monadic
    ( PropertyM
    , assert
    , monadicIO
    , monitor
    , run
    )
import Test.QuickCheck.Property
    ( Testable
    )

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Prelude

spec :: Spec
spec = parallel $ do
    context "fromRow ↔ toRow" $ do
        prop "Address" $
            roundtripFromToRow genAddress addressToRow addressFromRow
        prop "Result" $
            roundtripFromToRow genResult resultToRow resultFromRow
        prop "Checkpoint" $
            roundtripFromToRow genNonGenesisPoint pointToRow pointFromRow
        prop "Pattern" $
            roundtripFromToRow genPattern patternToRow patternFromRow
        prop "OutputReference" $
            roundtripFromToRow genExtendedOutputReference extendedOutputReferenceToRow extendedOutputReferenceFromRow
        prop "Datum" $
            roundtripFromToRow2 genDatum datumToRow datumFromRow
        prop "ScriptReference" $
            roundtripFromToRow2 genScriptReference scriptReferenceToRow scriptReferenceFromRow

    context "patternToSql" $ around withFixtureDatabase $ do
        forM_ patterns $ \(_, p, ms) -> do
            let (whereClause, fromMaybe "" -> additionalJoin) = patternToSql p
            let results = sort $ (\(_, out) -> getAddress out) <$> ms
            specify (toString whereClause) $ \conn -> do
                rows <- query_ conn $ Query $ unwords
                    [ "SELECT address, LENGTH(address) as len FROM inputs"
                    , additionalJoin
                    , "WHERE"
                    , whereClause
                    ]
                sort (rowToAddress <$> rows) `shouldBe` results

    context "checkpoints" $ do
        let k = 100
        prop "list checkpoints after inserting them" $
            forAllCheckpoints k $ \pts -> monadicIO $ do
                cps <- withInMemoryDatabase k $ \Database{..} -> do
                    runTransaction $ insertCheckpoints pts
                    runTransaction $ fmap getPointSlotNo <$> listCheckpointsDesc
                monitor $ counterexample (show cps)
                assert $ all (uncurry (>)) (zip cps (drop 1 cps))
                assert $ Prelude.head cps == maximum (getPointSlotNo <$> pts)

        prop "get ancestor of any checkpoint" $
            forAllCheckpoints k $ \pts -> monadicIO $ do
                oneByOne <- withInMemoryDatabase k $ \Database{..} -> do
                    runTransaction $ insertCheckpoints pts
                    fmap mconcat $ runTransaction $ forM pts $ \pt -> do
                        listAncestorsDesc (getPointSlotNo pt) 1

                allAtOnce <- withInMemoryDatabase k $ \Database{..} -> do
                    runTransaction $ insertCheckpoints pts
                    fmap reverse $ runTransaction $ do
                        let slotNo = maximum (getPointSlotNo <$> pts)
                        listAncestorsDesc slotNo (fromIntegral $ length pts)

                monitor $ counterexample $ toString $ unlines
                    [ "one-by-one:  " <> show (getPointSlotNo <$> oneByOne)
                    , "all-at-once: " <> show (getPointSlotNo <$> allAtOnce)
                    ]

                assert (Prelude.init pts == oneByOne)
                assert (oneByOne == allAtOnce)

    context "matches" $ do
        prop "return matches in order" $ do
            let slot = getPointSlotNo . createdAt
            let txIx = snd . outputReference
            let outIx = getOutputIndex . fst . outputReference

            let oldestFirst current successor
                    | slot current == slot successor =
                        if txIx current == txIx successor then
                            label "same transaction index" (outIx current <= outIx successor)
                        else
                            property (txIx current < txIx successor)
                    | otherwise =
                        property (slot current < slot successor)

            let mostRecentFirst current successor
                    | slot current == slot successor =
                        if txIx current == txIx successor then
                            label "same transaction index" (outIx current >= outIx successor)
                        else
                            property (txIx current >= txIx successor)
                    | otherwise =
                        property (slot current > slot successor)

            let genConflictingResults =
                    scale (10*) $ listOf1 $ genResultWith (genNonGenesisPointBetween (1, 100))

            let shrinkResults =
                    shrinkList (const [])

            let showResults xs = toString $ unlines
                    [ "sl/tx/out"
                    , "---------"
                    , T.intercalate "\n" $ fmap (\x ->
                            T.intercalate "/"
                                [ slotNoToText (slot x)
                                , show (txIx x)
                                , show (outIx x)
                                ]
                        ) xs
                    ]

            forAllShrinkShow genConflictingResults shrinkResults showResults $ \results ->
                withMaxSuccess 50 $ monadicIO $ do
                    (asc, desc) <- withInMemoryDatabase 10 $ \Database{..} -> do
                        let matchAll = MatchAny IncludingBootstrap

                        runTransaction $ do
                            insertInputs (resultToRow <$> results)
                            insertCheckpoints (createdAt <$> results)
                            insertCheckpoints (mapMaybe spentAt results)

                        qAsc <- newTBQueueIO (fromIntegral $ length results)
                        runTransaction $ foldInputs matchAll Whole NoStatusFlag AsReference Asc
                            (atomically . writeTBQueue qAsc)

                        qDesc <- newTBQueueIO (fromIntegral $ length results)
                        runTransaction $ foldInputs matchAll Whole NoStatusFlag AsReference Desc
                            (atomically . writeTBQueue qDesc)

                        atomically $ (,) <$> flushTBQueue qAsc <*> flushTBQueue qDesc

                    let pAsc = conjoin (uncurry oldestFirst <$> zip asc (drop 1 asc))
                            & counterexample (showResults asc)
                            & counterexample "\n    Not ordered ASC ↴\n"
                    let pDesc = conjoin (uncurry mostRecentFirst <$> zip desc (drop 1 desc))
                            & counterexample (showResults desc)
                            & counterexample "\n    Not ordered DESC ↴\n"

                    monitor (.&&. pAsc .&&. pDesc)

    context "concurrent read / write" $ do
        let k = LongestRollback { getLongestRollback = 42 }
        mapM_
            (\(title, withDatabasePool) -> do
                specify ("1 long-lived worker vs 2 short-lived workers (" <> title <> ")") $ do
                    withDatabasePool $ \pool -> do
                        waitGroup <- newTVarIO False
                        let allow = atomically (writeTVar waitGroup True)
                        let await = atomically (readTVar waitGroup >>= check)
                        mapConcurrently_ identity
                            [ longLivedWorker pool allow
                            , await >> shortLivedWorker pool ReadOnly
                            , await >> shortLivedWorker pool ReadWrite
                            ]
            )
            [ ( "in-memory"
              , \test -> do
                  test =<< newDBPool nullTracer False
                    (InMemory (Just "file::concurrent-read-write:?cache=shared&mode=memory"))
                    k
              )
            , ( "on-disk"
            , \test ->
                  withSystemTempDirectory "kupo-database-concurrent" $ \dir -> do
                    test =<< newDBPool nullTracer False (Dir dir) k
              )
            ]

    context "efficiency of search and update queries" $ do
        context "with essential indexes only" $ do
            let deferIndexes = SkipNonEssentialIndexes

            specifyQuery "listCheckpoints" deferIndexes
                (pure listCheckpointsQry)
                (`shouldBe`
                    [ "SEARCH checkpoints USING INTEGER PRIMARY KEY (rowid>?)"
                    , "SCALAR SUBQUERY 1"
                    , "SEARCH checkpoints"
                    ]
                )

            specifyQuery "listAncestorQry" deferIndexes
                (pure listAncestorQry)
                (`shouldBe`
                    [ "SEARCH checkpoints USING INTEGER PRIMARY KEY (rowid<?)"
                    ]
                )

            specifyQuery "pruneBinaryData" deferIndexes
                (pure pruneBinaryDataQry)
                (`shouldBe`
                    [ "SEARCH binary_data USING COVERING INDEX sqlite_autoindex_binary_data_1 (binary_data_hash=?)"
                    , "LIST SUBQUERY 1"
                    , "SCAN binary_data USING COVERING INDEX sqlite_autoindex_binary_data_1"
                    , "BLOOM FILTER ON inputs (datum_hash=?)"
                    , "SEARCH inputs USING AUTOMATIC COVERING INDEX (datum_hash=?) LEFT-JOIN"
                    , "USE TEMP B-TREE FOR ORDER BY"
                    ]
                )

            specifyQuery "deleteInputs" deferIndexes
                (deleteInputsQry . MatchOutputReference <$> genOutputReference)
                (`shouldBe`
                    [ "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                    , "SEARCH policies USING COVERING INDEX sqlite_autoindex_policies_1 (output_reference=?)"
                    ]
                )

            specifyQuery "markInputs" deferIndexes
                (markInputsQry . MatchOutputReference <$> genOutputReference)
                (`shouldBe`
                    [ "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                    ]
                )

            specifyQuery "getBinaryData" deferIndexes
                (pure getBinaryDataQry)
                (`shouldBe`
                    [ "SEARCH binary_data USING INDEX sqlite_autoindex_binary_data_1 (binary_data_hash=?)"
                    ]
                )

            specifyQuery "getScript" deferIndexes
                (pure getScriptQry)
                (`shouldBe`
                    [ "SEARCH scripts USING INDEX sqlite_autoindex_scripts_1 (script_hash=?)"
                    ]
                )

            specifyQuery "selectMaxCheckpoint" deferIndexes
                (pure selectMaxCheckpointQry)
                (`shouldBe`
                    [ "SEARCH checkpoints"
                    ]
                )

        context "with temporary indexes" $ do
            let deferIndexes = SkipNonEssentialIndexes

            specifyQueryWith "pruneInputs" deferIndexes
                (pure pruneInputsQry)
                (\conn -> installIndex nullTracer conn "inputsBySpentAt" "inputs(spent_at)")
                (`shouldBe`
                    [ "SEARCH inputs USING COVERING INDEX sqlite_autoindex_inputs_1 (ext_output_reference=?)"
                    , "LIST SUBQUERY 2"
                    , "SEARCH inputs USING INDEX inputsBySpentAt (spent_at<?)"
                    , "SCALAR SUBQUERY 1"
                    , "SEARCH checkpoints"
                    , "SEARCH policies USING COVERING INDEX sqlite_autoindex_policies_1 (output_reference=?)"
                    ]
                )

            specifyQueryWith "rollbackQry (delete inputs)" deferIndexes
                (pure rollbackQryDeleteInputs)
                (\conn -> installIndex nullTracer conn "inputsByCreatedAt" "inputs(created_at)")
                (`shouldBe`
                    [ "SEARCH inputs USING INTEGER PRIMARY KEY (rowid=?)"
                    , "LIST SUBQUERY 1"
                    , "SEARCH inputs USING COVERING INDEX inputsByCreatedAt (created_at>?)"
                    , "SEARCH policies USING COVERING INDEX sqlite_autoindex_policies_1 (output_reference=?)"
                    ]
                )

            specifyQueryWith "rollbackQry (update inputs)" deferIndexes
                (pure rollbackQryUpdateInputs)
                (\conn -> installIndex nullTracer conn "inputsBySpentAt" "inputs(spent_at)")
                (`shouldBe`
                    [ "SEARCH inputs USING INDEX inputsBySpentAt (spent_at>?)"
                    ]
                )

            specifyQuery "rollbackQry (delete checkpoints)" deferIndexes
                (pure rollbackQryDeleteCheckpoints)
                (`shouldBe`
                    [ "SEARCH checkpoints USING INTEGER PRIMARY KEY (rowid>?)"
                    ]
                )

        context "with extra lookup indexes" $ do
            let installIndexes =
                    InstallIndexesIfNotExist

            let suffix =
                    [ "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                    , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?) LEFT-JOIN"
                    , "USE TEMP B-TREE FOR ORDER BY"
                    ]

            let suffixInline =
                    Prelude.init suffix ++
                        [ "SEARCH datums USING INDEX sqlite_autoindex_binary_data_1 (binary_data_hash=?) LEFT-JOIN"
                        , "SEARCH scripts USING INDEX sqlite_autoindex_scripts_1 (script_hash=?) LEFT-JOIN"
                        , Prelude.last suffix
                        ]

            context "pruneBinaryData" $ do
                specifyQuery "pruneBinaryData" installIndexes
                    (pure pruneBinaryDataQry)
                    (`shouldBe`
                        [ "SEARCH binary_data USING COVERING INDEX sqlite_autoindex_binary_data_1 (binary_data_hash=?)"
                        , "LIST SUBQUERY 1"
                        , "SCAN binary_data USING COVERING INDEX sqlite_autoindex_binary_data_1"
                        , "SEARCH inputs USING INDEX inputsByDatumHash (datum_hash=?) LEFT-JOIN"
                        , "USE TEMP B-TREE FOR ORDER BY"
                        ]
                    )

            context "foldInputs / MatchExact" $ do
                specifyQuery "NoStatusFlag" installIndexes
                    (foldInputsQry
                        <$> fmap MatchExact genAddress
                        <*> pure Whole
                        <*> pure NoStatusFlag
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address=?)" : suffix )
                    )

                specifyQuery "NoStatusFlag + InlineAll" installIndexes
                    (foldInputsQry
                        <$> fmap MatchExact genAddress
                        <*> pure Whole
                        <*> pure NoStatusFlag
                        <*> pure InlineAll
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address=?)" : suffixInline )
                    )

                specifyQuery "Created After" installIndexes
                    (foldInputsQry
                        <$> fmap MatchExact genAddress
                        <*> pure (After CreatedAt 14)
                        <*> pure NoStatusFlag
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address=?)" : suffix )
                    )

                specifyQuery "Created After + InlineAll" installIndexes
                    (foldInputsQry
                        <$> fmap MatchExact genAddress
                        <*> pure (After CreatedAt 14)
                        <*> pure NoStatusFlag
                        <*> pure InlineAll
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address=?)" : suffixInline )
                    )

                specifyQuery "Spent Before" installIndexes
                    (foldInputsQry
                        <$> fmap MatchExact genAddress
                        <*> pure (Before SpentAt 14)
                        <*> pure NoStatusFlag
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address=?)" : suffix )
                    )

                specifyQuery "Spent Before + InlineAll" installIndexes
                    (foldInputsQry
                        <$> fmap MatchExact genAddress
                        <*> pure (Before SpentAt 14)
                        <*> pure NoStatusFlag
                        <*> pure InlineAll
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address=?)" : suffixInline )
                    )

                specifyQuery "Created Between" installIndexes
                    (foldInputsQry
                        <$> fmap MatchExact genAddress
                        <*> pure (Between (CreatedAt, 14) (CreatedAt, 42))
                        <*> pure NoStatusFlag
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address=?)" : suffix )
                    )

                specifyQuery "Created Between + InlineAll" installIndexes
                    (foldInputsQry
                        <$> fmap MatchExact genAddress
                        <*> pure (Between (CreatedAt, 14) (CreatedAt, 42))
                        <*> pure NoStatusFlag
                        <*> pure InlineAll
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address=?)" : suffixInline )
                    )

                specifyQuery "Spent Between" installIndexes
                    (foldInputsQry
                        <$> fmap MatchExact genAddress
                        <*> pure (Between (SpentAt, 14) (SpentAt, 42))
                        <*> pure NoStatusFlag
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address=?)" : suffix )
                    )

                specifyQuery "Spent Between + InlineAll" installIndexes
                    (foldInputsQry
                        <$> fmap MatchExact genAddress
                        <*> pure (Between (SpentAt, 14) (SpentAt, 42))
                        <*> pure NoStatusFlag
                        <*> pure InlineAll
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address=?)" : suffixInline )
                    )

                specifyQuery "Created/Spent Between" installIndexes
                    (foldInputsQry
                        <$> fmap MatchExact genAddress
                        <*> pure (Between (CreatedAt, 14) (SpentAt, 42))
                        <*> pure NoStatusFlag
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address=?)" : suffix )
                    )

                specifyQuery "OnlyUnspent" installIndexes
                    (foldInputsQry
                        <$> fmap MatchExact genAddress
                        <*> pure Whole
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address=?)" : suffix )
                    )

                specifyQuery "OnlyUnspent + InlineAll" installIndexes
                    (foldInputsQry
                        <$> fmap MatchExact genAddress
                        <*> pure Whole
                        <*> pure OnlyUnspent
                        <*> pure InlineAll
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address=?)" : suffixInline )
                    )

                specifyQuery "Created Before" installIndexes
                    (foldInputsQry
                        <$> fmap MatchExact genAddress
                        <*> pure (Before CreatedAt 42)
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address=?)" : suffix )
                    )

                specifyQuery "Spent Before" installIndexes
                    (foldInputsQry
                        <$> fmap MatchExact genAddress
                        <*> pure (Before SpentAt 42)
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address=?)" : suffix )
                    )

                specifyQuery "OnlySpent" installIndexes
                    (foldInputsQry
                        <$> fmap MatchExact genAddress
                        <*> pure Whole
                        <*> pure OnlySpent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address=?)" :
                            [ "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                            , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?)"
                            , "USE TEMP B-TREE FOR ORDER BY"
                            ]
                        )
                    )

            context "foldInputs / MatchPayment" $ do
                specifyQuery "NoStatusFlag" installIndexes
                    (foldInputsQry
                        <$> fmap MatchPayment (genBytes 28)
                        <*> pure Whole
                        <*> pure NoStatusFlag
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByPaymentCredential (payment_credential=?)" : suffix )
                    )

                specifyQuery "NoStatusFlag + InlineAll" installIndexes
                    (foldInputsQry
                        <$> fmap MatchPayment (genBytes 28)
                        <*> pure Whole
                        <*> pure NoStatusFlag
                        <*> pure InlineAll
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByPaymentCredential (payment_credential=?)" : suffixInline )
                    )

                specifyQuery "Created Before" installIndexes
                    (foldInputsQry
                        <$> fmap MatchPayment (genBytes 28)
                        <*> pure (Before CreatedAt 42)
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByPaymentCredential (payment_credential=?)" : suffix )
                    )

                specifyQuery "Spent After" installIndexes
                    (foldInputsQry
                        <$> fmap MatchPayment (genBytes 28)
                        <*> pure (After SpentAt 42)
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByPaymentCredential (payment_credential=?)" : suffix )
                    )

                specifyQuery "Created Between" installIndexes
                    (foldInputsQry
                        <$> fmap MatchPayment (genBytes 28)
                        <*> pure (Between (CreatedAt, 14) (CreatedAt, 42))
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByPaymentCredential (payment_credential=?)" : suffix )
                    )

                specifyQuery "Spent Between" installIndexes
                    (foldInputsQry
                        <$> fmap MatchPayment (genBytes 28)
                        <*> pure (Between (SpentAt, 14) (SpentAt, 42))
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByPaymentCredential (payment_credential=?)" : suffix )
                    )

                specifyQuery "Created/Spent Between" installIndexes
                    (foldInputsQry
                        <$> fmap MatchPayment (genBytes 28)
                        <*> pure (Between (CreatedAt, 14) (SpentAt, 42))
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByPaymentCredential (payment_credential=?)" : suffix )
                    )

                specifyQuery "OnlyUnspent" installIndexes
                    (foldInputsQry
                        <$> fmap MatchPayment (genBytes 28)
                        <*> pure Whole
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByPaymentCredential (payment_credential=?)" : suffix )
                    )

                specifyQuery "OnlySpent" installIndexes
                    (foldInputsQry
                        <$> fmap MatchPayment (genBytes 28)
                        <*> pure Whole
                        <*> pure OnlySpent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByPaymentCredential (payment_credential=?)" :
                            [ "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                            , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?)"
                            , "USE TEMP B-TREE FOR ORDER BY"
                            ]
                        )
                    )

            context "foldInputs / MatchDelegation" $ do
                specifyQuery "NoStatusFlag" installIndexes
                    (foldInputsQry
                        <$> fmap MatchDelegation (genBytes 28)
                        <*> pure Whole
                        <*> pure NoStatusFlag
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address>? AND address<?)" : suffix )
                    )

                specifyQuery "NoStatusFlag + InlineAll" installIndexes
                    (foldInputsQry
                        <$> fmap MatchDelegation (genBytes 28)
                        <*> pure Whole
                        <*> pure NoStatusFlag
                        <*> pure InlineAll
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address>? AND address<?)" : suffixInline )
                    )

                specifyQuery "Created After" installIndexes
                    (foldInputsQry
                        <$> fmap MatchDelegation (genBytes 28)
                        <*> pure (After CreatedAt 14)
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address>? AND address<?)" : suffix )
                    )

                specifyQuery "Spent Before" installIndexes
                    (foldInputsQry
                        <$> fmap MatchDelegation (genBytes 28)
                        <*> pure (Before SpentAt 42)
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address>? AND address<?)" : suffix )
                    )

                specifyQuery "Created Between" installIndexes
                    (foldInputsQry
                        <$> fmap MatchDelegation (genBytes 28)
                        <*> pure (Between (CreatedAt, 14) (CreatedAt, 42))
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address>? AND address<?)" : suffix )
                    )

                specifyQuery "Spent Between" installIndexes
                    (foldInputsQry
                        <$> fmap MatchDelegation (genBytes 28)
                        <*> pure (Between (SpentAt, 14) (SpentAt, 42))
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address>? AND address<?)" : suffix )
                    )

                specifyQuery "Created/Spent Between" installIndexes
                    (foldInputsQry
                        <$> fmap MatchDelegation (genBytes 28)
                        <*> pure (Between (SpentAt, 14) (CreatedAt, 42))
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address>? AND address<?)" : suffix )
                    )

                specifyQuery "OnlyUnspent" installIndexes
                    (foldInputsQry
                        <$> fmap MatchDelegation (genBytes 28)
                        <*> pure Whole
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address>? AND address<?)" : suffix )
                    )

                specifyQuery "OnlySpent" installIndexes
                    (foldInputsQry
                        <$> fmap MatchDelegation (genBytes 28)
                        <*> pure Whole
                        <*> pure OnlySpent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByAddress (address>? AND address<?)" :
                            [ "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?)"
                            , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                            , "USE TEMP B-TREE FOR ORDER BY"
                            ]
                        )
                    )

            context "foldInputs / MatchTransactionId" $ do
                specifyQuery "NoStatusFlag" installIndexes
                    (foldInputsQry
                        <$> fmap MatchTransactionId genTransactionId
                        <*> pure Whole
                        <*> pure NoStatusFlag
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByOutputReference (output_reference>? AND output_reference<?)" : suffix )
                    )

                specifyQuery "NoStatusFlag + InlineAll" installIndexes
                    (foldInputsQry
                        <$> fmap MatchTransactionId genTransactionId
                        <*> pure Whole
                        <*> pure NoStatusFlag
                        <*> pure InlineAll
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByOutputReference (output_reference>? AND output_reference<?)" : suffixInline )
                    )

                specifyQuery "Created Before" installIndexes
                    (foldInputsQry
                        <$> fmap MatchTransactionId genTransactionId
                        <*> pure (Before CreatedAt 42)
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByOutputReference (output_reference>? AND output_reference<?)" : suffix )
                    )

                specifyQuery "Spent After" installIndexes
                    (foldInputsQry
                        <$> fmap MatchTransactionId genTransactionId
                        <*> pure (After SpentAt 14)
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByOutputReference (output_reference>? AND output_reference<?)" : suffix )
                    )

                specifyQuery "Created Between" installIndexes
                    (foldInputsQry
                        <$> fmap MatchTransactionId genTransactionId
                        <*> pure (Between (CreatedAt, 14) (CreatedAt, 42))
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByOutputReference (output_reference>? AND output_reference<?)" : suffix )
                    )

                specifyQuery "Spent Between" installIndexes
                    (foldInputsQry
                        <$> fmap MatchTransactionId genTransactionId
                        <*> pure (Between (SpentAt, 14) (SpentAt, 42))
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByOutputReference (output_reference>? AND output_reference<?)" : suffix )
                    )

                specifyQuery "Created/Spent Between" installIndexes
                    (foldInputsQry
                        <$> fmap MatchTransactionId genTransactionId
                        <*> pure (Between (CreatedAt, 14) (SpentAt, 42))
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByOutputReference (output_reference>? AND output_reference<?)" : suffix )
                    )

                specifyQuery "OnlyUnspent" installIndexes
                    (foldInputsQry
                        <$> fmap MatchTransactionId genTransactionId
                        <*> pure Whole
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByOutputReference (output_reference>? AND output_reference<?)" : suffix )
                    )

                specifyQuery "OnlySpent" installIndexes
                    (foldInputsQry
                        <$> fmap MatchTransactionId genTransactionId
                        <*> pure Whole
                        <*> pure OnlySpent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByOutputReference (output_reference>? AND output_reference<?)" :
                            [ "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?)"
                            , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                            , "USE TEMP B-TREE FOR ORDER BY"
                            ]
                        )
                    )

            context "foldInputs / MatchOutputReference" $ do
                specifyQuery "NoStatusFlag" installIndexes
                    (foldInputsQry
                        <$> fmap MatchOutputReference genOutputReference
                        <*> pure Whole
                        <*> pure NoStatusFlag
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        [ "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                        , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?) LEFT-JOIN"
                        ]
                    )

                specifyQuery "NoStatusFlag + InlineAll" installIndexes
                    (foldInputsQry
                        <$> fmap MatchOutputReference genOutputReference
                        <*> pure Whole
                        <*> pure NoStatusFlag
                        <*> pure InlineAll
                        <*> pure Asc
                    )
                    (`shouldBe`
                        [ "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                        , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?) LEFT-JOIN"
                        , "SEARCH datums USING INDEX sqlite_autoindex_binary_data_1 (binary_data_hash=?) LEFT-JOIN"
                        , "SEARCH scripts USING INDEX sqlite_autoindex_scripts_1 (script_hash=?) LEFT-JOIN"
                        ]
                    )

                specifyQuery "Created After" installIndexes
                    (foldInputsQry
                        <$> fmap MatchOutputReference genOutputReference
                        <*> pure (After CreatedAt 14)
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        [ "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                        , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?) LEFT-JOIN"
                        ]
                    )

                specifyQuery "Spent Before" installIndexes
                    (foldInputsQry
                        <$> fmap MatchOutputReference genOutputReference
                        <*> pure (Before SpentAt 42)
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        [ "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                        , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?) LEFT-JOIN"
                        ]
                    )

                specifyQuery "Created Between" installIndexes
                    (foldInputsQry
                        <$> fmap MatchOutputReference genOutputReference
                        <*> pure (Between (CreatedAt, 14) (CreatedAt, 42))
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        [ "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                        , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?) LEFT-JOIN"
                        ]
                    )

                specifyQuery "Spent Between" installIndexes
                    (foldInputsQry
                        <$> fmap MatchOutputReference genOutputReference
                        <*> pure (Between (SpentAt, 14) (SpentAt, 42))
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        [ "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                        , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?) LEFT-JOIN"
                        ]
                    )

                specifyQuery "Created/Spent Between" installIndexes
                    (foldInputsQry
                        <$> fmap MatchOutputReference genOutputReference
                        <*> pure (Between (SpentAt, 14) (CreatedAt, 42))
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        [ "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                        , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?) LEFT-JOIN"
                        ]
                    )

                specifyQuery "OnlyUnspent" installIndexes
                    (foldInputsQry
                        <$> fmap MatchOutputReference genOutputReference
                        <*> pure Whole
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)" : suffix )
                    )

                specifyQuery "OnlySpent" installIndexes
                    (foldInputsQry
                        <$> fmap MatchOutputReference genOutputReference
                        <*> pure Whole
                        <*> pure OnlySpent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        ( "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)" :
                            [ "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?)"
                            , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                            , "USE TEMP B-TREE FOR ORDER BY"
                            ]
                        )
                    )

            context "foldInputs / MatchPolicyId - MatchAssetId" $ do
                specifyQuery "NoStatusFlag" installIndexes
                    (foldInputsQry
                        <$> fmap MatchPolicyId genPolicyId
                        <*> pure Whole
                        <*> pure NoStatusFlag
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        [ "SEARCH policies USING INDEX policiesByPolicyId (policy_id=?)"
                        , "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        ] ++ suffix
                    )

                specifyQuery "NoStatusFlag + InlineAll" installIndexes
                    (foldInputsQry
                        <$> fmap MatchPolicyId genPolicyId
                        <*> pure Whole
                        <*> pure NoStatusFlag
                        <*> pure InlineAll
                        <*> pure Asc
                    )
                    (`shouldBe`
                        [ "SEARCH policies USING INDEX policiesByPolicyId (policy_id=?)"
                        , "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        ] ++ suffixInline
                    )

                specifyQuery "Created Before" installIndexes
                    (foldInputsQry
                        <$> fmap MatchPolicyId genPolicyId
                        <*> pure (Before CreatedAt 42)
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        [ "SEARCH policies USING INDEX policiesByPolicyId (policy_id=?)"
                        , "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        ] ++ suffix
                    )

                specifyQuery "Spent After" installIndexes
                    (foldInputsQry
                        <$> fmap MatchPolicyId genPolicyId
                        <*> pure (After SpentAt 14)
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        [ "SEARCH policies USING INDEX policiesByPolicyId (policy_id=?)"
                        , "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        ] ++ suffix
                    )

                specifyQuery "Created Between" installIndexes
                    (foldInputsQry
                        <$> fmap MatchPolicyId genPolicyId
                        <*> pure (Between (CreatedAt, 14) (CreatedAt, 42))
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        [ "SEARCH policies USING INDEX policiesByPolicyId (policy_id=?)"
                        , "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        ] ++ suffix
                    )

                specifyQuery "Spent Between" installIndexes
                    (foldInputsQry
                        <$> fmap MatchPolicyId genPolicyId
                        <*> pure (Between (SpentAt, 14) (SpentAt, 42))
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        [ "SEARCH policies USING INDEX policiesByPolicyId (policy_id=?)"
                        , "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        ] ++ suffix
                    )

                specifyQuery "Created/Spent Between" installIndexes
                    (foldInputsQry
                        <$> fmap MatchPolicyId genPolicyId
                        <*> pure (Between (CreatedAt, 14) (SpentAt, 42))
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        [ "SEARCH policies USING INDEX policiesByPolicyId (policy_id=?)"
                        , "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        ] ++ suffix
                    )

                specifyQuery "OnlyUnspent" installIndexes
                    (foldInputsQry
                        <$> fmap MatchPolicyId genPolicyId
                        <*> pure Whole
                        <*> pure OnlyUnspent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        [ "SEARCH policies USING INDEX policiesByPolicyId (policy_id=?)"
                        , "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        ] ++ suffix
                    )

                specifyQuery "OnlySpent" installIndexes
                    (foldInputsQry
                        <$> fmap MatchPolicyId genPolicyId
                        <*> pure Whole
                        <*> pure OnlySpent
                        <*> pure AsReference
                        <*> pure Asc
                    )
                    (`shouldBe`
                        [ "SEARCH policies USING INDEX policiesByPolicyId (policy_id=?)"
                        , "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                        , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?)"
                        , "USE TEMP B-TREE FOR ORDER BY"
                        ]
                    )

            context "foldPolicies" $ do
                specifyQuery "MatchAddress" installIndexes
                    (foldPoliciesQry
                        <$> fmap MatchExact genAddress
                    )
                    (`shouldBe`
                        [ "SEARCH inputs USING INDEX inputsByAddress (address=?)"
                        , "SEARCH policies USING COVERING INDEX sqlite_autoindex_policies_1 (output_reference=?)"
                        ]
                    )

                specifyQuery "MatchPayment" installIndexes
                    (foldPoliciesQry
                        <$> fmap MatchPayment (genBytes 28)
                    )
                    (`shouldBe`
                        [ "SEARCH inputs USING INDEX inputsByPaymentCredential (payment_credential=?)"
                        , "SEARCH policies USING COVERING INDEX sqlite_autoindex_policies_1 (output_reference=?)"
                        ]
                    )

                specifyQuery "MatchDelegation" installIndexes
                    (foldPoliciesQry
                        <$> fmap MatchDelegation (genBytes 28)
                    )
                    (`shouldBe`
                        [ "SEARCH inputs USING INDEX inputsByAddress (address>? AND address<?)"
                        , "SEARCH policies USING COVERING INDEX sqlite_autoindex_policies_1 (output_reference=?)"
                        ]
                    )

                specifyQuery "MatchTransactionId" installIndexes
                    (foldPoliciesQry
                        <$> fmap MatchTransactionId genTransactionId
                    )
                    (`shouldBe`
                        [ "SEARCH policies USING COVERING INDEX sqlite_autoindex_policies_1 (output_reference>? AND output_reference<?)"
                        , "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        ]
                    )

                specifyQuery "MatchOutputReference" installIndexes
                    (foldPoliciesQry
                        <$> fmap MatchOutputReference genOutputReference
                    )
                    (`shouldBe`
                        [ "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        , "SEARCH policies USING COVERING INDEX sqlite_autoindex_policies_1 (output_reference=?)"
                        ]
                    )

                specifyQuery "MatchPolicyId" installIndexes
                    (foldPoliciesQry
                        <$> fmap MatchPolicyId genPolicyId
                    )
                    (`shouldBe`
                        [ "SEARCH policies USING INDEX policiesByPolicyId (policy_id=?)"
                        , "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                        ]
                    )

--
-- Workers
--

loudly :: SomeException -> IO ()
loudly e = do
    print e
    throwIO e

longLivedWorker :: DBPool IO -> IO () -> IO ()
longLivedWorker dbPool allow =
    handle loudly $ (withDatabaseExclusiveWriter dbPool) InstallIndexesIfNotExist $ \db -> do
        allow
        loop db 0
  where
    loop :: Database IO -> Int -> IO ()
    loop db@Database{..} = \case
        25 -> pure ()
        n   -> do
            result <- generate (chooseVector (100, 500) genResult)
            runTransaction $ insertInputs (resultToRow <$> result)
            ms <- millisecondsToDiffTime <$> generate (choose (1, 15))
            threadDelay ms
            loop db (next n)

shortLivedWorker :: DBPool IO -> ConnectionType -> IO ()
shortLivedWorker dbPool mode = do
    handle loudly $
        (withDatabaseBlocking dbPool) mode (`loop` 0)
  where
    loop :: Database IO -> Int -> IO ()
    loop db@Database{..} = \case
        25 -> pure ()
        n   -> do
            void $ join $ generate $ frequency $
                [ (10, do
                    pure $ void $ runTransaction listCheckpointsDesc
                  )
                , (2, do
                    pattern_ <- genQueryablePattern
                    status <- elements [NoStatusFlag, OnlySpent, OnlyUnspent]
                    reference <- elements [AsReference, InlineAll]
                    sortDir <- elements [Asc, Desc]
                    pure $ runTransaction $ foldInputs pattern_ Whole status reference sortDir (\_ -> pure ())
                  )
                ]
                ++
                case mode of
                    WriteOnly -> []
                    ReadOnly -> []
                    ReadWrite ->
                        [ (1, do
                            pattern_ <- genQueryablePattern
                            pure $ void $ runTransaction $ deleteInputs [pattern_]
                          )
                        , (1, do
                            pattern_ <- genPattern
                            pure $ runTransaction $ insertPatterns (fromList [pattern_])
                          )
                        , (1, do
                            void . runTransaction . deletePattern <$> genPattern
                          )
                        ]
            ms <- millisecondsToDiffTime <$> generate (choose (15, 50))
            threadDelay ms
            loop db (next n)

--
-- Properties
--

roundtripFromToRow
    :: forall a row.
        ( Show a
        , Show row
        , Eq a
        )
    => Gen a
    -> (a -> row)
    -> (row -> a)
    -> Property
roundtripFromToRow genA toRow fromRow =
    forAllBlind genA $ \a ->
        let row = toRow a in fromRow row == a
        & counterexample ("Row: "  <> show row)
        & counterexample ("Got:  " <> show (fromRow row))
        & counterexample ("Want: " <> show a)

roundtripFromToRow2
    :: forall a k v.
        ( Show a
        , Show k
        , Show v
        , Eq a
        )
    => Gen a
    -> (a -> (k, v))
    -> (k -> v -> a)
    -> Property
roundtripFromToRow2 genA toRow fromRow =
    forAllBlind genA $ \a ->
        let (k, v) = toRow a in fromRow k v == a
        & counterexample ("Row(v): "  <> show v)
        & counterexample ("Row(k): "  <> show k)
        & counterexample ("Got:  " <> show (fromRow k v))
        & counterexample ("Want: " <> show a)

--
-- Helpers
--

specifyQuery
    :: Text
    -> DeferIndexesInstallation
    -> Gen Query
    -> ([Text] -> Expectation)
    -> Spec
specifyQuery title deferIndexes genQry =
    specifyQueryWith title deferIndexes genQry (const $ pure ())

specifyQueryWith
    :: Text
    -> DeferIndexesInstallation
    -> Gen Query
    -> (Connection -> IO ())
    -> ([Text] -> Expectation)
    -> Spec
specifyQueryWith title deferIndexes genQry beforeQuery expect =
    specify (toString title) $
        withInMemoryDatabase' identity deferIndexes 42 $ \Database{..} -> do
            runTransaction $ ReaderT $ \conn -> do
                beforeQuery conn
                explainQuery conn (generateWith 42 genQry) >>= expect

explainQuery
    :: Connection
    -> Query
    -> IO [Text]
explainQuery conn query = do
    fmap extractQueryPlanner <$> query_ @[SQLData] conn ("EXPLAIN QUERY PLAN " <> query)
  where
    extractQueryPlanner = \case
        [_, _, _, SQLText hint] ->
            hint
        unexpectedRow ->
            error $ "whoops, unexpected row from the query planner: " <> show unexpectedRow

withFixtureDatabase :: (Connection -> IO ()) -> IO ()
withFixtureDatabase action = withConnection ":memory:" $ \conn -> do
    withTransaction conn $ do
        execute_ conn
            "CREATE TABLE IF NOT EXISTS inputs (\
            \  address TEXT NOT NULL,\
            \  payment_credential TEXT NOT NULL GENERATED ALWAYS AS (substr(address, -56)) VIRTUAL,\
            \  ext_output_reference BLOB NOT NULL,\
            \  output_reference BLOB NOT NULL GENERATED ALWAYS AS (substr(ext_output_reference, 1, 34)) VIRTUAL\
            \)"
        execute_ conn
            "CREATE TABLE IF NOT EXISTS policies (\
            \  output_reference BLOB NOT NULL,\
            \  policy_id BLOB NOT NULL\
            \)"
        executeMany conn "INSERT INTO inputs VALUES (?, ?)" $
            flip map matches $ \(outRef, out) ->
                ( SQLText (addressToRow (getAddress out))
                , SQLBlob (extendedOutputReferenceToRow outRef)
                )
        executeMany conn "INSERT INTO policies VALUES (?, ?)" $
            flip concatMap matches $ \((outRef, _), out) ->
                [ ( SQLBlob (outputReferenceToRow outRef)
                  , SQLBlob (policyIdToBytes policyId)
                  )
                | policyId <- toList (policies out)
                ]
    action conn
  where
    policies :: Output -> Set PolicyId
    policies = foldrValue
        (\policy _ -> Set.insert policy)
        Set.empty
        .
        getValue

rowToAddress :: HasCallStack => [SQLData] -> Address
rowToAddress = \case
    [SQLText row, _] ->
        addressFromRow row
    _notSqlText ->
        error "rowToAddress: not SQLText"

withInMemoryDatabase
    :: Word64
    -> (Database IO -> IO b)
    -> PropertyM IO b
withInMemoryDatabase =
    withInMemoryDatabase' run InstallIndexesIfNotExist

withInMemoryDatabase'
    :: forall (m :: Type -> Type) b. (Monad m)
    => (forall a. IO a -> m a)
    -> DeferIndexesInstallation
    -> Word64
    -> (Database IO -> IO b)
    -> m b
withInMemoryDatabase' runInIO deferIndexes k action = do
  pool <- runInIO $ newDBPool nullTracer
    False
    (InMemory (Just ":memory:"))
    (LongestRollback { getLongestRollback = k })
  runInIO $ (withDatabaseExclusiveWriter pool) deferIndexes action

forAllCheckpoints
    :: Testable prop
    => Word64
    -> ([Point] -> prop)
    -> Property
forAllCheckpoints k =
    forAllShow
        (genPointsBetween (0, SlotNo (10 * k)))
        (show . fmap getPointSlotNo)
