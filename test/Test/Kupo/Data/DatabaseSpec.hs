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
    ( ConnectionType (..)
    , DBLock
    , Database (..)
    , DatabaseFile (..)
    , createShortLivedConnection
    , deleteInputsQry
    , foldInputsQry
    , getBinaryDataQry
    , getScriptQry
    , listAncestorQry
    , listCheckpointsQry
    , markInputsQry
    , newLock
    , pruneBinaryDataQry
    , pruneInputsQry
    , rollbackQry1
    , rollbackQry2
    , rollbackQry3
    , selectMaxCheckpointQry
    , withLongLivedConnection
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
    ( DeferIndexesInstallation (..)
    , LongestRollback (..)
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
import Kupo.Data.Http.StatusFlag
    ( StatusFlag (..)
    )
import Kupo.Data.Pattern
    ( MatchBootstrap (..)
    , Pattern (..)
    , Result (..)
    )
import System.FilePath
    ( (</>)
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
    , genAssetId
    , genBytes
    , genDatum
    , genExtendedOutputReference
    , genNonGenesisPoint
    , genNonGenesisPointBetween
    , genOutputReference
    , genPattern
    , genPointsBetween
    , genPolicyId
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
            let whereClause = patternToSql p
            let results = sort $ (\(_, out) -> getAddress out) <$> ms
            specify (toString whereClause) $ \conn -> do
                rows <- query_ conn $ "SELECT address, LENGTH(address) as len \
                                      \FROM inputs " <> Query whereClause
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
                        runTransaction $ foldInputs matchAll NoStatusFlag Asc
                            (atomically . writeTBQueue qAsc)

                        qDesc <- newTBQueueIO (fromIntegral $ length results)
                        runTransaction $ foldInputs matchAll NoStatusFlag Desc
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
        mapM_
            (\(title, withDatabaseFile) -> do
                specify ("1 long-lived worker vs 2 short-lived workers (" <> title <> ")") $ do
                    withDatabaseFile $ \file -> do
                        lock <- newLock
                        waitGroup <- newTVarIO False
                        let allow = atomically (writeTVar waitGroup True)
                        let await = atomically (readTVar waitGroup >>= check)
                        mapConcurrently_ identity
                            [ longLivedWorker file lock allow
                            , await >> shortLivedWorker file ReadOnly lock
                            , await >> shortLivedWorker file ReadWrite lock
                            ]
            )
            [ ( "in-memory"
              , \test ->
                    test (InMemory (Just "file::concurrent-read-write:?cache=shared&mode=memory"))
              )
            , ( "on-disk"
              , \test ->
                    withSystemTempDirectory "kupo-database-concurrent" $ \dir ->
                        test (OnDisk (dir </> "db.sqlite3"))
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

            -- NOTE: require on-the-fly index for efficiency
            specifyQuery "pruneInputs" deferIndexes
                (pure pruneInputsQry)
                (`shouldBe`
                    [ "SCAN inputs"
                    , "SCALAR SUBQUERY 1"
                    , "SEARCH checkpoints"
                    , "SEARCH policies USING COVERING INDEX sqlite_autoindex_policies_1 (output_reference=?)"
                    ]
                )

            specifyQuery "pruneBinaryData" deferIndexes
                (pure pruneBinaryDataQry)
                (`shouldBe`
                    [ "SEARCH binary_data USING INDEX sqlite_autoindex_binary_data_1 (binary_data_hash=?)"
                    , "LIST SUBQUERY 1"
                    , "SCAN binary_data USING COVERING INDEX sqlite_autoindex_binary_data_1"
                    , "SEARCH inputs USING AUTOMATIC COVERING INDEX (datum_hash=?)"
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

            -- NOTE: require on-the-fly index for efficiency
            specifyQuery "rollbackQry (1)" deferIndexes
                (pure rollbackQry1)
                (`shouldBe`
                    [ "SCAN inputs"
                    , "SEARCH policies USING COVERING INDEX sqlite_autoindex_policies_1 (output_reference=?)"
                    ]
                )

            -- NOTE: require on-the-fly index for efficiency
            specifyQuery "rollbackQry (2)" deferIndexes
                (pure rollbackQry2)
                (`shouldBe`
                    [ "SCAN inputs"
                    ]
                )

            specifyQuery "rollbackQry (3)" deferIndexes
                (pure rollbackQry3)
                (`shouldBe`
                    [ "SEARCH checkpoints USING INTEGER PRIMARY KEY (rowid>?)"
                    ]
                )

        context "with extra lookup indxes" $ do
            let installIndexes = InstallIndexesIfNotExist

            specifyQuery "pruneInputs" installIndexes
                (pure pruneInputsQry)
                (`shouldBe`
                    [ "SEARCH inputs USING COVERING INDEX inputsBySpentAt (spent_at<?)"
                    , "SCALAR SUBQUERY 1"
                    , "SEARCH checkpoints"
                    , "SEARCH policies USING COVERING INDEX sqlite_autoindex_policies_1 (output_reference=?)"
                    ]
                )

            specifyQuery "rollbackQry (1)" installIndexes
                (pure rollbackQry1)
                (`shouldBe`
                    [ "SEARCH inputs USING COVERING INDEX inputsByCreatedAt (created_at>?)"
                    , "SEARCH policies USING COVERING INDEX sqlite_autoindex_policies_1 (output_reference=?)"
                    ]
                )

            specifyQuery "rollbackQry (2)" installIndexes
                (pure rollbackQry2)
                (`shouldBe`
                    [ "SEARCH inputs USING INDEX inputsBySpentAt (spent_at>?)"
                    ]
                )

            specifyQuery "foldInputs (MatchExact)" installIndexes
                (foldInputsQry <$> fmap MatchExact genAddress <*> pure NoStatusFlag <*> pure Asc)
                (`shouldBe`
                    [ "SEARCH inputs USING INDEX inputsByAddress (address=?)"
                    , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                    , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?)"
                    , "USE TEMP B-TREE FOR ORDER BY"
                    ]
                )

            specifyQuery "foldInputs (MatchExact/OnlySpent)" installIndexes
                (foldInputsQry <$> fmap MatchExact genAddress <*> pure OnlySpent <*> pure Asc)
                (`shouldBe`
                    [ "SEARCH inputs USING INDEX inputsByAddress (address=? AND spent_at>?)"
                    , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                    , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?)"
                    , "USE TEMP B-TREE FOR ORDER BY"
                    ]
                )

            specifyQuery "foldInputs (MatchPayment)" installIndexes
                (foldInputsQry <$> fmap MatchPayment (genBytes 28) <*> pure NoStatusFlag <*> pure Asc)
                (`shouldBe`
                    [ "SEARCH inputs USING INDEX inputsByPaymentCredential (payment_credential=?)"
                    , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                    , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?)"
                    , "USE TEMP B-TREE FOR ORDER BY"
                    ]
                )

            specifyQuery "foldInputs (MatchPayment/OnlySpent)" installIndexes
                (foldInputsQry <$> fmap MatchPayment (genBytes 28) <*> pure OnlySpent <*> pure Asc)
                (`shouldBe`
                    [ "SEARCH inputs USING INDEX inputsByPaymentCredential (payment_credential=? AND spent_at>?)"
                    , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                    , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?)"
                    , "USE TEMP B-TREE FOR ORDER BY"
                    ]
                )

            specifyQuery "foldInputs (MatchDelegation)" installIndexes
                (foldInputsQry <$> fmap MatchDelegation (genBytes 28) <*> pure NoStatusFlag <*> pure Asc)
                (`shouldBe`
                    [ "SEARCH inputs USING INDEX inputsByAddress (address>? AND address<?)"
                    , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                    , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?)"
                    , "USE TEMP B-TREE FOR ORDER BY"
                    ]
                )

            specifyQuery "foldInputs (MatchTransactionId)" installIndexes
                (foldInputsQry <$> fmap MatchTransactionId genTransactionId <*> pure NoStatusFlag <*> pure Asc)
                (`shouldBe`
                    [ "SEARCH inputs USING INDEX inputsByOutputReference (output_reference>? AND output_reference<?)"
                    , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                    , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?)"
                    , "USE TEMP B-TREE FOR ORDER BY"
                    ]
                )

            specifyQuery "foldInputs (MatchOutputReference)" installIndexes
                (foldInputsQry <$> fmap MatchOutputReference genOutputReference <*> pure NoStatusFlag <*> pure Asc)
                (`shouldBe`
                    [ "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                    , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                    , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?)"
                    ]
                )

            specifyQuery "foldInputs (MatchPolicyId)" installIndexes
                (foldInputsQry <$> fmap MatchPolicyId genPolicyId <*> pure NoStatusFlag <*> pure Asc)
                (`shouldBe`
                    [ "SEARCH policies USING COVERING INDEX sqlite_autoindex_policies_1 (output_reference>?)"
                    , "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                    , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                    , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?)"
                    , "USE TEMP B-TREE FOR ORDER BY"
                    ]
                )

            specifyQuery "foldInputs (MatchAssetId)" installIndexes
                (foldInputsQry <$> fmap MatchAssetId genAssetId <*> pure NoStatusFlag <*> pure Asc)
                (`shouldBe`
                    [ "SEARCH policies USING COVERING INDEX sqlite_autoindex_policies_1 (output_reference>?)"
                    , "SEARCH inputs USING INDEX inputsByOutputReference (output_reference=?)"
                    , "SEARCH createdAt USING INTEGER PRIMARY KEY (rowid=?)"
                    , "SEARCH spentAt USING INTEGER PRIMARY KEY (rowid=?)"
                    , "USE TEMP B-TREE FOR ORDER BY"
                    ]
                )

--
-- Workers
--

loudly :: SomeException -> IO ()
loudly e = do
    print e
    throwIO e

longLivedWorker :: DatabaseFile -> DBLock IO -> IO () -> IO ()
longLivedWorker fp lock allow =
    handle loudly $ withLongLivedConnection nullTracer lock 42 fp InstallIndexesIfNotExist $ \db -> do
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

shortLivedWorker :: DatabaseFile -> ConnectionType -> DBLock IO -> IO ()
shortLivedWorker fp mode lock = do
    handle loudly $ bracket
        (createShortLivedConnection nullTracer mode lock 42 fp)
        (\Database{close} -> close)
        (`loop` 0)
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
                    pattern_ <- genPattern
                    status <- elements [NoStatusFlag, OnlySpent, OnlyUnspent]
                    sortDir <- elements [Asc, Desc]
                    pure $ runTransaction $ foldInputs pattern_ status sortDir (\_ -> pure ())
                  )
                ]
                ++
                case mode of
                    ReadOnly -> []
                    ReadWrite ->
                        [ (1, do
                            pattern_ <- genPattern
                            pure $ void $ runTransaction $ deleteInputs (Set.singleton pattern_)
                          )
                        , (1, do
                            pattern_ <- genPattern
                            pure $ runTransaction $ insertPatterns [pattern_]
                          )
                        , (1, do
                            pattern_ <- genPattern
                            pure $ void $ runTransaction $ deletePattern pattern_
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
specifyQuery title deferIndexes genQry expect =
    specify (toString title) $
        withInMemoryDatabase' identity deferIndexes 42 $ \Database{..} -> do
            runTransaction $ ReaderT $ \conn -> do
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
    lock <- runInIO newLock
    runInIO $ withLongLivedConnection
        nullTracer
        lock
        (LongestRollback k)
        (InMemory (Just ":memory:"))
        deferIndexes
        action

forAllCheckpoints
    :: Testable prop
    => Word64
    -> ([Point] -> prop)
    -> Property
forAllCheckpoints k =
    forAllShow
        (genPointsBetween (0, SlotNo (10 * k)))
        (show . fmap getPointSlotNo)
