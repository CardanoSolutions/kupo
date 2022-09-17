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
    , Only (..)
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
    , newLock
    , withDatabase
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
    , OutputReference
    , Point
    , SlotNo (..)
    , getAddress
    , getPointSlotNo
    )
import Kupo.Data.Configuration
    ( LongestRollback (..)
    )
import Kupo.Data.Database
    ( addressFromRow
    , addressToRow
    , datumFromRow
    , datumToRow
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
import Kupo.Data.Pattern
    ( Pattern (..)
    )
import System.FilePath
    ( (</>)
    )
import System.IO.Temp
    ( withSystemTempDirectory
    )
import Test.Hspec
    ( Spec
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
    , genDatum
    , genNonGenesisPoint
    , genPattern
    , genPointsBetween
    , genResult
    , genScriptReference
    )
import Test.Kupo.Data.Pattern.Fixture
    ( matches
    , patterns
    )
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , counterexample
    , forAllBlind
    , forAllShow
    , frequency
    , generate
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
        prop "Datum" $
            roundtripFromToRow2 genDatum datumToRow datumFromRow
        prop "ScriptReference" $
            roundtripFromToRow2 genScriptReference scriptReferenceToRow scriptReferenceFromRow

    context "patternToSql" $ around withFixtureDatabase $ do
        forM_ patterns $ \(_, p, ms) -> do
            let queryLike = patternToSql p

            let searchAddresses = do
                    let results = sort $ (\(_, out) -> getAddress out) <$> ms
                    specify (toString queryLike) $ \conn -> do
                        rows <- query_ conn $ "SELECT address, LENGTH(address) as len \
                                              \FROM addresses \
                                              \WHERE " <> Query queryLike
                        sort (rowToAddress <$> rows) `shouldBe` results

            let searchOutputReferences = do
                    let results = sort $ fst <$> ms
                    specify (toString queryLike) $ \conn -> do
                        rows <- query_ conn $ "SELECT output_reference \
                                              \FROM output_references \
                                              \WHERE " <> Query queryLike
                        sort (rowToOutputReference <$> rows) `shouldBe` results

            case p of
                MatchAny{} ->
                    searchAddresses
                MatchExact{} ->
                    searchAddresses
                MatchPayment{} ->
                    searchAddresses
                MatchDelegation{} ->
                    searchAddresses
                MatchPaymentAndDelegation{} ->
                    searchAddresses
                MatchTransactionId{} ->
                    searchOutputReferences
                MatchOutputReference{} ->
                    searchOutputReferences
                MatchPolicyId{} ->
                    searchAddresses
                MatchAssetId{} ->
                    searchAddresses

    context "checkpoints" $ do
        let k = 100
        prop "list checkpoints after inserting them" $
            forAllCheckpoints k $ \pts -> monadicIO $ do
                cps <- withInMemoryDatabase k $ \Database{..} -> do
                    runReadOnlyTransaction $ insertCheckpoints (pointToRow <$> pts)
                    runReadOnlyTransaction $ fmap getPointSlotNo <$> listCheckpointsDesc pointFromRow
                monitor $ counterexample (show cps)
                assert $ all (uncurry (>)) (zip cps (drop 1 cps))
                assert $ Prelude.head cps == maximum (getPointSlotNo <$> pts)

        prop "get ancestor of any checkpoint" $
            forAllCheckpoints k $ \pts -> monadicIO $ do
                oneByOne <- withInMemoryDatabase k $ \Database{..} -> do
                    runReadOnlyTransaction $ insertCheckpoints (pointToRow <$> pts)
                    fmap mconcat $ runReadOnlyTransaction $ forM pts $ \pt -> do
                        let slotNo = unSlotNo (getPointSlotNo pt)
                        listAncestorsDesc slotNo 1 pointFromRow

                allAtOnce <- withInMemoryDatabase k $ \Database{..} -> do
                    runReadOnlyTransaction $ insertCheckpoints (pointToRow <$> pts)
                    fmap reverse $ runReadOnlyTransaction $ do
                        let slotNo = unSlotNo (maximum (getPointSlotNo <$> pts))
                        listAncestorsDesc slotNo (fromIntegral $ length pts) pointFromRow

                monitor $ counterexample $ toString $ unlines
                    [ "one-by-one:  " <> show (getPointSlotNo <$> oneByOne)
                    , "all-at-once: " <> show (getPointSlotNo <$> allAtOnce)
                    ]

                assert (Prelude.init pts == oneByOne)
                assert (oneByOne == allAtOnce)

    context "concurrent read / write" $ do
        specify "1 long-lived worker vs 2 short-lived workers (in-memory)" $ do
            lock <- newLock
            waitGroup <- newTVarIO False
            let allow = atomically (writeTVar waitGroup True)
            let await = atomically (readTVar waitGroup >>= check)
            let filename = "file:concurrent-read-write?cache=shared&mode=memory"
            mapConcurrently_ identity
                [ longLivedWorker filename lock allow
                , await >> shortLivedWorker filename lock
                , await >> shortLivedWorker filename lock
                ]

        specify "1 long-lived worker vs 2 short-lived workers (filesystem)" $ do
            withSystemTempDirectory "kupo-database-concurrent" $ \dir -> do
                lock <- newLock
                waitGroup <- newTVarIO False
                let allow = atomically (writeTVar waitGroup True)
                let await = atomically (readTVar waitGroup >>= check)
                mapConcurrently_ identity
                    [ longLivedWorker  (dir </> "db.sqlite3") lock allow
                    , await >> shortLivedWorker (dir </> "db.sqlite3") lock
                    , await >> shortLivedWorker (dir </> "db.sqlite3") lock
                    ]

--
-- Workers
--

loudly :: SomeException -> IO ()
loudly e = do
    print e
    throwIO e

longLivedWorker :: FilePath -> DBLock IO -> IO () -> IO ()
longLivedWorker dir lock allow =
    handle loudly $ withDatabase nullTracer LongLived lock 42 dir $ \db -> do
        allow
        loop db 0
  where
    loop :: Database IO -> Int -> IO ()
    loop db@Database{..} = \case
        25 -> pure ()
        n   -> do
            result <- generate (chooseVector (100, 500) genResult)
            runReadOnlyTransaction $ insertInputs (resultToRow <$> result)
            ms <- millisecondsToDiffTime <$> generate (choose (1, 15))
            threadDelay ms
            loop db (succ n)

shortLivedWorker :: FilePath -> DBLock IO -> IO ()
shortLivedWorker dir lock = do
    handle loudly $ withDatabase nullTracer ShortLived lock 42 dir (`loop` 0)
  where
    loop :: Database IO -> Int -> IO ()
    loop db@Database{..} = \case
        25 -> pure ()
        n   -> do
            void $ join $ generate $ frequency
                [ (10, do
                    pure $ void $ runReadOnlyTransaction $ listCheckpointsDesc pointFromRow
                  )
                , (2, do
                    p <- genPattern
                    let q = patternToSql p
                    pure $ runReadOnlyTransaction $ foldInputs q (\_ -> pure ())
                  )
                , (1, do
                    p <- genPattern
                    let q = patternToSql p
                    pure $ void $ runReadWriteTransaction $ deleteInputsByAddress q
                  )
                , (1, do
                    p <- genPattern
                    pure $ runReadWriteTransaction $ insertPatterns [patternToRow p]
                  )
                , (1, do
                    p <- genPattern
                    pure $ void $ runReadWriteTransaction $ deletePattern (patternToRow p)
                  )
                ]
            ms <- millisecondsToDiffTime <$> generate (choose (15, 50))
            threadDelay ms
            loop db (succ n)

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

withFixtureDatabase :: (Connection -> IO ()) -> IO ()
withFixtureDatabase action = withConnection ":memory:" $ \conn -> do
    withTransaction conn $ do
        execute_ conn
            "CREATE TABLE IF NOT EXISTS addresses (\
            \  address TEXT NOT NULL\
            \)"
        execute_ conn
            "CREATE TABLE IF NOT EXISTS output_references (\
            \  output_reference BLOB NOT NULL\
            \)"
        executeMany conn
            "INSERT INTO addresses VALUES (?)"
            (Only . SQLText . addressToRow <$> (getAddress . snd <$> matches))
        executeMany conn
            "INSERT INTO output_references VALUES (?)"
            (Only . outputReferenceToRow <$> (fst <$> matches))
    action conn

rowToAddress :: HasCallStack => [SQLData] -> Address
rowToAddress = \case
    [SQLText row, _] ->
        addressFromRow row
    _ ->
        error "rowToAddress: not SQLText"

outputReferenceToRow :: OutputReference -> SQLData
outputReferenceToRow =
    SQLBlob . serialize'

rowToOutputReference :: HasCallStack => [SQLData] -> OutputReference
rowToOutputReference = \case
    [SQLBlob row] ->
        unsafeDeserialize' row
    _ ->
        error "rowToOutputReference: not SQLBlob"

withInMemoryDatabase
    :: Word64
    -> (Database IO -> IO b)
    -> PropertyM IO b
withInMemoryDatabase k action = do
    lock <- run newLock
    run $ withDatabase
        nullTracer
        LongLived
        lock
        (LongestRollback k)
        ":memory:"
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
