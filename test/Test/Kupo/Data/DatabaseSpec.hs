-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module Test.Kupo.Data.DatabaseSpec
    ( spec
    ) where

import Kupo.Prelude

import Data.Maybe
    ( fromJust )
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
import Kupo.Control.MonadAsync
    ( mapConcurrently_ )
import Kupo.Control.MonadDatabase
    ( ConnectionType (..), Database (..), MonadDatabase (..) )
import Kupo.Control.MonadDelay
    ( threadDelay )
import Kupo.Control.MonadLog
    ( nullTracer )
import Kupo.Control.MonadSTM
    ( MonadSTM (..) )
import Kupo.Control.MonadTime
    ( millisecondsToDiffTime )
import Kupo.Data.Cardano
    ( Address, addressFromBytes, addressToBytes )
import Kupo.Data.Database
    ( patternFromRow
    , patternToRow
    , patternToSql
    , pointFromRow
    , pointToRow
    , resultFromRow
    , resultToRow
    )
import System.FilePath
    ( (</>) )
import System.IO.Temp
    ( withSystemTempDirectory )
import Test.Hspec
    ( Spec, around, context, parallel, shouldBe, specify )
import Test.Hspec.QuickCheck
    ( prop )
import Test.Kupo.Data.Generators
    ( chooseVector, genNonGenesisPoint, genPattern, genResult )
import Test.Kupo.Data.Pattern.Fixture
    ( addresses, patterns )
import Test.QuickCheck
    ( Gen, Property, choose, counterexample, forAllBlind, frequency, generate )

spec :: Spec
spec = parallel $ do
    context "fromRow ↔ toRow" $ do
        prop "Result" $
            roundtripFromToRow genResult resultToRow resultFromRow
        prop "Checkpoint" $
            roundtripFromToRow genNonGenesisPoint pointToRow pointFromRow
        prop "Pattern" $
            roundtripFromToRow genPattern patternToRow patternFromRow

    context "patternToSql" $ around withFixtureDatabase $ do
        forM_ patterns $ \(_, p, results) -> do
            let like = patternToSql p
            specify (toString like) $ \conn -> do
                rows <- query_ conn $ "SELECT address, LENGTH(address) as len \
                                      \FROM addresses \
                                      \WHERE address " <> Query like
                sort (rowToAddress <$> rows) `shouldBe` sort results

    context "concurrent read / write" $ do
        specify "1 long-lived worker vs 2 short-lived workers" $ do
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

longLivedWorker :: FilePath -> DBLock IO -> IO () -> IO ()
longLivedWorker dir lock allow =
    withDatabase nullTracer LongLived lock 42 dir $ \db -> do
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
            loop db (succ n)

shortLivedWorker :: FilePath -> DBLock IO -> IO ()
shortLivedWorker dir lock = do
    withDatabase nullTracer ShortLived lock 42 dir (`loop` 0)
  where
    loop :: Database IO -> Int -> IO ()
    loop db@Database{..} = \case
        25 -> pure ()
        n   -> do
            void $ join $ generate $ frequency
                [ (10, do
                    pure $ void $ runTransaction $ listCheckpointsDesc pointFromRow
                  )
                , (2, do
                    p <- genPattern
                    let q = patternToSql p
                    pure $ runTransaction $ foldInputs q (\_ -> pure ())
                  )
                , (1, do
                    p <- genPattern
                    let q = patternToSql p
                    pure $ void $ runImmediateTransaction $ deleteInputsByAddress q
                  )
                , (1, do
                    p <- genPattern
                    pure $ runImmediateTransaction $ insertPatterns [patternToRow p]
                  )
                , (1, do
                    p <- genPattern
                    pure $ void $ runImmediateTransaction $ deletePattern (patternToRow p)
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
        executeMany conn
            "INSERT INTO addresses VALUES (?)"
            (Only . SQLText . encodeBase16 . addressToBytes <$> addresses)
    action conn

rowToAddress :: HasCallStack => [SQLData] -> Address
rowToAddress = \case
    [SQLText txt, _] ->
        fromJust (addressFromBytes (unsafeDecodeBase16 txt))
    _ ->
        error "rowToAddress: not SQLText"
