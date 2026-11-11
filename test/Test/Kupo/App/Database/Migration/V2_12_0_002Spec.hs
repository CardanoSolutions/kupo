-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.App.Database.Migration.V2_12_0_002Spec
    ( spec
    ) where

import Kupo.Prelude

import Control.Tracer
    ( Tracer (..)
    , emit
    )
import Database.SQLite.Simple
    ( Connection
    , FromRow (..)
    , Only (..)
    , SQLData
    , execute
    , field
    , query_
    , withConnection
    )
import Kupo.App.Database
    ( installIndexes
    )
import Kupo.App.Database.Migration
    ( migrateDatabase
    , migrations
    , readStatus
    , runMigrations
    )
import Kupo.App.Database.Migration.Definition
    ( BatchRepair (..)
    , Migration (..)
    , MigrationAction (..)
    )
import Kupo.App.Database.Migration.Status
    ( MigrationStatus (..)
    , StatusReport (..)
    )
import Kupo.App.Database.Migration.V2_12_0_002
    ( windowSlots
    )
import Kupo.App.Database.Types
    ( TraceDatabase (..)
    )
import Kupo.Control.MonadLog
    ( nullTracer
    )
import Kupo.Control.MonadThrow
    ( MonadThrow (..)
    )
import Kupo.Data.Cardano
    ( OutputReference
    , TransactionId
    , mkOutputReference
    , transactionIdToBytes
    )
import Kupo.Data.Configuration
    ( DeferIndexesInstallation (..)
    )
import Kupo.Data.Database
    ( outputReferenceToRow
    )
import Test.Hspec
    ( Spec
    , context
    , parallel
    , shouldBe
    , shouldReturn
    , shouldThrow
    , specify
    )
import Test.Hspec.QuickCheck
    ( prop
    )
import Test.Kupo.Data.Generators
    ( genTransactionId
    )
import Test.QuickCheck
    ( Gen
    , arbitrary
    , choose
    , elements
    , forAll
    , shuffle
    , vectorOf
    , (===)
    )

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Kupo.App.Database.Migration.V2_12_0_002 as V2_12_0_002

spec :: Spec
spec = parallel $ context "v2.12.0/002" $ do
    specify "renumbers the spent inputs of each transaction and swaps their redeemers" $
        withSpends 42 ["*"] (concatMap (storedRows FromLastInput) spends) $ \conn -> do
            (tr, readLog) <- recordRepair
            migrateDatabase tr conn True migrations `shouldReturn` []
            spentRows conn `shouldReturn` sortedRows (concatMap (storedRows InLedgerOrder) spends)
            readLog `shouldReturn` ["batch 1", "Renumbered the spent inputs of 3 transactions."]
            repairRow conn `shouldReturn` [("applied", Nothing)]

    specify "leaves spends after the cutoff, and unspent rows, as they are" $ do
        let afterCutoff = storedRows FromLastInput $
                Spend 0xb1 43 [(0x11, 0), (0x11, 1)] [Just "a0", Just "a1"]
        -- A rollback unmarks the spend of a row, but keeps its spent_by and spent_with.
        let unspent = (\stored -> stored{spentAt = Nothing}) <$> storedRows FromLastInput
                (Spend 0xb2 40 [(0x12, 0), (0x12, 1)] [Just "b0", Nothing])
        let untouched = afterCutoff <> unspent
        withSpends 42 ["*"] (concatMap (storedRows FromLastInput) spends <> untouched) $ \conn -> do
            migrateDatabase nullTracer conn True migrations `shouldReturn` []
            spentRows conn `shouldReturn`
                sortedRows (concatMap (storedRows InLedgerOrder) spends <> untouched)

    specify "leaves the transactions that lack inputs in the index as they are, and counts them" $ do
        let complete = Spend 0xc1 10 [(0x21, 0), (0x21, 1)] [Just "a0", Nothing]
        -- Inputs 0 and 2 of three, then inputs 0 and 1 of three.
        let incomplete =
                [ row (0x22, 0) 10 (0xc2, 2) (Just "b2")
                , row (0x22, 2) 10 (0xc2, 0) (Just "b0")
                , row (0x23, 0) 11 (0xc3, 2) Nothing
                , row (0x23, 1) 11 (0xc3, 1) (Just "c1")
                ]
        withSpends 42 ["*"] (storedRows FromLastInput complete <> incomplete) $ \conn -> do
            (tr, readLog) <- recordRepair
            migrateDatabase tr conn True migrations `shouldReturn` []
            spentRows conn `shouldReturn` sortedRows (storedRows InLedgerOrder complete <> incomplete)
            readLog `shouldReturn`
                [ "batch 1"
                , "Renumbered the spent inputs of 1 transaction. Left those of 2 transactions as \
                  \they were, because they could not be verified: resynchronise to fix them."
                ]

    specify "leaves the transactions already in ledger order as they are" $ do
        -- As a fixed Kupo records them again, after a rollback below the cutoff.
        let recordedAgain = storedRows InLedgerOrder $
                Spend 0xd1 40 [(0x31, 0), (0x31, 1), (0x31, 2)] [Just "a0", Nothing, Just "a2"]
        withSpends 42 ["*"] recordedAgain $ \conn -> do
            (tr, readLog) <- recordRepair
            migrateDatabase tr conn True migrations `shouldReturn` []
            spentRows conn `shouldReturn` sortedRows recordedAgain
            readLog `shouldReturn` ["batch 1", "Renumbered the spent inputs of 0 transactions."]

    specify "stays pending until the operator confirms a complete index" $ do
        let before = concatMap (storedRows FromLastInput) spends
        withSpends 42 ["*"] before $ \conn -> do
            migrateDatabase nullTracer conn False migrations `shouldReturn` ["v2.12.0/002"]
            spentRows conn `shouldReturn` sortedRows before
            repairRow conn `shouldReturn` [("pending", Nothing)]

    specify "stays pending, even when confirmed, when the index does not match *" $ do
        let before = concatMap (storedRows FromLastInput) spends
        withSpends 42 ["*/*"] before $ \conn -> do
            migrateDatabase nullTracer conn True migrations `shouldReturn` ["v2.12.0/002"]
            spentRows conn `shouldReturn` sortedRows before
            repairRow conn `shouldReturn` [("pending", Nothing)]

    specify "repairs in windows of slots, and skips the windows without spends" $ do
        let windowed =
                [ Spend 0xe1 100 [(0x41, 0), (0x41, 1)] [Just "a0", Nothing]
                , Spend 0xe2 (100 + windowSlots - 1) [(0x42, 0), (0x42, 1)] [Nothing, Just "b1"]
                , Spend 0xe3 (100 + windowSlots) [(0x43, 0), (0x43, 1)] [Just "c0", Just "c1"]
                , Spend 0xe4 500_000 [(0x44, 0), (0x44, 1)] [Nothing, Nothing]
                ]
        withSpends 600_000 ["*"] (concatMap (storedRows FromLastInput) windowed) $ \conn -> do
            (tr, readLog) <- recordRepair
            migrateDatabase tr conn True migrations `shouldReturn` []
            spentRows conn `shouldReturn` sortedRows (concatMap (storedRows InLedgerOrder) windowed)
            readLog `shouldReturn`
                [ "batch 1 " <> loggedCursor (100 + windowSlots - 1) 2 0
                , "batch 2 " <> loggedCursor (100 + 2 * windowSlots - 1) 3 0
                , "batch 3 " <> loggedCursor (500_000 + windowSlots - 1) 4 0
                , "batch 4"
                , "Renumbered the spent inputs of 4 transactions."
                ]

    specify "a second run changes nothing" $
        withSpends 42 ["*"] (concatMap (storedRows FromLastInput) spends) $ \conn -> do
            migrateDatabase nullTracer conn True migrations `shouldReturn` []
            before <- snapshot conn
            (tr, readLog) <- recordRepair
            migrateDatabase tr conn True migrations `shouldReturn` []
            snapshot conn `shouldReturn` before
            readLog `shouldReturn` []

    specify "resumes after a crash, with the counts of the batches done" $ do
        let firstSpend = Spend 0xf1 100 [(0x51, 0), (0x51, 1)] [Just "a0", Nothing]
        let unmatched = row (0x52, 0) 100 (0xf2, 2) (Just "b2")
        let secondSpend = Spend 0xf3 30_000 [(0x53, 0), (0x53, 1), (0x53, 2)] [Nothing, Just "c1", Just "c2"]
        let before = storedRows FromLastInput firstSpend <> [unmatched] <> storedRows FromLastInput secondSpend
        withSpends 42_000 ["*"] before $ \conn -> do
            crashing <- crashingInSecondBatch
            migrateDatabase nullTracer conn True (take 11 migrations <> [crashing])
                `shouldThrow` (== SimulatedCrash)
            repairRow conn `shouldReturn` [("pending", Just $ cursorBytes (100 + windowSlots - 1) 1 1)]
            spentRows conn `shouldReturn`
                sortedRows (storedRows InLedgerOrder firstSpend <> [unmatched] <> storedRows FromLastInput secondSpend)
            (tr, readLog) <- recordRepair
            migrateDatabase tr conn True migrations `shouldReturn` []
            spentRows conn `shouldReturn`
                sortedRows (storedRows InLedgerOrder firstSpend <> [unmatched] <> storedRows InLedgerOrder secondSpend)
            readLog `shouldReturn`
                [ "batch 1"
                , "Renumbered the spent inputs of 2 transactions. Left those of 1 transaction as \
                  \they were, because they could not be verified: resynchronise to fix them."
                ]

    specify "estimates its work from the cutoff and the size of the database" $
        withSpends 42 ["*"] [] $ \conn -> do
            StatusReport{reportMigrations} <- readStatus conn migrations
            [ statusEstimate | MigrationStatus{statusId = "v2.12.0/002", statusEstimate} <- reportMigrations ]
                `shouldBe` [Just "spends up to slot 42, in a database of 1 MiB"]

    specify "is skipped, without a warning, on an upgrade from before v2.10.0" $ do
        upgradeFrom 10 `shouldReturn` ([["v2.12.0/002"]], [("pending", Nothing)], ["v2.12.0/002"])
        upgradeFrom 9 `shouldReturn` ([], [("skipped", Nothing)], [])

    specify "leaves the schema of an upgraded database equal to that of a fresh one" $ do
        fresh <- withConnection ":memory:" $ \conn -> do
            runMigrations nullTracer conn migrations
            installIndexes nullTracer conn InstallIndexesIfNotExist
            schema conn
        upgraded <- withSpends 42 ["*"] [] $ \conn -> do
            migrateDatabase nullTracer conn True migrations `shouldReturn` []
            repairRow conn `shouldReturn` [("applied", Nothing)]
            installIndexes nullTracer conn InstallIndexesIfNotExist
            schema conn
        upgraded `shouldBe` fresh

    prop "ranks the inputs of a transaction in the order of its input set" $
        forAll genSpentInputs $ \(transaction, spentOutputRefs) -> do
            let rows = storedInputs transaction spentOutputRefs
            afterRepair rows (V2_12_0_002.renumberSpender (transactionIdToBytes transaction) rows)
                === [ (fromIntegral rank, inLedgerOrder transaction rank) | rank <- [0 .. length rows - 1] ]

-- Spends of one, two, three and four inputs, with redeemers and without.
spends :: [Spend]
spends =
    [ Spend 0xa1 10 [(0x01, 0)] [Just "r0"]
    , Spend 0xa2 10 [(0x02, 0), (0x02, 1)] [Just "s0", Nothing]
    , Spend 0xa3 11 [(0x03, 0), (0x03, 1), (0x03, 2)] [Just "t0", Nothing, Just "t2"]
    , Spend 0xa4 42 [(0x04, 1), (0x05, 0), (0x05, 3), (0x06, 2)] [Nothing, Just "u1", Just "u2", Just "u3"]
    ]

-- A database that a fixed Kupo opened with its latest checkpoint at the cutoff, which registered
-- the repair. Kupo before the fix had stored the rows.
withSpends :: Word64 -> [Text] -> [SpentRow] -> (Connection -> IO a) -> IO a
withSpends cutoff patterns rows action = withConnection ":memory:" $ \conn -> do
    runMigrations nullTracer conn (take 11 migrations)
    execute conn "INSERT INTO checkpoints (header_hash, slot_no) VALUES (x'00', ?)" (Only cutoff)
    forM_ patterns $ \pattern_ ->
        execute conn "INSERT INTO patterns (pattern) VALUES (?)" (Only pattern_)
    -- In reverse, so that the row order is not the ledger order.
    forM_ (reverse rows) $ \SpentRow{input, spentAt, spentBy, spentWith} ->
        execute conn
            "INSERT INTO inputs (ext_output_reference, address, value, created_at, spent_at, spent_by, spent_with) \
            \VALUES (?, 'addr', x'00', 0, ?, ?, ?)"
            (input <> BS.replicate 2 0, spentAt, spentBy, spentWith)
    runMigrations nullTracer conn migrations
    action conn

-- The stored rows, by output reference.
spentRows :: Connection -> IO [SpentRow]
spentRows conn =
    query_ conn "SELECT output_reference, spent_at, spent_by, spent_with FROM inputs ORDER BY output_reference"

sortedRows :: [SpentRow] -> [SpentRow]
sortedRows =
    sortOn input

-- The type, name, table and SQL of each schema object that is not SQLite's own.
schema :: Connection -> IO [(Text, Text, Text, Maybe Text)]
schema conn =
    query_ conn
        "SELECT type, name, tbl_name, sql FROM sqlite_master WHERE name NOT LIKE 'sqlite_%' ORDER BY name"

-- The state and the cursor of the repair.
repairRow :: Connection -> IO [(Text, Maybe ByteString)]
repairRow conn =
    query_ conn "SELECT state, cursor FROM migrations WHERE id = 'v2.12.0/002'"

snapshot :: Connection -> IO ([[SQLData]], [[SQLData]])
snapshot conn =
    (,) <$> query_ conn "SELECT * FROM inputs ORDER BY ext_output_reference"
        <*> query_ conn "SELECT * FROM migrations ORDER BY id"

-- A tracer that records the batches and the summary that the repair runner logs.
recordRepair :: IO (Tracer IO TraceDatabase, IO [Text])
recordRepair = do
    events <- newIORef []
    let record = \case
            DatabaseRepairBatch{batch, cursor} ->
                modifyIORef' events (<> ["batch " <> show batch <> maybe "" (" " <>) cursor])
            DatabaseRepairApplied{summary} ->
                modifyIORef' events (<> [summary])
            _otherEvent ->
                pass
    pure (Tracer $ emit record, readIORef events)

-- The real repair, which crashes in its second batch, after the work, before it commits.
crashingInSecondBatch :: IO Migration
crashingInSecondBatch = do
    batches <- newIORef (0 :: Int)
    let crashing repair@BatchRepair{repairBatch} = repair
            { repairBatch = \conn repairContext batchCursor -> do
                result <- repairBatch conn repairContext batchCursor
                batch <- atomicModifyIORef' batches $ \done -> (done + 1, done + 1)
                when (batch == 2) $ throwIO SimulatedCrash
                pure result
            }
    pure V2_12_0_002.migration
        { migrationAction =
            case migrationAction V2_12_0_002.migration of
                Repair repair -> Repair $ crashing repair
                otherAction -> otherAction
        }

-- Upgrade a database with a checkpoint from a user_version. Return the repairs that each warning
-- names, the state and cursor of the repair, and what kupo db migrate leaves pending.
upgradeFrom :: Int -> IO ([[Text]], [(Text, Maybe ByteString)], [Text])
upgradeFrom userVersion = withConnection ":memory:" $ \conn -> do
    runMigrations nullTracer conn (take userVersion migrations)
    execute conn "INSERT INTO checkpoints (header_hash, slot_no) VALUES (x'00', ?)" (Only (42 :: Word64))
    warnings <- newIORef []
    let record = \case
            DatabasePendingRepairs{pendingRepairs} ->
                modifyIORef' warnings (<> [T.takeWhile (/= ' ') <$> pendingRepairs])
            _otherEvent ->
                pass
    runMigrations (Tracer $ emit record) conn migrations
    (,,) <$> readIORef warnings <*> repairRow conn <*> migrateDatabase nullTracer conn False migrations

-- The inputs of a transaction, as Kupo stored them before the fix. The input at position p of N in
-- the input set gets rowid p, index N-1-p, and the redeemer of that pointer.
storedInputs :: TransactionId -> [OutputReference] -> [V2_12_0_002.SpentInput]
storedInputs transaction spentOutputRefs =
    [ V2_12_0_002.SpentInput
        (fromIntegral rank)
        (outputReferenceToRow spentOutputRef)
        (outputReferenceToRow . mkOutputReference transaction $ fromIntegral storedIndex)
        (Just $ show storedIndex)
    | spentOutputRef <- spentOutputRefs
    , let rank = Set.findIndex spentOutputRef inputSet
    , let storedIndex = length inputSet - 1 - rank
    ]
  where
    inputSet = fromList spentOutputRefs :: Set OutputReference

-- The spent_by and spent_with of the input of rank p in ledger order: index p, and the redeemer
-- of pointer p.
inLedgerOrder :: TransactionId -> Int -> (ByteString, Maybe ByteString)
inLedgerOrder transaction rank =
    (outputReferenceToRow . mkOutputReference transaction $ fromIntegral rank, Just $ show rank)

-- The spent_by and spent_with of each row, by rowid, once the outcome of the repair applies.
afterRepair :: [V2_12_0_002.SpentInput] -> V2_12_0_002.SpenderOutcome -> [(Int64, (ByteString, Maybe ByteString))]
afterRepair rows outcome =
    sort
        [ (rowId, Map.findWithDefault (storedBy, storedWith) rowId changes)
        | V2_12_0_002.SpentInput rowId _ storedBy storedWith <- rows
        ]
  where
    changes = case outcome of
        V2_12_0_002.Renumbered updates ->
            fromList [ (rowId, (newBy, newWith)) | (newBy, newWith, rowId) <- updates ]
        _unchanged ->
            mempty

-- How the runner logs a cursor: the last slot done, then the renumbered and the unmatched
-- transactions so far.
loggedCursor :: Word64 -> Word64 -> Word64 -> Text
loggedCursor lastSlot renumbered unmatched =
    encodeBase16 $ cursorBytes lastSlot renumbered unmatched

cursorBytes :: Word64 -> Word64 -> Word64 -> ByteString
cursorBytes lastSlot renumbered unmatched =
    foldMap (bigEndian 8) [lastSlot, renumbered, unmatched]

data SimulatedCrash = SimulatedCrash
    deriving (Eq, Show)

instance Exception SimulatedCrash

--
-- Fixtures
--

-- A transaction that spends outputs, in ledger order, with a redeemer for each pointer.
data Spend = Spend
    { spender :: !Word8
    , slot :: !Word64
    , spentOutputs :: ![(Word8, Word16)]
    , redeemers :: ![Maybe ByteString]
    }

-- One row of @inputs@, as stored: the output reference, and the spend if any.
data SpentRow = SpentRow
    { input :: !ByteString
    , spentAt :: !(Maybe Word64)
    , spentBy :: !(Maybe ByteString)
    , spentWith :: !(Maybe ByteString)
    }
    deriving (Eq, Show)

instance FromRow SpentRow where
    fromRow = SpentRow <$> field <*> field <*> field <*> field

data Numbering
    = FromLastInput
    -- ^ As Kupo numbered spent inputs from v2.10.0 until the fix.
    | InLedgerOrder

-- The rows of the spent outputs of a transaction, numbered from the last input or in ledger order.
storedRows :: Numbering -> Spend -> [SpentRow]
storedRows numbering Spend{spender, slot, spentOutputs, redeemers} =
    [ row output slot (spender, index) (join $ redeemers !!? fromIntegral index)
    | (position, output) <- zip [0 ..] spentOutputs
    , let index = case numbering of
            FromLastInput -> fromIntegral (length spentOutputs) - 1 - position
            InLedgerOrder -> position
    ]

-- A spent output, the slot of its spend, its spender and stored index, and the stored redeemer.
row :: (Word8, Word16) -> Word64 -> (Word8, Word16) -> Maybe ByteString -> SpentRow
row output spentAtSlot storedSpend redeemer =
    SpentRow
        { input = reference output
        , spentAt = Just spentAtSlot
        , spentBy = Just $ reference storedSpend
        , spentWith = redeemer
        }

-- A transaction id of 32 equal bytes, then a big-endian index.
reference :: (Word8, Word16) -> ByteString
reference (txId, index) =
    BS.replicate 32 txId <> bigEndian 2 index

bigEndian :: Integral a => Int -> a -> ByteString
bigEndian size n =
    BS.pack [ fromIntegral (toInteger n `div` (256 ^ k)) | k <- reverse [0 .. size - 1] ]

--
-- Generators
--

-- A transaction, and one to six of the inputs that it spends, distinct and in random order. The
-- inputs share one to three transaction ids, so that their output indexes decide their order too.
genSpentInputs :: Gen (TransactionId, [OutputReference])
genSpentInputs = do
    transaction <- genTransactionId
    txIds <- choose (1, 3) >>= (`vectorOf` genTransactionId)
    inputCount <- choose (1, 6)
    spentOutputRefs <- vectorOf inputCount $ mkOutputReference <$> elements txIds <*> arbitrary
    (transaction,) <$> shuffle (ordNub spentOutputRefs)
