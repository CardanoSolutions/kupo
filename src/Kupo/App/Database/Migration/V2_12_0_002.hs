--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.Database.Migration.V2_12_0_002
    ( migration
    , windowSlots
    , MalformedCursorException (..)

      -- * Internal
    , renumberSpender
    , SpentInput (..)
    , SpenderOutcome (..)
    ) where

import Kupo.Prelude

import Data.Binary.Get
    ( getWord64be
    , isEmpty
    , runGetOrFail
    )
import Data.Binary.Put
    ( putWord64be
    , runPut
    )
import Database.SQLite.Simple
    ( Connection
    , FromRow (..)
    , Only (..)
    , executeMany
    , execute_
    , field
    , query
    , query_
    )
import Kupo.App.Database.Migration.Definition
    ( BatchRepair (..)
    , BatchResult (..)
    , Migration (..)
    , MigrationAction (..)
    , RepairContext (..)
    )
import Kupo.Control.MonadThrow
    ( MonadThrow (..)
    )
import Kupo.Data.Cardano
    ( InputIndex
    , OutputReference
    , getOutputIndex
    , mkOutputReference
    , transactionIdFromHash
    )
import Kupo.Data.Cardano.SlotNo
    ( SlotNo (..)
    )
import Kupo.Data.Database
    ( outputReferenceToRow
    )
import Kupo.Data.Pattern
    ( MatchBootstrap (IncludingBootstrap)
    , Pattern (..)
    , patternToText
    )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map

migration :: Migration
migration = Migration
    { migrationId = "v2.12.0/002"
    , migrationSummary =
        "Fix spent_at.input_index and redeemer of spends recorded before the cutoff \
        \(numbered from the last input since v2.10.0)"
    , migrationAction = Repair spentInputOrder
    }

-- | The slots that one batch covers: six hours of one-second slots.
windowSlots :: Word64
windowSlots = 21_600

-- Kupo numbered the inputs of a spender from the last one: the input at ledger position p of N
-- stored index N-1-p, and the redeemer of pointer N-1-p. Number them in ledger order instead.
spentInputOrder :: BatchRepair
spentInputOrder = BatchRepair
    { repairDataIntroducedBy = "v2.10.0/001"
    , repairPrecondition = requireCompleteIndex
    , repairEstimate = estimateRepair
    , repairBatch = repairWindow
    }

-- A spender whose inputs are missing from the index cannot be renumbered.
requireCompleteIndex :: Connection -> RepairContext -> IO (Either Text ())
requireCompleteIndex conn RepairContext{repairCompleteIndexConfirmed} = do
    rows <- query conn "SELECT 1 FROM patterns WHERE pattern = ?" (Only matchAll)
    pure $ if
        | null (rows :: [Only Int]) ->
            Left $ needsCompleteIndex <> " This index does not match '*', so resynchronise it instead."
        | not repairCompleteIndexConfirmed ->
            Left $ needsCompleteIndex <> " The database cannot prove it: confirm it with \
            \'kupo db migrate --confirm-complete-index', or resynchronise."
        | otherwise ->
            Right ()
  where
    matchAll = patternToText (MatchAny IncludingBootstrap)
    needsCompleteIndex =
        "The repair needs a complete index, which matches '*', was synchronised from origin and \
        \was never pruned. Kupo cannot repair the spends by fetching their blocks again yet."

estimateRepair :: Connection -> RepairContext -> IO Text
estimateRepair conn RepairContext{repairCutoffSlot} = do
    sizes <- query_ conn "SELECT page_count * page_size FROM pragma_page_count(), pragma_page_size()"
    pure $ "spends up to slot " <> show (unSlotNo repairCutoffSlot)
        <> ", in a database of " <> maybe "unknown size" (renderSize . fromOnly) (listToMaybe sizes)

-- Repair the spends of the next window of slots that holds any. A spender's inputs share their
-- spent_at slot, so a window never splits them.
repairWindow :: Connection -> RepairContext -> Maybe ByteString -> IO BatchResult
repairWindow conn RepairContext{repairCutoffSlot = SlotNo cutoffSlot} cursor = do
    progress@Progress{nextSlot} <- fromMaybe startProgress <$> forM cursor decodeCursor
    -- The same index as Kupo's own; without it, every window scans the whole table.
    -- Kupo installs it itself, but not with --defer-db-indexes: the repair only makes sure it exists.
    execute_ conn "CREATE INDEX IF NOT EXISTS inputsBySpentAt ON inputs(spent_at)"
    firstSpentSlots <- query conn
        "SELECT spent_at FROM inputs WHERE spent_at >= ? AND spent_at <= ? ORDER BY spent_at LIMIT 1"
        (nextSlot, cutoffSlot)
    case firstSpentSlots of
        [] ->
            pure . RepairDone $ summarise progress
        Only firstSlot : _ -> do
            let lastSlot = min cutoffSlot (firstSlot + windowSlots - 1)
            rows <- query conn
                "SELECT rowid, output_reference, spent_by, spent_with FROM inputs \
                \WHERE spent_at >= ? AND spent_at <= ? AND spent_by IS NOT NULL"
                (firstSlot, lastSlot)
            let outcomes = uncurry renumberSpender <$> Map.toList (groupBySpender rows)
            executeMany conn "UPDATE inputs SET spent_by = ?, spent_with = ? WHERE rowid = ?" $
                concat [ updates | Renumbered updates <- outcomes ]
            let progressAfter = foldl' countOutcome progress{nextSlot = lastSlot + 1} outcomes
            pure $ if lastSlot >= cutoffSlot
                then RepairDone $ summarise progressAfter
                else NextBatch $ encodeCursor progressAfter

-- One row of @inputs@ that some transaction spent.
data SpentInput = SpentInput
    { rowId :: !Int64
    , spentInput :: !ByteString
    -- ^ The output reference of the input.
    , spentBy :: !ByteString
    -- ^ The spending transaction id, then the stored input index.
    , spentWith :: !(Maybe ByteString)
    -- ^ The stored redeemer.
    }

instance FromRow SpentInput where
    fromRow = SpentInput <$> field <*> field <*> field <*> field

-- By the bytes of the spending transaction id, so that rows that fail to decode stay together.
groupBySpender :: [SpentInput] -> Map ByteString [SpentInput]
groupBySpender rows =
    Map.fromListWith (flip (<>)) [ (BS.take (digestSize @Blake2b_256) spentBy, [row]) | row@SpentInput{spentBy} <- rows ]

data SpenderOutcome
    = Renumbered ![(ByteString, Maybe ByteString, Int64)]
    -- ^ The new @spent_by@ and @spent_with@ of each row that changes, by rowid.
    | InLedgerOrder
    -- ^ Recorded by a fixed Kupo, after a rollback below the cutoff.
    | Unmatched
    -- ^ The rows do not decode, or their stored indexes match neither order, for example because
    -- the index lacks some of the spender's inputs.

-- Rank the rows by ledger order, the order of the spender's input set. Swap each redeemer with
-- that of the mirror row, whose stored index was the pointer of that redeemer.
renumberSpender :: ByteString -> [SpentInput] -> SpenderOutcome
renumberSpender spenderBytes rows = fromMaybe Unmatched $ do
    spender <- transactionIdFromHash <$> hashFromBytes spenderBytes
    ordered <- fmap snd . sortOn fst <$> traverse decodeSpentInput rows
    let storedIndexes = fst <$> ordered
    let ranks = take (length ordered) [0 ..]
    let mirrorRedeemers = reverse $ spentWith . snd <$> ordered
    if  | storedIndexes == reverse ranks ->
            Just . Renumbered $
                [ (outputReferenceToRow (mkOutputReference spender rank), redeemer, rowId)
                | (rank, (storedIndex, SpentInput{rowId}), redeemer) <- zip3 ranks ordered mirrorRedeemers
                , rank /= storedIndex
                ]
        | storedIndexes == ranks ->
            Just InLedgerOrder
        | otherwise ->
            Nothing

-- The spent input, then its stored index, with the row.
decodeSpentInput :: SpentInput -> Maybe (OutputReference, (InputIndex, SpentInput))
decodeSpentInput row@SpentInput{spentInput, spentBy} = do
    input <- referenceFromRow spentInput
    storedIndex <- getOutputIndex <$> referenceFromRow spentBy
    pure (input, (storedIndex, row))

-- A transaction id, then a big-endian index: how Kupo stores output_reference and spent_by.
referenceFromRow :: ByteString -> Maybe OutputReference
referenceFromRow bytes = do
    let (txIdBytes, indexBytes) = BS.splitAt (digestSize @Blake2b_256) bytes
    txId <- transactionIdFromHash <$> hashFromBytes txIdBytes
    guard $ BS.length indexBytes == 2
    pure . mkOutputReference txId $ BS.foldl' (\index byte -> index * 256 + fromIntegral byte) 0 indexBytes

-- Where the repair stands, and what it did so far.
data Progress = Progress
    { nextSlot :: !Word64
    , renumberedSpenders :: !Word64
    , unmatchedSpenders :: !Word64
    }

startProgress :: Progress
startProgress =
    Progress{nextSlot = 0, renumberedSpenders = 0, unmatchedSpenders = 0}

countOutcome :: Progress -> SpenderOutcome -> Progress
countOutcome progress@Progress{renumberedSpenders, unmatchedSpenders} = \case
    Renumbered [] -> progress
    Renumbered{} -> progress{renumberedSpenders = renumberedSpenders + 1}
    InLedgerOrder -> progress
    Unmatched -> progress{unmatchedSpenders = unmatchedSpenders + 1}

summarise :: Progress -> Text
summarise Progress{renumberedSpenders, unmatchedSpenders} =
    "Renumbered the spent inputs of " <> transactions renumberedSpenders <> "." <> unmatched
  where
    unmatched
        | unmatchedSpenders == 0 = ""
        | otherwise =
            " Left those of " <> transactions unmatchedSpenders <> " as they were, because they \
            \could not be verified: resynchronise to fix them."
    transactions = \case
        1 -> "1 transaction"
        count -> show count <> " transactions"

-- The last slot done, then the counts: three big-endian 64-bit words.
encodeCursor :: Progress -> ByteString
encodeCursor Progress{nextSlot, renumberedSpenders, unmatchedSpenders} =
    BL.toStrict . runPut $ traverse_ putWord64be [nextSlot - 1, renumberedSpenders, unmatchedSpenders]

decodeCursor :: ByteString -> IO Progress
decodeCursor bytes =
    either (const . throwIO $ MalformedCursor bytes) (\(_, _, progress) -> pure progress) $
        runGetOrFail getProgress (BL.fromStrict bytes)
  where
    getProgress = do
        lastSlot <- getWord64be
        progress <- Progress (lastSlot + 1) <$> getWord64be <*> getWord64be
        guard =<< isEmpty
        pure progress

renderSize :: Int64 -> Text
renderSize bytes
    | bytes >= 10 * gibibyte = show (bytes `div` gibibyte) <> " GiB"
    | otherwise = show ((bytes + mebibyte - 1) `div` mebibyte) <> " MiB"
  where
    mebibyte = 1_048_576
    gibibyte = 1_024 * mebibyte

-- | The stored cursor of the repair is not one that it wrote.
newtype MalformedCursorException
    = MalformedCursor ByteString
    deriving Show

instance Exception MalformedCursorException where
    displayException (MalformedCursor bytes) =
        "The cursor of repair v2.12.0/002 is malformed: " <> toString (encodeBase16 bytes) <> "."
