--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kupo.App.Database.Postgres
    (
      -- // TODO: Fix documentation headers
      -- ** Queries
      -- *** Inputs
    deleteInputsQry
    , markInputsQry
    , pruneInputsQry
    , foldInputsQry
      -- ** Policies
    , foldPoliciesQry
      -- *** Checkpoints
    , listCheckpointsQry
    , listAncestorQry
      -- *** Binary Data
    , getBinaryDataQry
    , pruneBinaryDataQry
      -- *** Scripts
    , getScriptQry
      -- *** Rollback
    , selectMaxCheckpointQry
    , rollbackQryDeleteInputs
    , rollbackQryUpdateInputs
    , rollbackQryDeleteCheckpoints

      -- * Setup
    , newDBPool
    , copyDatabase

      -- * Internal
    , installIndexes
    , installIndex

      -- * Tracer
    , TraceDatabase (..)
    ) where

import Kupo.Prelude

import Control.Exception
    ( IOException
    , handle
    , throwIO
    )
import Control.Monad
    ( foldM
    )
import Control.Tracer
    ( Tracer
    , traceWith
    )
import qualified Data.Char as Char
import qualified Data.Set as Set
    ( map
    )
import Database.PostgreSQL.Simple
    ( Connection
    , Only (..)
    , Query
    , execute
    , executeMany
    , execute_
    , query
    , query_
    )
import qualified Database.PostgreSQL.Simple as PG
import GHC.TypeLits
    ( KnownSymbol
    , symbolVal
    )
import Kupo.Data.Cardano
    ( SlotNo (..)
    , slotNoToText
    )
import Kupo.Data.Configuration
    ( DeferIndexesInstallation (..)
    , LongestRollback (..)
    , pruneInputsMaxIncrement
    )
import Kupo.Data.Database
    ( SortDirection (..)
    , patternToSql
    )
import Kupo.Data.Http.SlotRange
    ( Range (..)
    , RangeField (..)
    )
import Kupo.Data.Http.StatusFlag
    ( StatusFlag (..)
    )
import Kupo.Data.Pattern
    ( Pattern (..)
    , patternToText
    )
import Numeric
    ( Floating (..)
    )

import Control.Concurrent
    ( getNumCapabilities
    )
import Data.Pool
    ( Pool
    , defaultPoolConfig
    , destroyAllResources
    , newPool
    , tryWithResource
    , withResource
    )
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TL
import Kupo.App.Database.Types
    ( ConnectionType (..)
    , DBPool (..)
    , Database (..)
    , TraceConnection (..)
    , TraceDatabase (..)
    )
import Kupo.Control.MonadLog
    ( TraceProgress (..)
    )
import qualified Kupo.Data.Configuration as Configuration
import qualified Kupo.Data.Database as DB

data FailedToCreateConnection = FailedToCreateConnection { reason :: FailedToCreateConnectionReason }
  deriving (Show)

instance Exception FailedToCreateConnection

data FailedToCreateConnectionReason
  = SQLiteDirSpecified { path :: !FilePath }
  | SQLiteInMemorySpecified { inMemoryPath :: !(Maybe FilePath) }
  | SomeUnexpectedErrorOccured { error :: !IOException }
  deriving (Show)

-- | Create a Database pool that uses separate pools for `ReadOnly` and `ReadWrite` connections.
-- This function creates a database file if it does not already exist.
newDBPool :: (Tracer IO TraceDatabase) -> Bool -> Configuration.DatabaseLocation -> LongestRollback -> IO (DBPool IO)
newDBPool tr isReadOnly dbLocation longestRollback = do
    maxConnections <-
      (5*) <$> liftIO getNumCapabilities

    connectionPool <- liftIO $ newPool $ defaultPoolConfig
        mkConnection
        (\Database{close} -> close)
        600 -- // TODO: Review concurrency requirements
        maxConnections

    let
        withDB :: forall a b. (Pool (Database IO) -> (Database IO -> IO a) -> IO b) -> ConnectionType -> (Database IO -> IO a) -> IO b
        withDB withRes connType dbAction =
            case connType of
                ReadWrite | isReadOnly -> fail "Cannot acquire a read/write connection on read-only replica"
                WriteOnly -> fail "Impossible: tried to acquire a WriteOnly database?"
                _ -> withRes connectionPool dbAction

      -- // TODO: Acutally do something with defer indexes! And possibly actually provide a preferred connection?
        withDatabaseExclusiveWriter :: DeferIndexesInstallation -> (Database IO -> IO a) -> (IO a)

        destroyResources = destroyAllResources connectionPool
        withDatabaseExclusiveWriter _deferIndexes = withResource connectionPool

    return DBPool { tryWithDatabase = withDB tryWithResource, withDatabaseBlocking = withDB withResource, withDatabaseExclusiveWriter, maxConcurrentReaders = 0, maxConcurrentWriters = maxConnections, destroyResources }

    where
      mkConnection = case dbLocation of
        Configuration.Remote uri -> do
            traceWith tr $ DatabaseConnection ConnectionCreateGeneric
            conn <- PG.connectPostgreSQL (encodeUtf8 $ render uri)
            return $ mkDatabase trConn longestRollback (\dbAction -> dbAction conn)
        Configuration.Dir dir -> liftIO $ do
            traceLocationError
            throwIO (FailedToCreateConnection $ SQLiteDirSpecified dir)
        Configuration.InMemory path -> liftIO $ do
            traceLocationError
            throwIO (FailedToCreateConnection $ SQLiteInMemorySpecified path)

        where
          trConn :: Tracer IO TraceConnection
          trConn = contramap DatabaseConnection tr

          traceLocationError = traceWith tr $ DatabaseLocationInvalid
              { errorMessage = "This binary was compiled to use PostgreSQL and requires a Postgres connection URI. \
                                \Local file paths and in-memory configurations are only valid for binaries compiled for SQLite." }


-- Copy from an existing database into another, using the provided patterns
-- as filter. Note that this only makes sense when the source database's patterns
-- are a superset of the provided patterns.
--
-- This command is meant to quickly bootstrap new indexes from existing larget ones.
-- So, an application can for example keep a background job running to index '*' and,
-- create on-the-fly indexes on specific patterns from that source index.
copyDatabase
    :: (Tracer IO TraceDatabase, Tracer IO TraceProgress)
    -> FilePath
    -> FilePath
    -> Set Pattern
    -> IO ()
copyDatabase = undefined -- // TODO: Implement copyDatabase

--
-- IO
--

mkDatabase
    :: Tracer IO TraceConnection
    -> LongestRollback
    -> (forall a. (Connection -> IO a) -> IO a)
    -> Database IO
mkDatabase tr longestRollback bracketConnection = Database
    { insertInputs = \inputs -> ReaderT $ \conn -> do
        mapM_ (\DB.Input{..} -> do
            insertRow @"inputs" conn 7
                ( extendedOutputReference
                , address
                , value
                , datumInfo
                , refScriptHash
                , (fromIntegral createdAtSlotNo :: Int64)
                , spentAtSlotNo
                )
            case datum of
                Nothing ->
                    pure ()
                Just DB.BinaryData{..} ->
                    insertRow @"binary_data" conn 2
                        ( binaryDataHash
                        , binaryData
                        )
            case refScript of
                Nothing ->
                    pure ()
                Just DB.ScriptReference{..} ->
                    insertRow @"scripts" conn 2
                        ( scriptHash
                        , script
                        )
            )
            inputs

    , deleteInputs = \refs -> ReaderT $ \conn -> do
          withTotalChanges (\pattern -> execute_ conn (deleteInputsQry pattern)) refs

    , markInputs = \(fromIntegral . unSlotNo -> slotNo) refs -> ReaderT $ \conn -> do
          withTotalChanges (\ref ->
            execute conn (markInputsQry ref) (Only (slotNo :: Int64)))
            refs

    , pruneInputs = ReaderT $ \conn -> do
        withTemporaryIndex tr conn "inputsBySpentAt" "inputs(spent_at)" $ do
            traceExecute tr conn pruneInputsQry [ fromIntegral longestRollback :: Int64 ]

    , foldInputs = \pattern_ slotRange statusFlag sortDirection yield -> ReaderT $ \conn -> do
        -- TODO: Allow resolving datums / scripts on demand through LEFT JOIN
        --
        -- See [#21](https://github.com/CardanoSolutions/kupo/issues/21)
        let (datum, refScript) = (Nothing, Nothing)
        (throwIO . UnexpectedRow "foldInputs") `handle`
            PG.forEach_ conn (foldInputsQry pattern_ slotRange statusFlag sortDirection)
                (\(extendedOutputReference
                  , address
                  , value
                  , datumInfo
                  , refScriptHash
                  , ((fromIntegral :: Int64 -> Word64) -> createdAtSlotNo)
                  ,  createdAtHeaderHash
                  , (fmap (fromIntegral :: Int64 -> Word64) -> spentAtSlotNo)
                  , spentAtHeaderHash
                  ) ->
                    yield (DB.resultFromRow DB.Input{..}))


    , countInputs = \pattern_ -> ReaderT $ \conn -> do
      handle (throwIO . UnexpectedRow "countInputs") $
          query_ conn (countInputsQry pattern_) >>= \case
              [Only n] -> pure n
              (length -> n) -> throwIO $ ExpectedSingletonResult "countInputs" n

    , countPolicies = \pattern_ -> ReaderT $ \conn -> do
        handle (throwIO . UnexpectedRow "countPolicies") $
            query_ conn (countPoliciesQry pattern_) >>= \case
                [Only n] -> pure n
                (length -> n) -> throwIO $ ExpectedSingletonResult "countPolicies" n


    , foldPolicies = \pattern_ yield -> ReaderT $ \conn -> do
        handle (throwIO . UnexpectedRow "foldPolicies") $
            PG.forEach_ conn (foldPoliciesQry pattern_) $
                \(outputReference, policyId) -> yield DB.Policy{..}

    , insertPolicies = \policies -> ReaderT $ \conn -> do
        let
            rows = flip Set.map policies $ \DB.Policy{..} ->
                (outputReference, policyId)
        insertRows @"policies" conn 2 rows

    , insertCheckpoints = \cps -> ReaderT $ \conn -> do
        let
            rows = cps <&> \(DB.pointToRow -> DB.Checkpoint{..}) ->
                (checkpointHeaderHash, ((fromIntegral :: Word64 -> Int64) checkpointSlotNo))
        insertRows @"checkpoints" conn 2 rows

    , listCheckpointsDesc = ReaderT $ \conn -> do
        let
            k = fromIntegral longestRollback
            points =
                [ 0, 10 .. k `div` 2 ^ n ]
                ++
                [ k `div` (2 ^ e) | (e :: Integer) <- [ n-1, n-2 .. 0 ] ]
              where
                n = ceiling (log (fromIntegral @_ @Double k))
        fmap (fmap DB.pointFromRow . nubOn DB.checkpointSlotNo . mconcat) $ forM points $ \pt ->
            PG.fold conn listCheckpointsQry [pt :: Int64] [] $
                \xs (checkpointHeaderHash, (fromIntegral :: Int64 -> Word64) -> checkpointSlotNo) ->
                    pure (DB.Checkpoint{..} : xs)

    , listAncestorsDesc = \(SlotNo slotNo) n -> ReaderT $ \conn -> do
        fmap reverse $
            PG.fold conn listAncestorQry ((fromIntegral :: Word64 -> Int32) slotNo, n) [] $
                \xs (checkpointHeaderHash, (fromIntegral :: Int32 -> Word64) -> checkpointSlotNo) ->
                    pure ((DB.pointFromRow DB.Checkpoint{..}) : xs)

    , insertPatterns = \patterns -> ReaderT $ \conn -> do
          insertRows @"patterns" conn 1 $ Set.map (Only . patternToText) patterns

    , deletePattern = \pattern_-> ReaderT $ \conn -> do
          fromIntegral <$>
              execute conn "DELETE FROM patterns WHERE pattern = ?"
                  (Only $ DB.patternToRow pattern_)

    , listPatterns = ReaderT $ \conn -> do
        fmap fromList
            $ PG.fold_ conn "SELECT * FROM patterns" []
            $ \xs (Only x) -> pure (DB.patternFromRow x:xs)

    , insertBinaryData = \bin -> ReaderT $ \conn -> do
        let
            rows = bin <&> \DB.BinaryData{..} ->
                (binaryDataHash, binaryData)
        insertRows @"binary_data" conn 2 rows

    , getBinaryData = \(DB.datumHashToRow -> binaryDataHash) -> ReaderT $ \conn -> do
        handle (throwIO . UnexpectedRow "getBinaryData") $
            query conn getBinaryDataQry (Only binaryDataHash) >>= \case
                [Only binaryData] ->
                    pure $ Just (DB.binaryDataFromRow DB.BinaryData{..})
                [] ->
                    pure $ Nothing
                (length -> n) ->
                    throwIO $ ExpectedSingletonResult "getBinaryData" n

    , pruneBinaryData = ReaderT $ \conn -> do
        traceExecute_ tr conn pruneBinaryDataQry

    , insertScripts = \scripts -> ReaderT $ \conn -> do
        let
            rows = scripts <&> \DB.ScriptReference{..} ->
                (scriptHash, script)
        insertRows @"scripts" conn 2 rows

    , getScript = \(DB.scriptHashToRow -> scriptHash)-> ReaderT $ \conn -> do
        handle (throwIO . UnexpectedRow "getScript") $
            query conn getScriptQry (Only scriptHash) >>= \case
                [Only script] ->
                    pure $ Just (DB.scriptFromRow DB.ScriptReference{..})
                [] ->
                    pure $ Nothing
                (length -> n) ->
                    throwIO $ ExpectedSingletonResult "getScript" n

    , rollbackTo = \(fromIntegral . unSlotNo -> minSlotNo) -> ReaderT $ \conn -> do
        query_ conn selectMaxCheckpointQry >>= \case
            -- NOTE: Rolling back takes quite a bit of time and, when restarting
            -- the application, we'll always be asked to rollback to the
            -- _current tip_. In this case, there's nothing to delete or update,
            -- so we can safely skip it.
            [(currentSlotNo, _ :: ByteString)] | currentSlotNo == minSlotNo -> do
                pure ()
            _otherwise -> do
                withTemporaryIndex tr conn "inputsByCreatedAt" "inputs(created_at)" $ do
                    withTemporaryIndex tr conn "inputsBySpentAt" "inputs(spent_at)" $ void $ do
                        deleteInputsIncrementally tr conn minSlotNo
                        _ <- traceExecute tr conn rollbackQryUpdateInputs [ minSlotNo ]
                        traceExecute tr conn rollbackQryDeleteCheckpoints [ minSlotNo ]
        handle (throwIO . UnexpectedRow (show selectMaxCheckpointQry)) $
            query_ conn selectMaxCheckpointQry >>= \case
                [(fmap (fromIntegral :: Int64 -> Word64) -> Just checkpointSlotNo, Just checkpointHeaderHash)] ->
                    return $ Just (DB.pointFromRow DB.Checkpoint{..})
                [(Nothing, Nothing)] ->
                    return Nothing
                res -> throwIO $ ExpectedSingletonResult (show selectMaxCheckpointQry) (length res)

    , optimize = return () -- // TODO: Review if optimize needs to happen with Postgres. Also determine if this can be hidden within the `Database` implementation.

    , runTransaction = \r -> bracketConnection $ \conn ->
          withTransaction conn (runReaderT r conn)
          -- ^ // TODO: Check this. Do we need a retry in PG? 

    , longestRollback

    , close = do
        traceWith tr ConnectionDestroyGeneric
        bracketConnection PG.close
    }

--
-- Queries
--

deleteInputsQry :: Pattern -> Query
deleteInputsQry pattern_ =
    "DELETE FROM inputs"
    <>  additionalJoin
    <> "WHERE"
    <>  whereClause
  where
    (fromText -> whereClause, fromText . fromMaybe "" -> additionalJoin) = patternToSql pattern_

markInputsQry :: Pattern -> Query
markInputsQry pattern_ =
    "UPDATE inputs SET spent_at = ?"
    <> additionalJoin
    <> "WHERE"
    <> whereClause
  where
    (fromText -> whereClause, fromText . fromMaybe "" -> additionalJoin) = patternToSql pattern_

-- NOTE: This query only prune down a certain number of inputs at a time to keep his time bounded. The
-- query in itself is quite expensive, and on large indexes, may takes several minutes.
--
-- This happens only during garbage collection and prevents the consumer thread from pushing any new
-- block to the database in the meantime. By making the query more incremental, we can allow the
-- consumer thread to preempt the connection in between increments. While the overall garbage
-- collection may be slower, it should not disrupt the application that much (especially because
-- read-only connections can still go through while the GC is happening).
pruneInputsQry :: Query
pruneInputsQry =
    "DELETE FROM inputs \
    \WHERE ext_output_reference IN ( \
    \  SELECT ext_output_reference FROM inputs \
    \    WHERE spent_at < ((SELECT MAX(slot_no) FROM checkpoints) - ?) \
    \    LIMIT " <> show pruneInputsMaxIncrement <> "\
    \)"

foldInputsQry
    :: Pattern
    -> Range SlotNo
    -> StatusFlag
    -> SortDirection
    -> Query
foldInputsQry pattern_ slotRange statusFlag sortDirection = fromText $
     "SELECT \
      \inputs.ext_output_reference, inputs.address, inputs.value, \
      \inputs.datum_info, inputs.script_hash, \
      \inputs.created_at, createdAt.header_hash, \
      \inputs.spent_at, spentAt.header_hash \
    \FROM (" <> inputs <> ") inputs \
    \JOIN checkpoints AS createdAt ON createdAt.slot_no = inputs.created_at \
    \LEFT OUTER JOIN checkpoints AS spentAt ON spentAt.slot_no = inputs.spent_at"
    <> case statusFlag of
        NoStatusFlag -> ""
        OnlyUnspent -> " WHERE spentAt.header_hash IS NULL"
        OnlySpent -> " WHERE spentAt.header_hash IS NOT NULL"
    <> " ORDER BY \
       \inputs.created_at " <> ordering <> ", \
       \inputs.transaction_index " <> ordering <> ", \
       \inputs.output_index " <> ordering
  where
    inputs = "SELECT * FROM inputs "
        <> additionalJoin
        <> " WHERE "
        <> T.intercalate " AND " (
            [ slotRangeToSql slotRange
            , patternWhereClause
            ] & filter (not . T.null)
           )
        <> " ORDER BY \
           \inputs.created_at " <> ordering

    (patternWhereClause, fromMaybe "" -> additionalJoin) =
        patternToSql pattern_

    ordering = case sortDirection of
        Asc -> "ASC"
        Desc -> "DESC"

    slotRangeToSql = \case
        Whole ->
            ""
        After field lowerBound ->
            "+inputs." <> fieldToSql field <> " >= " <> slotNoToText lowerBound
        Before field upperBound ->
            "+inputs." <> fieldToSql field <> " <= " <> slotNoToText upperBound
        Between (lowerField, lowerBound) (upperField, upperBound) | lowerField == upperField ->
            unwords
            ["+inputs."<> fieldToSql lowerField
            , "BETWEEN"
            , slotNoToText lowerBound
            , "AND"
            , slotNoToText upperBound
            ]
        Between (lowerField, lowerBound) (upperField, upperBound) ->
            unwords
            ["+inputs."<> fieldToSql lowerField
            , ">="
            , slotNoToText lowerBound
            , "AND"
            ,"+inputs."<> fieldToSql upperField
            , "<="
            , slotNoToText upperBound
            ]

    fieldToSql = \case
        CreatedAt -> "created_at"
        SpentAt -> "spent_at"

countInputsQry :: Pattern -> Query
countInputsQry pattern_ = fromText $
    "SELECT COUNT(*) FROM inputs "
    <> additionalJoin
    <> " WHERE "
    <> patternWhereClause
  where
    (patternWhereClause, fromMaybe "" -> additionalJoin) =
        patternToSql pattern_

countPoliciesQry :: Pattern -> Query
countPoliciesQry pattern_ =
    "SELECT COUNT(*) \
    \FROM policies \
    \JOIN inputs \
    \ON inputs.output_reference = policies.output_reference \
    \WHERE "
    <> patternWhereClause
  where
    (fromText -> patternWhereClause, _) =
        patternToSql pattern_

foldPoliciesQry :: Pattern -> Query
foldPoliciesQry pattern_ =
    "SELECT policies.output_reference, policy_id \
    \FROM policies \
    \JOIN inputs \
    \ON inputs.output_reference = policies.output_reference \
    \WHERE "
    <> patternWhereClause
  where
    (fromText -> patternWhereClause, _) =
        patternToSql pattern_

listCheckpointsQry :: Query
listCheckpointsQry =
    "SELECT * FROM checkpoints \
    \WHERE slot_no >= ((SELECT MAX(slot_no) FROM checkpoints) - ?) \
    \ORDER BY slot_no ASC \
    \LIMIT 1"

listAncestorQry :: Query
listAncestorQry =
    "SELECT * FROM checkpoints \
    \WHERE slot_no < ? \
    \ORDER BY slot_no DESC \
    \LIMIT ?"

getBinaryDataQry :: Query
getBinaryDataQry =
    "SELECT binary_data FROM binary_data \
    \WHERE binary_data_hash = ? \
    \LIMIT 1"

-- // TODO: Investigate if the 'ORDER BY' clause is necessary in PostgreSQL
-- NOTE: This removes all binary_data that aren't associted with any
-- known input. The 'ORDER BY' at the end may seem pointless but is
-- actually CRUCIAL for the query performance as it forces SQLite to use
-- the availables indexes of both tables on 'data_hash' and
-- 'binary_data_hash'. Without that, this query may take 1h+ on a large
-- database (e.g. mainnet matching '*').
pruneBinaryDataQry :: Query
pruneBinaryDataQry =
    " DELETE FROM binary_data \
    \ WHERE binary_data_hash IN ( \
    \   SELECT binary_data_hash FROM binary_data \
    \   LEFT JOIN inputs \
    \   ON binary_data_hash = inputs.datum_hash \
    \   WHERE inputs.ext_output_reference IS NULL \
    \   ORDER BY inputs.datum_hash \
    \   LIMIT " <> show pruneInputsMaxIncrement <> "\
    \ )"

getScriptQry :: Query
getScriptQry =
    "SELECT script FROM scripts \
    \WHERE script_hash = ? \
    \LIMIT 1"

selectMaxCheckpointQry :: Query
selectMaxCheckpointQry =
    "SELECT MAX(slot_no),header_hash FROM checkpoints"

deleteInputsIncrementally :: Tracer IO TraceConnection -> Connection -> Int64 -> IO ()
deleteInputsIncrementally tr conn minSlotNo = do
    deleted <- traceExecute tr conn rollbackQryDeleteInputs $ Only minSlotNo
    unless (deleted < pruneInputsMaxIncrement) $ deleteInputsIncrementally tr conn minSlotNo

rollbackQryDeleteInputs :: Query
rollbackQryDeleteInputs =
    "DELETE FROM inputs WHERE rowid IN \
    \(SELECT rowid FROM inputs WHERE created_at > ? LIMIT "
    <> show pruneInputsMaxIncrement <> ")"

rollbackQryUpdateInputs :: Query
rollbackQryUpdateInputs =
    "UPDATE inputs SET spent_at = NULL WHERE spent_at > ?"

rollbackQryDeleteCheckpoints :: Query
rollbackQryDeleteCheckpoints =
    "DELETE FROM checkpoints WHERE slot_no > ?"

-- // TODO: Header comment for this section
withTotalChanges :: forall t a. (Foldable t) => (a -> IO Int64) -> t a -> IO Int
withTotalChanges io t =
    fromIntegral <$>
        foldM (\accum a -> (accum +) <$> io a) 0 t

--
-- Indexes
--

-- | Install database indexes if they do not already exists. Database indexes
-- make queries faster but they tend to make overall synchronization faster. Thus, when synchronizing
-- from scratch or over a long window it may be good idea to defer installation of some non-essential
-- database indexes.
installIndexes
    :: Tracer IO TraceDatabase
    -> Connection
    -> DeferIndexesInstallation
    -> IO ()
installIndexes tr conn = \case
    SkipNonEssentialIndexes -> do
        dropIndexIfExists (contramap DatabaseConnection tr) conn "inputsByAddress" False
        dropIndexIfExists (contramap DatabaseConnection tr) conn "inputsByDatumHash" False
        dropIndexIfExists (contramap DatabaseConnection tr) conn "inputsByPaymentCredential" False
        dropIndexIfExists (contramap DatabaseConnection tr) conn "inputsByCreatedAt" False
        dropIndexIfExists (contramap DatabaseConnection tr) conn "inputsBySpentAt" False
        dropIndexIfExists (contramap DatabaseConnection tr) conn "policiesByPolicyId" False
    InstallIndexesIfNotExist -> do
        installIndex tr conn
            "inputsByAddress"
            "inputs(address COLLATE NOCASE)"
        installIndex tr conn
            "inputsByDatumHash"
            "inputs(datum_hash)"
        installIndex tr conn
            "inputsByPaymentCredential"
            "inputs(payment_credential COLLATE NOCASE)"
        installIndex tr conn
            "inputsByCreatedAt"
            "inputs(created_at)"
        installIndex tr conn
            "inputsBySpentAt"
            "inputs(spent_at)"
        installIndex tr conn
            "policiesByPolicyId"
            "policies(policy_id)"

-- Create the given index with some extra logging around it.
installIndex :: Tracer IO TraceDatabase -> Connection -> Text -> Text -> IO ()
installIndex tr conn name definition = do
    indexDoesExist conn name >>= \case
        False -> do
            traceWith tr (DatabaseCreateIndex name)
            void $ execute conn "CREATE INDEX IF NOT EXISTS ? ON ?" (T.unpack name, T.unpack definition)
        True ->
            traceWith tr (DatabaseIndexAlreadyExists name)

withTemporaryIndex :: Tracer IO TraceConnection -> Connection -> Text -> Text -> IO a -> IO a
withTemporaryIndex tr conn name definition action = do
    exists <- indexDoesExist conn name
    unless exists $ traceWith tr (ConnectionCreateTemporaryIndex name)
    _ <- execute conn "CREATE INDEX IF NOT EXISTS ? ON ?" (name, definition)
    unless exists $ traceWith tr (ConnectionCreatedTemporaryIndex name)
    a <- action
    unless exists (dropIndexIfExists tr conn name True)
    return a

-- | Check whether an index exists in the database. Handy to customize the behavior (e.g. logging)
-- depending on whether or not indexes are already there since 'CREATE INDEX IF NOT EXISTS' will not
-- tell whether or not it has indeed created something.
-- // TODO: Validate that this works
indexDoesExist :: Connection -> Text -> IO Bool
indexDoesExist conn name = do
    query conn qry [name] <&> \case
        [Only n] | n > (0 :: Int64) -> True
        _doesNotExist -> False
    where
        qry = "SELECT COUNT(*) FROM pg_indexes WHERE indexname = ?;"

dropIndexIfExists :: Tracer IO TraceConnection -> Connection -> Text -> Bool -> IO ()
dropIndexIfExists tr conn indexName wasTemporary = do
    whenM (indexDoesExist conn indexName) $ traceWith tr $ if wasTemporary
        then ConnectionRemoveTemporaryIndex{indexName}
        else ConnectionRemoveIndex{indexName}
    void $ execute conn "DROP INDEX IF EXISTS ?" [indexName]

--
-- Helpers
--

insertRow
    :: forall tableName r.
        (KnownSymbol tableName, PG.ToRow r)
    => Connection
    -> Int
    -> r
    -> IO ()
insertRow conn len row = insertRows @tableName conn len [row]

insertRows
    :: forall tableName r t.
        (KnownSymbol tableName, PG.ToRow r, Foldable t)
    => Connection
    -> Int
    -> t r
    -> IO ()
insertRows conn len rows =
    void $ executeMany conn (fromString qry) (toList rows)
    where
        qry =
            "INSERT OR IGNORE INTO "
            <> symbolVal (Proxy @tableName)
            <> " VALUES "
            <> mkValuePlaceholders len

mkValuePlaceholders :: Int -> String
mkValuePlaceholders n =
    fromString $ "(" <> intercalate "," (replicate n "?") <> ")"
{-# INLINABLE mkValuePlaceholders #-}

-- See comments in `Kupo.App.Database.SQLite` to see why the
-- postgresql-simple/sqlite-simple `withTransaction` is insufficient
withTransaction :: Connection -> IO a -> IO a
withTransaction conn action =
  mask $ \restore -> do
    _ <- execute_ conn "BEGIN TRANSACTION"
    r <- restore action `onException` rollback
    _ <- commit `onException` rollback
    return r
  where
    commit   = execute_ conn "COMMIT TRANSACTION"
    rollback = execute_ conn "ROLLBACK TRANSACTION"

fromText :: Text -> Query
fromText = fromString . T.unpack

--
-- Exceptions & Tracing
--

traceExecute
    :: PG.ToRow q
    => Tracer IO TraceConnection
    -> Connection
    -> Query
    -> q
    -> IO Int
traceExecute tr conn template qs = do
    traceWith tr $ ConnectionBeginQuery (trim template)
    n <- execute conn template qs
    traceWith tr $ ConnectionExitQuery (trim template)
    return $ fromIntegral n

traceExecute_
    :: Tracer IO TraceConnection
    -> Connection
    -> Query
    -> IO Int
traceExecute_ tr conn template = do
    traceWith tr $ ConnectionBeginQuery (trim template)
    n <- execute_ conn template
    traceWith tr $ ConnectionExitQuery (trim template)
    return $ fromIntegral n

trim :: Query -> LText
trim =
    TL.toLazyText
    .
    snd
    .
    T.foldl
        (\(prevIsSpace, builder) c ->
            let currentIsSpace = Char.isSpace c in
            ( currentIsSpace
            , if currentIsSpace && prevIsSpace
                 then builder
                 else builder <> TL.singleton c
            )
        )
        (False, mempty)
    .
    fromString
    .
    show

-- | Something went wrong when unmarshalling data from the database.
data UnexpectedRowException
    = UnexpectedRow !Text !PG.ResultError
    deriving Show
instance Exception UnexpectedRowException

data ExpectedSingletonResultException
    = ExpectedSingletonResult
        { context :: !Text
        , received :: !Int
        } deriving Show
instance Exception ExpectedSingletonResultException
