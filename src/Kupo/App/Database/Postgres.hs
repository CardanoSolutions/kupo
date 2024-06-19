--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Kupo.App.Database.Postgres
    ( -- ** Queries
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
    , withDBPool
    , copyDatabase

      -- * Internal
    , installIndexes
    , installIndex

      -- * Tracer
    , TraceDatabase (..)

      -- * Test Helpers
    , withTestDatabase
    ) where

import Kupo.Prelude

import Control.Concurrent
    ( getNumCapabilities
    )
import Control.Exception
    ( IOException
    , handle
    , mask
    , onException
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
import Data.FileEmbed
    ( embedFile
    )
import Data.Maybe
    ( fromJust
    )
import Data.Pool
    ( Pool
    , defaultPoolConfig
    , destroyAllResources
    , newPool
    , tryWithResource
    , withResource
    )
import qualified Data.Set as Set
    ( map
    )
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TL
import Database.PostgreSQL.Simple
    ( Binary (..)
    , Connection
    , Only (..)
    , Query
    , SqlError (..)
    , execute
    , executeMany
    , execute_
    , query
    , query_
    )
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types
    ( Identifier (..)
    )
import GHC.TypeLits
    ( KnownSymbol
    , symbolVal
    )
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
import Kupo.Control.MonadThrow
    ( bracket
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
import qualified Kupo.Data.Configuration as Configuration
import Kupo.Data.Database
    ( SortDirection (..)
    , patternToSql
    )
import qualified Kupo.Data.Database as DB
import Kupo.Data.Http.SlotRange
    ( Range (..)
    , RangeField (..)
    )
import Kupo.Data.Http.StatusFlag
    ( StatusFlag (..)
    )
import Kupo.Data.Pattern
    ( Pattern (..)
    )
import Numeric
    ( Floating (..)
    )
import qualified Text.URI as URI

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
withDBPool
    :: (Tracer IO TraceDatabase)
    -> Bool
    -> Configuration.DatabaseLocation
    -> LongestRollback
    -> (DBPool IO -> IO a)
    -> IO a
withDBPool tr isReadOnly dbLocation longestRollback action = do
    bracket mkPool destroy action
    where
        mkPool = do
            maxConnections <-
                (5*) <$> liftIO getNumCapabilities

            -- TODO: Will running migrations here break things in a multi-client scenario?
            bracket mkConnection PG.close $ runMigrations tr
            pool <- liftIO . newPool $ defaultPoolConfig
                connectDb
                (\Database{close} -> close)
                600 -- TODO: Review concurrency requirements
                maxConnections

            return DBPool
                { tryWithDatabase = withDB pool tryWithResource
                , withDatabaseBlocking = withDB pool withResource
                , withDatabaseExclusiveWriter = withDatabaseExclusiveWriter pool
                , maxConcurrentReaders = 0
                , maxConcurrentWriters = maxConnections
                , destroy = destroyAllResources pool
                }

        withDB
            :: Pool (Database IO)
            -> (Pool (Database IO) -> (Database IO -> IO a) -> IO b)
            -> ConnectionType
            -> (Database IO -> IO a)
            -> IO b
        withDB pool withRes connType dbAction =
            case connType of
                ReadWrite | isReadOnly -> fail "Cannot acquire a read/write connection on read-only replica"
                WriteOnly -> fail "Impossible: tried to acquire a WriteOnly database?"
                _ -> withRes pool dbAction

        withDatabaseExclusiveWriter
            :: Pool (Database IO)
            -> DeferIndexesInstallation
            -> (Database IO -> IO a)
            -> IO a
        withDatabaseExclusiveWriter pool deferIndexes exclusiveWriterAction = do
            bracket mkConnection PG.close $ \conn -> installIndexes tr conn deferIndexes
            withResource pool exclusiveWriterAction
            -- ^ TODO: Review if any of the PRAGMAs in the SQLite `withLongLivedConnection`
            -- need equivalents here

        connectDb = mkConnection <&> \conn -> mkDatabase trConn longestRollback (\dbAction -> dbAction conn)

        mkConnection = case dbLocation of
            Configuration.Remote uri -> do
                traceWith tr $ DatabaseConnection ConnectionCreateGeneric
                PG.connectPostgreSQL . encodeUtf8 $ URI.render uri
            Configuration.Dir dir -> liftIO $ do
                traceLocationError
                throwIO (FailedToCreateConnection $ SQLiteDirSpecified dir)
            Configuration.InMemory path -> liftIO $ do
                traceLocationError
                throwIO (FailedToCreateConnection $ SQLiteInMemorySpecified path)

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
copyDatabase = undefined -- TODO: Implement copyDatabase

--
-- IO
--

mkDatabase
    :: Tracer IO TraceConnection
    -> LongestRollback
    -> (forall a. (Connection -> IO a) -> IO a)
    -> Database IO
mkDatabase tr longestRollback bracketConnection = Database
    { insertInputs = \inputs -> ReaderT $ \conn ->
        handle (throwIO . DatabaseException "insertInptus" ) $ do
            mapM_ (\DB.Input{..} -> do
                insertRow @"inputs" conn 7
                    ( Binary extendedOutputReference
                    , address
                    , Binary value
                    , Binary <$> datumInfo
                    , Binary <$> refScriptHash
                    , (fromIntegral createdAtSlotNo :: Int64)
                    , spentAtSlotNo
                    )
                case datum of
                    Nothing ->
                        pure ()
                    Just DB.BinaryData{..} ->
                        insertRow @"binary_data" conn 2
                            ( Binary binaryDataHash
                            , Binary binaryData
                            )
                case refScript of
                    Nothing ->
                        pure ()
                    Just DB.ScriptReference{..} ->
                        insertRow @"scripts" conn 2
                            ( Binary scriptHash
                            , Binary script
                            )
                )
                inputs

    , deleteInputs = \refs -> ReaderT $ \conn ->
        handle (throwIO . DatabaseException "deleteInputs") $ do
            withTotalChanges
                (\pattern -> do
                    execute_ conn (markInputsQry pattern))
                refs
                -- ^ TODO: Try to convert this to an `executeMany` call
                -- Since `markInputsQry` creates a few different queries
                -- we may have to group the queries into those with equivalent
                -- forms

    , markInputs = \(fromIntegral . unSlotNo -> slotNo) refs -> ReaderT $ \conn ->
        handle (throwIO . DatabaseException "markInputs") $ do
            withTotalChanges (\pattern -> do
              execute conn (markInputsQry pattern) $ Only (slotNo :: Int64))
              refs
              -- ^ TODO: Try to convert this to an `executeMany` call
              -- Since `markInputsQry` creates a few different queries
              -- we may have to group the queries into those with equivalent
              -- forms


    , pruneInputs = ReaderT $ \conn ->
        handle (throwIO . DatabaseException "pruneInputs") $ do
            withTemporaryIndex tr conn "inputsBySpentAt" "inputs" "spent_at" $ do
                traceExecute tr conn pruneInputsQry [ fromIntegral longestRollback :: Int64 ]

    , foldInputs = \pattern_ slotRange statusFlag sortDirection yield ->
          ReaderT $ \conn -> do
        -- TODO: Allow resolving datums / scripts on demand through LEFT JOIN
        --
        -- See [#21](https://github.com/CardanoSolutions/kupo/issues/21)
        let (datum, refScript) = (Nothing, Nothing)
        handle (throwIO . DatabaseException "foldInputs") $
            PG.forEach_ conn (foldInputsQry pattern_ slotRange statusFlag sortDirection)
                (\(Binary extendedOutputReference
                  , address
                  , Binary value
                  , fmap fromBinary -> datumInfo
                  , fmap fromBinary -> refScriptHash
                  , ((fromIntegral :: Int64 -> Word64) -> createdAtSlotNo)
                  ,  Binary createdAtHeaderHash
                  , (fmap (fromIntegral :: Int64 -> Word64) -> spentAtSlotNo)
                  , fmap fromBinary -> spentAtHeaderHash
                  ) ->
                    yield (DB.resultFromRow DB.Input{..}))


    , countInputs = \pattern_ -> ReaderT $ \conn -> do
        handle (throwIO . DatabaseException "countInputs") $ do
            query_ conn (countInputsQry pattern_) >>= \case
                [Only n] -> pure n
                (length -> n) -> throwIO $ ExpectedSingletonResult "countInputs" n

    , countPolicies = \pattern_ -> ReaderT $ \conn -> do
        handle (throwIO . DatabaseException "countPolicies") $
            query_ conn (countPoliciesQry pattern_) >>= \case
                [Only n] -> pure n
                (length -> n) -> throwIO $ ExpectedSingletonResult "countPolicies" n


    , foldPolicies = \pattern_ yield -> ReaderT $ \conn -> do
        handle (throwIO . DatabaseException "foldPolicies") $
            PG.forEach_ conn (foldPoliciesQry pattern_) $
                \(Binary outputReference, Binary policyId) -> yield DB.Policy{..}

    , insertPolicies = \policies -> ReaderT $ \conn ->
        handle (throwIO . DatabaseException "insertPolicies") $ do
            let
                rows = flip Set.map policies $ \DB.Policy{..} ->
                    (Binary outputReference, Binary policyId)
            insertRows @"policies" conn 2 rows

    , insertCheckpoints = \cps -> ReaderT $ \conn ->
        handle (throwIO . DatabaseException "insertCheckpoints") $ do
            let
                rows = cps <&> \(DB.pointToRow -> DB.Checkpoint{..}) ->
                    (Binary checkpointHeaderHash, ((fromIntegral :: Word64 -> Int64) checkpointSlotNo))
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
        handle (throwIO . DatabaseException "listCheckpointsDesc") $
            fmap (fmap DB.pointFromRow . nubOn DB.checkpointSlotNo . mconcat) $ forM points $ \pt ->
                PG.fold conn listCheckpointsQry [pt :: Int64] [] $
                    \xs (Binary checkpointHeaderHash, (fromIntegral :: Int64 -> Word64) -> checkpointSlotNo) ->
                        pure (DB.Checkpoint{..} : xs)

    , listAncestorsDesc = \(SlotNo slotNo) n -> ReaderT $ \conn -> do
        handle (throwIO . DatabaseException "listAncestorsDesc") $
            fmap reverse $
                PG.fold conn listAncestorQry (fromIntegral slotNo :: Int64, n) [] $
                    \xs (Binary checkpointHeaderHash, (fromIntegral :: Int64 -> Word64) -> checkpointSlotNo) ->
                        pure ((DB.pointFromRow DB.Checkpoint{..}) : xs)

    , insertPatterns = \patterns -> ReaderT $ \conn ->
          handle (throwIO . DatabaseException "insertPatterns") $
              insertRows @"patterns" conn 1 $ Set.map (Only . DB.patternToRow) patterns

    , deletePattern = \pattern_-> ReaderT $ \conn -> do
          handle (throwIO . DatabaseException "deletePattern") $ do
              fromIntegral <$>
                  execute conn "DELETE FROM patterns WHERE pattern = ?"
                      (Only $ DB.patternToRow pattern_)

    , listPatterns = ReaderT $ \conn ->
        handle (throwIO . DatabaseException "listPatterns") $ do
            fmap fromList
                $ PG.fold_ conn "SELECT * FROM patterns" []
                $ \xs (Only x) -> pure (DB.patternFromRow x:xs)

    , insertBinaryData = \bin -> ReaderT $ \conn ->
        handle (throwIO . DatabaseException "insertBinaryData") $ do
            let
                rows = bin <&> \DB.BinaryData{..} ->
                    (Binary binaryDataHash, Binary binaryData)
            insertRows @"binary_data" conn 2 rows

    , getBinaryData = \(DB.datumHashToRow -> binaryDataHash) -> ReaderT $ \conn -> do
        handle (throwIO . DatabaseException "getBinaryData") $
            query conn getBinaryDataQry (Only $ Binary binaryDataHash) >>= \case
                [Only (Binary binaryData)] ->
                    pure $ Just (DB.binaryDataFromRow DB.BinaryData{..})
                [] ->
                    pure Nothing
                (length -> n) ->
                    throwIO $ ExpectedSingletonResult "getBinaryData" n

    , pruneBinaryData = ReaderT $ \conn ->
        handle (throwIO . DatabaseException "pruneBinaryData") $ do
            traceExecute_ tr conn pruneBinaryDataQry

    , insertScripts = \scripts -> ReaderT $ \conn ->
        handle (throwIO . DatabaseException "insertScripts") $ do
            let
                rows = scripts <&> \DB.ScriptReference{..} ->
                    (Binary scriptHash, Binary script)
            insertRows @"scripts" conn 2 rows

    , getScript = \(DB.scriptHashToRow -> scriptHash)-> ReaderT $ \conn ->
        handle (throwIO . DatabaseException "getScript") $
            query conn getScriptQry (Only $ Binary scriptHash) >>= \case
                [Only (Binary script)] ->
                    pure $ Just (DB.scriptFromRow DB.ScriptReference{..})
                [] ->
                    pure Nothing
                (length -> n) ->
                    throwIO $ ExpectedSingletonResult "getScript" n

    , rollbackTo = \(fromIntegral . unSlotNo -> minSlotNo) -> ReaderT $ \conn -> do
        handle (throwIO . DatabaseException "rollbackTo") $
            query_ conn selectMaxCheckpointQry >>= \case
                -- NOTE: Rolling back takes quite a bit of time and, when restarting
                -- the application, we'll always be asked to rollback to the
                -- _current tip_. In this case, there's nothing to delete or update,
                -- so we can safely skip it.
                [(currentSlotNo, _ :: Binary ByteString)] | currentSlotNo == minSlotNo -> do
                    pure ()
                _otherwise -> do
                    withTemporaryIndex tr conn "inputsByCreatedAt" "inputs" "created_at" $ do
                        withTemporaryIndex tr conn "inputsBySpentAt" "inputs" "spent_at" $ void $ do
                            deleteInputsIncrementally tr conn minSlotNo
                            _ <- traceExecute tr conn rollbackQryUpdateInputs [ minSlotNo ]
                            traceExecute tr conn rollbackQryDeleteCheckpoints [ minSlotNo ]
        handle (throwIO . DatabaseException (show selectMaxCheckpointQry)) $
            query_ conn selectMaxCheckpointQry >>= \case
                [((fromIntegral :: Int64 -> Word64) -> checkpointSlotNo, Binary checkpointHeaderHash)] ->
                    return $ Just (DB.pointFromRow DB.Checkpoint{..})
                [] ->
                    return Nothing
                res -> throwIO $ ExpectedSingletonResult (show selectMaxCheckpointQry) (length res)

    , optimize = return ()
    -- ^ TODO: Review if optimize needs to happen with Postgres.
    -- Also determine if this can be hidden within the `Database` implementation.
    -- Perhaps this could be done with an async task that runs at regular intervals?

    , runTransaction = \r -> bracketConnection $ \conn ->
          withTransaction conn (runReaderT r conn)
          -- ^ TODO: Consider implementing a retry behavior 

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
        (fromText -> whereClause, fromText . fromMaybe "" -> additionalJoin) =
            patternToSql mkByteaLiteral pattern_

markInputsQry :: Pattern -> Query
markInputsQry pattern_ =
    "UPDATE inputs SET spent_at = ? "
        <> additionalJoin
        <> " WHERE "
        <> whereClause
    where
        (fromText -> whereClause, fromText . fromMaybe "" -> additionalJoin) =
            patternToSql mkByteaLiteral pattern_

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
        patternToSql mkByteaLiteral pattern_

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
          patternToSql mkByteaLiteral pattern_

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
        patternToSql mkByteaLiteral pattern_

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
            patternToSql mkByteaLiteral pattern_

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

-- NOTE: This removes all binary_data that aren't associted with any
-- known input. The 'ORDER BY' at the end may seem pointless but is
-- actually CRUCIAL for the query performance as it forces SQLite to use
-- the availables indexes of both tables on 'data_hash' and
-- 'binary_data_hash'. Without that, this query may take 1h+ on a large
-- database (e.g. mainnet matching '*').
-- TODO: Investigate if the 'ORDER BY' clause is necessary in PostgreSQL
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
    "SELECT slot_no,header_hash FROM checkpoints ORDER BY slot_no DESC LIMIT 1"

deleteInputsIncrementally :: Tracer IO TraceConnection -> Connection -> Int64 -> IO ()
deleteInputsIncrementally tr conn minSlotNo = do
    deleted <- traceExecute tr conn rollbackQryDeleteInputs $ Only minSlotNo
    unless (deleted < pruneInputsMaxIncrement) $ deleteInputsIncrementally tr conn minSlotNo

rollbackQryDeleteInputs :: Query
rollbackQryDeleteInputs =
    "DELETE FROM inputs WHERE output_reference IN \
    \(SELECT output_reference FROM inputs WHERE created_at > ? LIMIT "
    <> show pruneInputsMaxIncrement <> ")"

rollbackQryUpdateInputs :: Query
rollbackQryUpdateInputs =
    "UPDATE inputs SET spent_at = NULL WHERE spent_at > ?"

rollbackQryDeleteCheckpoints :: Query
rollbackQryDeleteCheckpoints =
    "DELETE FROM checkpoints WHERE slot_no > ?"

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
            "inputs(address)" -- TODO: Find a substitute for SQLite's nocase clause.
        installIndex tr conn
            "inputsByDatumHash"
            "inputs(datum_hash)"
        installIndex tr conn
            "inputsByPaymentCredential"
            "inputs(payment_credential)" -- TODO: Find a substitute for SQLite's nocase clause.
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
            void $ execute conn "CREATE INDEX IF NOT EXISTS ? ON ?" (Identifier name, Identifier definition)
        True ->
            traceWith tr (DatabaseIndexAlreadyExists name)

withTemporaryIndex :: Tracer IO TraceConnection -> Connection -> Text -> Text -> Text -> IO a -> IO a
withTemporaryIndex tr conn name table column action = do
    exists <- indexDoesExist conn name
    unless exists $ do
        traceWith tr $ ConnectionCreateTemporaryIndex name
        void $ execute conn "CREATE INDEX IF NOT EXISTS ? ON ? ( ? )"
            (Identifier name, Identifier table, Identifier column)
        traceWith tr (ConnectionCreatedTemporaryIndex name)
    a <- action
    unless exists $ do
        traceWith tr $ ConnectionRemoveTemporaryIndex name
        dropIndexIfExists tr conn name True
    return a

-- | Check whether an index exists in the database. Handy to customize the behavior (e.g. logging)
-- depending on whether or not indexes are already there since 'CREATE INDEX IF NOT EXISTS' will not
-- tell whether or not it has indeed created something.
indexDoesExist :: Connection -> Text -> IO Bool
indexDoesExist conn indexName = do
    query conn qry (Only indexName) <&> \case
        [Only n] | n > (0 :: Int64) -> True
        _doesNotExist -> False
    where
        qry = "SELECT COUNT(*) FROM pg_indexes WHERE indexname = ?"

dropIndexIfExists :: Tracer IO TraceConnection -> Connection -> Text -> Bool -> IO ()
dropIndexIfExists tr conn indexName wasTemporary = do
    whenM (indexDoesExist conn indexName) $ do
        traceWith tr $ if wasTemporary
            then ConnectionRemoveTemporaryIndex{indexName}
            else ConnectionRemoveIndex{indexName}
        void . execute conn "DROP INDEX IF EXISTS ?" . Only $ Identifier indexName

--
-- Migrations
--

type MigrationRevision = Int

type Migration = [Query]

databaseVersion :: Connection -> IO MigrationRevision
databaseVersion conn = do
    _ <- execute_ conn createStatement
    handle (throwIO . DatabaseException "databaseVersion") $
        query_ conn countStatement >>= \case
            [(revision, _version :: String)] -> return revision
            [] -> return 0
            (length -> n) -> throwIO $ ExpectedSingletonResult "databaseVersion" n
    where
        createStatement =
            "CREATE TABLE IF NOT EXISTS migrations \
            \(\
                \id INTEGER PRIMARY KEY, \
                \version VARCHAR (50) \
            \);"

        countStatement =
            "SELECT id, version FROM migrations ORDER BY id DESC LIMIT 1;"

-- TODO: How will running migrations affect a DB that supports multiple Kupo instances?
runMigrations :: Tracer IO TraceDatabase -> Connection -> IO ()
runMigrations tr conn = do
    currentVersion <- databaseVersion conn
    let missingMigrations = drop currentVersion migrations
    traceWith tr (DatabaseCurrentVersion currentVersion)
    if null missingMigrations then
        traceWith tr DatabaseNoMigrationNeeded
    else do
        let targetVersion = currentVersion + length missingMigrations
        traceWith tr $ DatabaseRunningMigration currentVersion targetVersion
        executeMigrations missingMigrations
  where
    executeMigrations = \case
        [] -> do
            pure ()
        (instructions):rest -> do
            withTransaction conn $ mapM_ (execute_ conn) instructions
            executeMigrations rest

migrations :: [Migration]
migrations =
    zipWith
        ((\idx (sql, version) -> mkMigration idx (fromString version) $ decodeUtf8 sql))
        [1..]
        files
    where
      mkMigration :: Int -> Query -> Text -> Migration
      mkMigration idx version sql =
          ("INSERT INTO migrations (id, version) VALUES (" <> show idx <> ",'" <> version <> "');")
          : (map (fromString . T.unpack) . filter (not . T.null . T.strip) . T.splitOn ";") sql

      files =
          [ ($(embedFile "db/postgres/v1.0.0-beta/001.sql"), "v1.0.0.-beta/001.sql")
          , ($(embedFile "db/postgres/v1.0.0/001.sql"), "v1.0.0/001.sql")
          , ($(embedFile "db/postgres/v1.0.0/002.sql"), "v1.0.0/002.sql")
          , ($(embedFile "db/postgres/v1.0.1/001.sql"), "v1.0.1/001.sql")
          , ($(embedFile "db/postgres/v2.0.0-beta/001.sql"), "v2.0.0-beta/001.sql")
          , ($(embedFile "db/postgres/v2.1.0/001.sql"), "v2.1.0/001.sql")
          , ($(embedFile "db/postgres/v2.1.0/002.sql"), "v2.1.0/002.sql")
          , ($(embedFile "db/postgres/v2.1.0/003.sql"), "v2.1.0/003.sql")
          , ($(embedFile "db/postgres/v2.2.0/001.sql"), "v2.2.0/001.sql")
          ]

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
            "INSERT INTO "
            <> symbolVal (Proxy @tableName)
            <> " VALUES "
            <> mkValuePlaceholders len
            <> " ON CONFLICT DO NOTHING"

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

mkByteaLiteral :: ByteString -> Text
mkByteaLiteral bytes = "'\\x" <> encodeBase16 bytes <> "'"

withTotalChanges :: forall t a. (Foldable t) => (a -> IO Int64) -> t a -> IO Int
withTotalChanges io t =
    fromIntegral <$>
        foldM (\accum a -> (accum +) <$> io a) 0 t

--
-- Test helper
--

withTestDatabase :: String -> (Configuration.DatabaseLocation -> IO a) -> IO a
withTestDatabase dbName action = do
    bracket createDb dropDb (const $ action dbLocation)
    where
        connectInfo name = PG.defaultConnectInfo
            { PG.connectUser = "kupotest"
            , PG.connectPassword = "kupo"
            , PG.connectDatabase = name
            }

        createDb = void $ PG.withConnect (connectInfo "kupo") $ \conn ->
            execute conn "CREATE DATABASE ?" (Only . Identifier $ fromString dbName)

        dropDb = const . void $ PG.withConnect (connectInfo "kupo") $ \conn ->
            execute conn "DROP DATABASE ?" (Only . Identifier $ fromString dbName)

        dbLocation = Configuration.Remote $ fromJust $ URI.mkURI $
            "postgresql://kupotest:kupo@localhost/" <> T.pack dbName

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

data ExpectedSingletonResultException
    = ExpectedSingletonResult
        { context :: !Text
        , received :: !Int
        } deriving Show
instance Exception ExpectedSingletonResultException

data DatabaseException
    = DatabaseException
        { context :: !Text
        , causedBy :: !SqlError
        } deriving Show
instance Exception DatabaseException
