--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kupo.App.Database
    ( -- * Database DSL
      Database (..)
    , DBTransaction

      -- * Setup
    , withDatabase
    , ConnectionType (..)
    , Connection

      -- ** Lock
    , DBLock
    , newLock

      -- * Tracer
    , TraceDatabase (..)
    ) where

import Kupo.Prelude

import Control.Exception
    ( mask
    , onException
    , throwIO
    )
import Control.Monad.Class.MonadSTM
    ( MonadSTM (..)
    )
import Control.Monad.Class.MonadThrow
    ( bracket_
    , catch
    )
import Control.Monad.Class.MonadTimer
    ( threadDelay
    )
import Control.Tracer
    ( Tracer
    , traceWith
    )
import Data.FileEmbed
    ( embedFile
    )
import Data.Severity
    ( HasSeverityAnnotation (..)
    , Severity (..)
    )
import Database.SQLite.Simple
    ( Connection
    , Error (..)
    , Only (..)
    , Query (..)
    , SQLData (..)
    , SQLError (..)
    , ToRow (..)
    , changes
    , execute
    , execute_
    , fold_
    , nextRow
    , query_
    , withConnection
    , withStatement
    )
import Database.SQLite.Simple.ToField
    ( ToField (..)
    )
import GHC.TypeLits
    ( KnownSymbol
    , symbolVal
    )
import Kupo.Data.Configuration
    ( LongestRollback (..)
    )
import Kupo.Data.Database
    ( BinaryData (..)
    , Checkpoint (..)
    , Input (..)
    , ScriptReference (..)
    )
import Numeric
    ( Floating (..)
    )

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Database.SQLite.Simple as Sqlite

data Database (m :: Type -> Type) = Database
    { insertInputs
        :: [Input]
        -> DBTransaction m ()

    , deleteInputsByAddress
        :: Text  -- An address-like query
        -> DBTransaction m Int

    , deleteInputsByReference
        :: Set ByteString  -- Output references
        -> DBTransaction m Int

    , markInputsByReference
        :: Word64 -- Slot
        -> Set ByteString  -- Output references
        -> DBTransaction m Int

    , pruneInputs
        :: DBTransaction m Int

    , foldInputs
        :: Text  -- An address-like query
        -> (Input -> m ())
        -> DBTransaction m ()

    , insertCheckpoints
        :: [Checkpoint]
        -> DBTransaction m ()

    , listCheckpointsDesc
        :: forall checkpoint. ()
        => (Checkpoint -> checkpoint)
        -> DBTransaction m [checkpoint]

    , listAncestorsDesc
        :: forall checkpoint. ()
        => Word64 -- Slot
        -> Int64 -- Number of ancestors to retrieve
        -> (Checkpoint -> checkpoint)
        -> DBTransaction m [checkpoint]

    , insertPatterns
        :: [Text]
        -> DBTransaction m ()

    , deletePattern
        :: Text
        -> DBTransaction m Int

    , listPatterns
        :: forall result. ()
        => (Text -> result)
        -> DBTransaction m [result]

    , insertBinaryData
        :: [BinaryData]
        -> DBTransaction m ()

    , getBinaryData
        :: forall binaryData. ()
        => ByteString -- binary_data_hash
        -> (BinaryData -> binaryData)
        -> DBTransaction m (Maybe binaryData)

    , pruneBinaryData
        :: DBTransaction m Int

    , insertScripts
        :: [ScriptReference]
        -> DBTransaction m ()

    , getScript
        :: forall script. ()
        => ByteString -- script_hash
        -> (ScriptReference -> script)
        -> DBTransaction m (Maybe script)

    , rollbackTo
        :: Word64  -- slot_no
        -> DBTransaction m (Maybe Word64)

    -- Run a read-only transaction, that is, a set of SELECT operations that
    -- leave the database unchanged. It acquires a different type of lock than
    -- the 'runReadwriteTransaction' which also leads to different behavior when
    -- it comes to retrying.
    -- Multiple read-only transactions can happen at the same time, whereas a
    -- read-write transaction will need to wait.
    , runReadOnlyTransaction
        :: forall a. ()
        => DBTransaction m a
        -> m a

    -- See note on 'runReadOnlyTransaction' above.
    , runReadWriteTransaction
        :: forall a. ()
        => DBTransaction m a
        -> m a

    , longestRollback
        :: LongestRollback
    }

type family DBTransaction (m :: Type -> Type) :: (Type -> Type) where
    DBTransaction IO = ReaderT Connection IO

withDatabase
    :: Tracer IO TraceDatabase
    -> ConnectionType
    -> DBLock IO
    -> LongestRollback
    -> FilePath
    -> (Database IO -> IO a)
    -> IO a
withDatabase tr mode (DBLock readers writer) k filePath action = do
    withConnection filePath $ \conn -> do
        when (mode == LongLived) $ do
            databaseVersion conn >>= runMigrations tr conn
            execute_ conn "PRAGMA synchronous = NORMAL"
            execute_ conn "PRAGMA journal_mode = WAL"
        action (mkDatabase tr k (bracketConnection conn))
  where
    -- The heuristic below aims at favoring light reader/writer over the main
    -- writer. We assume that there is only one persistent db producer and possibly
    -- many short-lived reader/writer. The count of short-lived connection
    -- is kept in a TVar and each connection increment/decrement it around
    -- each transaction.
    --
    -- Short-lived connection are _mostly_read-only, but they may sometimes
    -- perform quick writes. Hence, there's a simple fail-over mechanism for
    -- them, in the (unlikely) case where they'd perform two concurrent
    -- conflictual requests. There's no need for the LongLived connection
    -- which is always alone.
    --
    -- The persistent process will only attempt writing if there's currently
    -- no short-lived one. In case where there's at least one busy, it'll let
    -- them pass and wait until all short-lived are done.
    bracketConnection :: Connection -> (forall a. ((Connection -> IO a) -> IO a))
    bracketConnection conn between =
        case mode of
            ShortLived -> bracket_
                (do
                    atomically (modifyTVar' readers next)
                    atomically (readTVar writer >>= check . not)
                )
                (atomically (modifyTVar' readers prev))
                (between conn)

            LongLived -> bracket_
                (atomically $ do
                    readTVar readers >>= check . (== 0)
                    writeTVar writer True
                )
                (atomically $ writeTVar writer False)
                (between conn)

-- ** ConnectionType

data ConnectionType = LongLived | ShortLived
    deriving (Eq, Show)

-- ** Lock

data DBLock (m :: Type -> Type) = DBLock !(TVar m Word) !(TVar m Bool)

newLock :: MonadSTM m => m (DBLock m)
newLock = DBLock
    <$> newTVarIO 0 <*> newTVarIO True

--
-- IO
--

mkDatabase
    :: Tracer IO TraceDatabase
    -> LongestRollback
    -> (forall a. (Connection -> IO a) -> IO a)
    -> Database IO
mkDatabase tr longestRollback bracketConnection = Database
    { longestRollback
    , insertInputs = \inputs -> ReaderT $ \conn -> do
        mapM_
            (\Input{..} -> do
                insertRow @"inputs" conn
                    [ SQLBlob outputReference
                    , SQLText address
                    , SQLBlob value
                    , maybe SQLNull SQLBlob datumHash
                    , maybe SQLNull SQLBlob refScriptHash
                    , SQLInteger (fromIntegral createdAtSlotNo)
                    , maybe SQLNull (SQLInteger . fromIntegral) spentAtSlotNo
                    ]
                case datum of
                    Nothing ->
                        pure ()
                    Just BinaryData{..} ->
                        insertRow @"binary_data" conn
                            [ SQLBlob binaryDataHash
                            , SQLBlob binaryData
                            ]
                case refScript of
                    Nothing ->
                        pure ()
                    Just ScriptReference{..} ->
                        insertRow @"scripts" conn
                            [ SQLBlob scriptHash
                            , SQLBlob script
                            ]
            )
            inputs

    , deleteInputsByAddress = \addressLike -> ReaderT $ \conn -> do
        let qry = Query $ "DELETE FROM inputs WHERE " <> addressLike
        execute_ conn qry
        changes conn

    , pruneInputs = ReaderT $ \conn -> do
        traceWith tr (DatabaseBeginQuery "pruneInputs")
        -- NOTE: There's a (not-so) pointless query on the 'address' column
        -- to force SQLite to use the compound index 'inputsByAddress' which
        -- drastically speed up the query. Indeed, SQLite will only use an index
        -- if the left-most column(s) of an index are used in a WHERE or ORDER
        -- BY clause.
        -- Incidentally, the clause has no effect on the filtering because all
        -- stored addresses start with  '0'.
        let qry = "DELETE FROM inputs \
                  \WHERE address LIKE '0%' AND spent_at < ( \
                  \  (SELECT MAX(slot_no) FROM checkpoints) - ?\
                  \)"
        execute conn qry [ SQLInteger (fromIntegral longestRollback) ]
        traceWith tr (DatabaseExitQuery "pruneInputs")
        changes conn

    , deleteInputsByReference = \refs -> ReaderT $ \conn -> do
        let n = length refs
        if n > 0 then do
            let qry = "DELETE FROM inputs \
                      \WHERE output_reference IN " <> mkPreparedStatement n
            execute conn qry refs
            changes conn
        else do
            return 0

    , markInputsByReference = \(fromIntegral -> slotNo) refs -> ReaderT $ \conn -> do
        let n = length refs
        if n > 0 then do
            let qry = "UPDATE inputs SET spent_at = ? \
                      \WHERE output_reference IN " <> mkPreparedStatement n
            execute conn qry (SQLInteger slotNo : toRow refs)
            changes conn
        else do
            return 0

    , foldInputs = \whereClause yield -> ReaderT $ \conn -> do
        let qry = "SELECT output_reference, address, value, datum_hash, script_hash,\
                    \ created_at, createdAt.header_hash,\
                    \ spent_at, spentAt.header_hash \
                  \FROM inputs \
                  \JOIN checkpoints AS createdAt ON createdAt.slot_no = created_at \
                  \LEFT OUTER JOIN checkpoints AS spentAt ON spentAt.slot_no = spent_at \
                  \WHERE " <> whereClause <> " \
                  \ORDER BY created_at DESC"

        -- TODO: Allow resolving datums / scripts on demand through LEFT JOIN
        --
        -- See [#21](https://github.com/CardanoSolutions/kupo/issues/21)
        let (datum, refScript) = (Nothing, Nothing)

        Sqlite.fold_ conn (Query qry) () $ \() -> \case
            [  SQLBlob outputReference
             , SQLText address
             , SQLBlob value
             , matchMaybeBytes -> datumHash
             , matchMaybeBytes -> refScriptHash
             , SQLInteger (fromIntegral -> createdAtSlotNo)
             , SQLBlob createdAtHeaderHash
             , matchMaybeWord64 -> spentAtSlotNo
             , matchMaybeBytes -> spentAtHeaderHash
             ] ->
                yield Input{..}
            (xs :: [SQLData]) ->
                throwIO (UnexpectedRow whereClause [xs])

    , insertCheckpoints = \cps -> ReaderT $ \conn -> do
        mapM_
            (\Checkpoint{..} ->
                insertRow @"checkpoints" conn
                    [ SQLBlob checkpointHeaderHash
                    , SQLInteger (fromIntegral checkpointSlotNo)
                    ]
            )
            cps

    , listCheckpointsDesc = \mk -> ReaderT $ \conn -> do
        let k = fromIntegral longestRollback
        let points =
                [ 0, 10 .. k `div` 2 ^ n ]
                ++
                [ k `div` (2 ^ e) | (e :: Integer) <- [ n-1, n-2 .. 0 ] ]
              where
                n = ceiling (log (fromIntegral @_ @Double k))
        let qry = "SELECT * FROM checkpoints \
                  \WHERE slot_no >= ((SELECT MAX(slot_no) FROM checkpoints) - ?) \
                  \ORDER BY slot_no ASC \
                  \LIMIT 1"
        fmap (fmap mk . nubOn checkpointSlotNo . mconcat) $ forM points $ \pt ->
            Sqlite.fold conn qry [SQLInteger pt] [] $
                \xs (checkpointHeaderHash, checkpointSlotNo) ->
                    pure (Checkpoint{..} : xs)

    , listAncestorsDesc = \slotNo n mk -> ReaderT $ \conn -> do
        let qry = "SELECT * FROM checkpoints \
                  \WHERE slot_no < ? \
                  \ORDER BY slot_no DESC \
                  \LIMIT ?"
        fmap reverse $ Sqlite.fold conn qry (SQLInteger <$> [fromIntegral slotNo, n]) [] $
            \xs (checkpointHeaderHash, checkpointSlotNo) ->
                pure ((mk Checkpoint{..}) : xs)

    , insertPatterns = \patterns -> ReaderT $ \conn -> do
        mapM_
            (\p ->
                insertRow @"patterns" conn
                    [ SQLText p
                    ]
            )
            patterns

    , insertBinaryData = \bin -> ReaderT $ \conn -> do
        mapM_
            (\BinaryData{..} ->
                insertRow @"binary_data" conn
                    [ SQLBlob binaryDataHash
                    , SQLBlob binaryData
                    ]
            )
            bin

    , getBinaryData = \binaryDataHash mk -> ReaderT $ \conn -> do
        let qry = "SELECT binary_data FROM binary_data \
                  \WHERE binary_data_hash = ? \
                  \LIMIT 1"
        Sqlite.query conn qry (Only (SQLBlob binaryDataHash)) <&> \case
            [[SQLBlob binaryData]] ->
                Just (mk BinaryData{..})
            _notSQLBlob ->
                Nothing

    , pruneBinaryData = ReaderT $ \conn -> do
        traceWith tr (DatabaseBeginQuery "pruneBinaryData")
        -- NOTE: This removes all binary_data that aren't associted with any
        -- known input. The 'ORDER BY' at the end may seem pointless but is
        -- actually CRUCIAL for the query performance as it forces SQLite to use
        -- the availables indexes of both tables on 'data_hash' and
        -- 'binary_data_hash'. Without that, this query may take 1h+ on a large
        -- database (e.g. mainnet matching '*').
        let qry = " DELETE FROM binary_data \
                  \ WHERE binary_data_hash IN ( \
                  \   SELECT binary_data_hash FROM binary_data \
                  \   LEFT JOIN inputs \
                  \   ON binary_data_hash = inputs.datum_hash \
                  \   WHERE inputs.output_reference IS NULL \
                  \   ORDER BY inputs.datum_hash \
                  \ );"
        execute_ conn qry
        traceWith tr (DatabaseExitQuery "pruneBinaryData")
        changes conn

    , insertScripts = \scripts -> ReaderT $ \conn -> do
        mapM_
            (\ScriptReference{..} ->
                insertRow @"scripts" conn
                    [ SQLBlob scriptHash
                    , SQLBlob script
                    ]
            )
            scripts

    , getScript = \scriptHash mk -> ReaderT $ \conn -> do
        let qry = "SELECT script FROM scripts \
                  \WHERE script_hash = ? \
                  \LIMIT 1"
        Sqlite.query conn qry (Only (SQLBlob scriptHash)) <&> \case
            [[SQLBlob script]] ->
                Just (mk ScriptReference{..})
            _notSQLBlob ->
                Nothing

    , deletePattern = \p -> ReaderT $ \conn -> do
        execute conn "DELETE FROM patterns WHERE pattern = ?"
            [ SQLText p
            ]
        changes conn

    , listPatterns = \mk -> ReaderT $ \conn -> do
        fold_ conn "SELECT * FROM patterns" []
            $ \xs (Only x) -> pure (mk x:xs)

    , rollbackTo = \slotNo -> ReaderT $ \conn -> do
        let slotNoVar = SQLInteger (fromIntegral slotNo)
        query_ conn "SELECT MAX(slot_no) FROM checkpoints" >>= \case
            -- NOTE: Rolling back takes quite a bit of time and, when restarting
            -- the application, we'll always be asked to rollback to the
            -- _current tip_. In this case, there's nothing to delete or update,
            -- so we can safely skip it.
            [[SQLInteger slotNo']] | fromIntegral slotNo' == slotNo -> do
                pure ()
            _otherwise -> do
                traceWith tr $ DatabaseBeginQuery "rollbackTo"
                execute conn "DELETE FROM inputs WHERE created_at > ?"
                    [ slotNoVar
                    ]
                execute conn "UPDATE inputs SET spent_at = NULL WHERE address LIKE '0%' AND spent_at > ?"
                    [ slotNoVar
                    ]
                execute conn "DELETE FROM checkpoints WHERE slot_no > ?"
                    [ slotNoVar
                    ]
                traceWith tr $ DatabaseExitQuery "rollbackTo"
        -- NOTE: It is good to run the 'PRAGMA optimize' every now-and-then. The
        -- SQLite's official documentation recommend to do so either upon
        -- closing every connection, or, every few hours. We do it after rolling
        -- back since this happens regularly but not too often, and, because
        -- after a rollback, a lot of things gets re-organized in the database
        -- so it's a good opportunity to tidy things up.
        execute_ conn "PRAGMA optimize"
        query_ conn "SELECT MAX(slot_no) FROM checkpoints" >>= \case
            [[SQLInteger slotNo']] ->
                return $ Just (fromIntegral slotNo')
            [[SQLNull]] ->
                return Nothing
            xs ->
                throwIO $ UnexpectedRow ("MAX(" <> show slotNo <> ")") xs

    , runReadOnlyTransaction = \r -> bracketConnection $ \conn ->
        retryWhenBusy $ withTransaction conn False (runReaderT r conn)

    , runReadWriteTransaction = \r -> bracketConnection $ \conn ->
        retryWhenBusy $ withTransaction conn True (runReaderT r conn)
    }

insertRow
    :: forall tableName.
        ( KnownSymbol tableName
        )
    => Connection
    -> [SQLData]
    -> IO ()
insertRow conn r =
    let
        tableName = fromString (symbolVal (Proxy @tableName))
        values = mkPreparedStatement (length (toRow r))
        qry = "INSERT OR IGNORE INTO " <> tableName <> " VALUES " <> values
     in
        execute conn qry r

--
-- Helpers
--

mkPreparedStatement :: Int -> Query
mkPreparedStatement n =
    Query ("(" <> T.intercalate "," (replicate n "?") <> ")")
{-# INLINABLE mkPreparedStatement #-}

retryWhenBusy :: IO a -> IO a
retryWhenBusy action =
    action `catch` (\e@SQLError{sqlError} -> case sqlError of
        ErrorLocked -> do
            threadDelay 0.1
            retryWhenBusy action
        ErrorBusy -> do
            threadDelay 0.1
            retryWhenBusy action
        _otherError ->
            throwIO e
    )

-- NOTE: Not using sqlite-simple's version because it lacks the crucial
-- 'onException' on commits; The commit operation may throw an 'SQLiteBusy'
-- exception when concurrent transactions are begin executed.
-- Yet, because it doesn't rollback in this case, it leaves the transaction in
-- an odd shrodinger state and makes it hard for the caller to properly handle
-- the exception (was the transaction rolled back or not? Is it safe to retry
-- it?). So, this slightly modified version makes sure to also rollback on a
-- failed commit; allowing caller to simply retry the whole transaction on
-- failure.
withTransaction :: Connection -> Bool -> IO a -> IO a
withTransaction conn immediate action =
  mask $ \restore -> do
    begin
    r <- restore action `onException` rollback
    commit `onException` rollback
    return r
  where
    begin
        | immediate = execute_ conn "BEGIN IMMEDIATE TRANSACTION"
        | otherwise = execute_ conn "BEGIN TRANSACTION"
    commit   = execute_ conn "COMMIT TRANSACTION"
    rollback = execute_ conn "ROLLBACK TRANSACTION"

matchMaybeBytes :: SQLData -> Maybe ByteString
matchMaybeBytes = \case
    SQLBlob bytes -> Just bytes
    _notSQLBlob -> Nothing
{-# INLINABLE matchMaybeBytes #-}

matchMaybeWord64 :: SQLData -> Maybe Word64
matchMaybeWord64 = \case
    SQLInteger (fromIntegral -> wrd) -> Just wrd
    _notSQLInteger -> Nothing
{-# INLINABLE matchMaybeWord64 #-}

--
-- Migrations
--

type MigrationRevision = Int

type Migration = [Query]

databaseVersion :: Connection -> IO MigrationRevision
databaseVersion conn =
    withStatement conn "PRAGMA user_version" $ \stmt -> do
        nextRow stmt >>= \case
            Just (Only version) ->
                pure version
            _unexpectedVersion ->
                throwIO UnexpectedUserVersion

runMigrations :: Tracer IO TraceDatabase -> Connection -> MigrationRevision -> IO ()
runMigrations tr conn currentVersion = do
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
            void $ withTransaction conn True $ traverse (execute_ conn) instructions
            executeMigrations rest

migrations :: [Migration]
migrations =
    [ mkMigration ix (decodeUtf8 migration)
    | (ix, migration) <- zip
        [1..]
        [ $(embedFile "db/v1.0.0-beta/001.sql")
        , $(embedFile "db/v1.0.0/001.sql")
        , $(embedFile "db/v1.0.0/002.sql")
        , $(embedFile "db/v1.0.1/001.sql")
        , $(embedFile "db/v2.0.0-beta/001.sql")
        ]
    ]
  where
    mkMigration :: Int -> Text -> Migration
    mkMigration i sql =
        ("PRAGMA user_version = " <> show i <> ";")
        : (fmap Query . filter (not . T.null . T.strip) . T.splitOn ";") sql

--
-- Exceptions
--

-- | Somehow, a 'PRAGMA user_version' didn't yield a number but, either nothing
-- or something else?
data UnexpectedUserVersionException
    = UnexpectedUserVersion
    deriving Show
instance Exception UnexpectedUserVersionException

-- | Something went wrong when unmarshalling data from the database.
data UnexpectedRowException
    = UnexpectedRow !Text ![[SQLData]]
    deriving Show
instance Exception UnexpectedRowException

--
-- Orphan
--

instance (ToField a) => ToRow (Set a) where
    toRow = Set.foldr (\x xs -> toField x : xs) []

--
-- Tracer
--

data TraceDatabase where
    DatabaseCurrentVersion
        :: { currentVersion :: Int }
        -> TraceDatabase
    DatabaseBeginQuery
        :: { beginQuery :: Text }
        -> TraceDatabase
    DatabaseExitQuery
        :: { exitQuery :: Text }
        -> TraceDatabase
    DatabaseNoMigrationNeeded
        :: TraceDatabase
    DatabaseRunningMigration
        :: { from :: Int, to :: Int }
        -> TraceDatabase
    DatabaseRunningInMemory
        :: TraceDatabase
    deriving stock (Generic, Show)

instance ToJSON TraceDatabase where
    toEncoding =
        defaultGenericToEncoding

instance HasSeverityAnnotation TraceDatabase where
    getSeverityAnnotation = \case
        DatabaseBeginQuery{}        -> Debug
        DatabaseExitQuery{}         -> Debug
        DatabaseCurrentVersion{}    -> Info
        DatabaseNoMigrationNeeded{} -> Debug
        DatabaseRunningMigration{}  -> Notice
        DatabaseRunningInMemory{}   -> Warning
