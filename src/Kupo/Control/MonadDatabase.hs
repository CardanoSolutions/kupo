--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kupo.Control.MonadDatabase
    ( -- * Database DSL
      MonadDatabase (..)
    , ConnectionType (..)
    , Database (..)
    , LongestRollback (..)
    , Connection

      -- * Database Entities
    , Input (..)
    , Checkpoint (..)

      -- * Tracer
    , TraceDatabase (..)
    ) where

import Kupo.Prelude

import Control.Exception
    ( mask, onException, throwIO )
import Control.Monad.Class.MonadSTM
    ( MonadSTM (..) )
import Control.Monad.Class.MonadThrow
    ( bracket_, catch )
import Control.Monad.Class.MonadTimer
    ( threadDelay )
import Control.Tracer
    ( Tracer, traceWith )
import Data.FileEmbed
    ( embedFile )
import Data.Severity
    ( HasSeverityAnnotation (..), Severity (..) )
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
    ( ToField (..) )
import GHC.TypeLits
    ( KnownSymbol, symbolVal )
import Numeric
    ( Floating (..) )

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Database.SQLite.Simple as Sqlite

class (Monad m, Monad (DBTransaction m)) => MonadDatabase (m :: Type -> Type) where
    type DBTransaction m :: (Type -> Type)
    data DBLock m :: Type

    newLock
        :: m (DBLock m)

    withDatabase
        :: Tracer m TraceDatabase
        -> ConnectionType
        -> DBLock m
        -> LongestRollback
        -> FilePath
        -> (Database m -> m a)
        -> m a

data ConnectionType = LongLived | ShortLived
    deriving (Eq, Show)

newtype LongestRollback = LongestRollback
    { getLongestRollback :: Word64
    } deriving newtype (Integral, Real, Num, Enum, Ord, Eq)

data Input = Input
    { outputReference :: ByteString
    , address :: Text
    , value :: ByteString
    , datumHash :: Maybe ByteString
    , createdAtSlotNo :: Word64
    , createdAtHeaderHash :: ByteString
    , spentAtSlotNo :: Maybe Word64
    , spentAtHeaderHash :: Maybe ByteString
    } deriving (Show)

data Checkpoint = Checkpoint
    { checkpointHeaderHash :: ByteString
    , checkpointSlotNo :: Word64
    } deriving (Show)

data Database (m :: Type -> Type) = Database
    { insertInputs
        :: [Input]
        -> DBTransaction m ()

    , deleteInputsByAddress
        :: Text  -- An address-like query
        -> DBTransaction m Int

    , deleteInputsByReference
        :: Set ByteString  -- An output reference
        -> DBTransaction m ()

    , markInputsByReference
        :: Word64 -- Slot
        -> Set ByteString  -- An output reference
        -> DBTransaction m ()

    , countSpentInputs
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

    , selectPatterns
        :: forall result. ()
        => (Text -> result)
        -> Text
        -> (result -> m ())
        -> DBTransaction m ()

    , deletePattern
        :: Text
        -> DBTransaction m Int

    , listPatterns
        :: forall result. ()
        => (Text -> result)
        -> DBTransaction m [result]

    , rollbackTo
        :: Word64  -- slot_no
        -> DBTransaction m (Maybe Word64)

    , runTransaction
        :: forall a. ()
        => DBTransaction m a
        -> m a

    , runImmediateTransaction
        :: forall a. ()
        => DBTransaction m a
        -> m a
    }

--
-- IO
--

instance MonadDatabase IO where
    type DBTransaction IO = ReaderT Connection IO
    data DBLock IO = DBLock (TVar IO Word) (TVar IO Bool) (TMVar IO Connection)

    newLock = DBLock <$> newTVarIO 0 <*> newTVarIO True <*> newEmptyTMVarIO

    withDatabase tr mode (DBLock readers writer inMemory) k filePath action
        | filePath == ":memory:" = do
            traceWith tr DatabaseRunningInMemory
            case mode of
                LongLived -> do
                    withConnection filePath $ \conn -> do
                        databaseVersion conn >>= runMigrations tr conn
                        atomically $ putTMVar inMemory conn
                        action (mkDatabase k (bracketConnection conn))
                ShortLived -> do
                    conn <- atomically $ readTMVar inMemory
                    action (mkDatabase k (bracketConnection conn))
        | otherwise = do
            withConnection filePath $ \conn -> do
                when (mode == LongLived) (databaseVersion conn >>= runMigrations tr conn)
                action (mkDatabase k (bracketConnection conn))
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
                        atomically (modifyTVar' readers succ)
                        atomically (readTVar writer >>= check . not)
                    )
                    (atomically (modifyTVar' readers pred))
                    (between conn)

                LongLived -> bracket_
                    (atomically $ do
                        readTVar readers >>= check . (== 0)
                        writeTVar writer True
                    )
                    (atomically $ writeTVar writer False)
                    (between conn)

mkDatabase :: LongestRollback -> (forall a. (Connection -> IO a) -> IO a) -> Database IO
mkDatabase (fromIntegral -> longestRollback) bracketConnection = Database
    { insertInputs = \inputs -> ReaderT $ \conn ->
        mapM_
        (\Input{..} ->
            insertRow @"inputs" conn
                [ SQLBlob outputReference
                , SQLText address
                , SQLBlob value
                , maybe SQLNull SQLBlob datumHash
                , SQLInteger (fromIntegral createdAtSlotNo)
                , maybe SQLNull (SQLInteger . fromIntegral) spentAtSlotNo
                ]
        )
        inputs

    , deleteInputsByAddress = \addressLike -> ReaderT $ \conn -> do
        execute_ conn (Query $ "DELETE FROM inputs WHERE address " <> T.replace "len" "LENGTH(address)" addressLike)
        changes conn

    , deleteInputsByReference = \refs -> ReaderT $ \conn -> do
        let n = length refs
        unless (n == 0) $ do
            let qry = "DELETE FROM inputs WHERE output_reference IN " <> mkPreparedStatement n
            execute conn qry refs

    , markInputsByReference = \(fromIntegral -> slotNo) refs -> ReaderT $ \conn -> do
        let n = length refs
        unless (n == 0) $ do
            let qry = "UPDATE inputs SET spent_at = ? WHERE output_reference IN " <> mkPreparedStatement n
            execute conn qry (SQLInteger slotNo : toRow refs)

    , foldInputs = \addressLike yield -> ReaderT $ \conn -> do
        let matchMaybeBytes = \case
                SQLBlob bytes -> Just bytes
                _ -> Nothing
        let matchMaybeWord64 = \case
                SQLInteger (fromIntegral -> wrd) -> Just wrd
                _ -> Nothing
        let qry = "SELECT output_reference, address, value, datum_hash, created_at, createdAt.header_hash, spent_at, spentAt.header_hash, LENGTH(address) as len \
                  \FROM inputs \
                  \JOIN checkpoints AS createdAt ON createdAt.slot_no = created_at \
                  \LEFT OUTER JOIN checkpoints AS spentAt ON spentAt.slot_no = spent_at \
                  \WHERE address " <> addressLike <> " ORDER BY created_at DESC"

        fold_ conn (Query qry) () $ \() -> \case
            [ SQLBlob outputReference
                , SQLText address
                , SQLBlob value
                , matchMaybeBytes -> datumHash
                , SQLInteger (fromIntegral -> createdAtSlotNo)
                , SQLBlob createdAtHeaderHash
                , matchMaybeWord64 -> spentAtSlotNo
                , matchMaybeBytes -> spentAtHeaderHash
                , _ -- LENGTH(address)
                ] -> yield Input{..}
            (xs :: [SQLData]) -> throwIO (UnexpectedRow addressLike [xs])

    , countSpentInputs = ReaderT $ \conn -> do
        let qry = "SELECT COUNT(rowid) FROM inputs WHERE spent_at IS NOT NULL"
        query_ conn qry >>= \case
            [[SQLInteger n]] ->
                return (fromIntegral n)
            xs ->
                throwIO $ UnexpectedRow (fromQuery qry) xs

    , insertCheckpoints = \cps -> ReaderT $ \conn ->
        mapM_
        (\Checkpoint{..} ->
            insertRow @"checkpoints" conn
                [ SQLBlob checkpointHeaderHash
                , SQLInteger (fromIntegral checkpointSlotNo)
                ]
        )
        cps

    , listCheckpointsDesc = \mk -> ReaderT $ \conn -> do
        let points =
                [ 0, 10 .. longestRollback `div` 2 ^ n ]
                ++
                [ longestRollback `div` (2 ^ e) | (e :: Integer) <- [ n-1, n-2 .. 0 ] ]
              where
                n = ceiling (log (fromIntegral @_ @Double longestRollback))
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

    , deletePattern = \p -> ReaderT $ \conn -> do
        execute conn "DELETE FROM patterns WHERE pattern = ?"
            [ SQLText p
            ]
        changes conn

    , listPatterns = \mk -> ReaderT $ \conn -> do
        fold_ conn "SELECT * FROM patterns" []
            $ \xs (Only x) -> pure (mk x:xs)

    , selectPatterns = \mk addressLike yield -> ReaderT $ \conn -> do
        let qry = "SELECT pattern, LENGTH(pattern) as len \
                  \FROM patterns \
                  \WHERE pattern " <> addressLike

        fold_ conn (Query qry) () $ \() -> \case 
            [ SQLText address ] -> yield (mk address)
            (xs :: [SQLData]) -> throwIO (UnexpectedRow addressLike [xs])

    , rollbackTo = \slotNo -> ReaderT $ \conn -> do
        execute conn "DELETE FROM inputs WHERE created_at > ?"
            [ SQLInteger (fromIntegral slotNo)
            ]
        execute conn "DELETE FROM checkpoints WHERE slot_no > ?"
            [ SQLInteger (fromIntegral slotNo)
            ]
        query_ conn "SELECT MAX(slot_no) FROM checkpoints" >>= \case
            [[SQLInteger slotNo']] ->
                return $ Just (fromIntegral slotNo')
            [[SQLNull]] ->
                return Nothing
            xs ->
                throwIO $ UnexpectedRow ("MAX(" <> show slotNo <> ")") xs

    , runTransaction = \r -> bracketConnection $ \conn ->
        retryWhenBusy $ withTransaction conn False (runReaderT r conn)

    , runImmediateTransaction = \r -> bracketConnection $ \conn ->
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

retryWhenBusy :: IO a -> IO a
retryWhenBusy action =
    action `catch` (\e@SQLError{sqlError} -> case sqlError of
        ErrorBusy -> do
            threadDelay 0.1
            retryWhenBusy action
        _ ->
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

--
-- Migrations
--

type MigrationRevision = Int

data Migration = SchemaMigration [Query] | SettingsMigration Query

databaseVersion :: Connection -> IO MigrationRevision
databaseVersion conn =
    withStatement conn "PRAGMA user_version" $ \stmt -> do
        nextRow stmt >>= \case
            Just (Only version) ->
                pure version
            _ ->
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
        (SchemaMigration instructions):rest -> do
            void $ withTransaction conn True $ traverse (execute_ conn) instructions
            executeMigrations rest
        (SettingsMigration instruction):rest -> do
            execute_ conn instruction
            executeMigrations rest

migrations :: [Migration]
migrations =
    [ mkMigration ix (decodeUtf8 migration)
    | (ix, (migration, mkMigration)) <- zip
        [1..]
        [ ( $(embedFile "db/v1.0.0-beta/001.sql"), mkSchemaMigration   )
        , ( $(embedFile "db/v1.0.0/001.sql"),      mkSchemaMigration   )
        , ( $(embedFile "db/v1.0.0/002.sql"),      mkSchemaMigration   )
        , ( $(embedFile "db/v1.0.1/001.sql"),      mkSchemaMigration   )
        , ( $(embedFile "db/v2.0.0/001.sql"),      mkSchemaMigration   )
        , ( $(embedFile "db/v2.0.0/002.sql"),      mkSettingsMigration )
        ]
    ]
  where
    mkSchemaMigration :: Int -> Text -> Migration
    mkSchemaMigration i sql = SchemaMigration $
        ("PRAGMA user_version = " <> show i <> ";")
        : (fmap Query . filter (not . T.null . T.strip) . T.splitOn ";") sql

    mkSettingsMigration :: Int -> Text -> Migration
    mkSettingsMigration i sql = SettingsMigration $
        ("PRAGMA user_version = " <> show i <> ";") <> " " <> Query sql

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
    = UnexpectedRow Text [[SQLData]]
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
        DatabaseCurrentVersion{}    -> Info
        DatabaseNoMigrationNeeded{} -> Debug
        DatabaseRunningMigration{}  -> Notice
        DatabaseRunningInMemory{}   -> Warning
