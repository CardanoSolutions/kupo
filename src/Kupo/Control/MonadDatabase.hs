--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Kupo.Control.MonadDatabase
    ( -- * Database DSL
      MonadDatabase (..)
    , Mode (..)
    , Database (..)
    , LongestRollback (..)

      -- * Tracer
    , TraceDatabase (..)
    ) where

import Kupo.Prelude

import Control.Exception
    ( throwIO )
import Control.Monad.Class.MonadSTM
    ( MonadSTM (..) )
import Control.Monad.Class.MonadThrow
    ( bracket_ )
import Control.Tracer
    ( Tracer, traceWith )
import Data.FileEmbed
    ( embedFile )
import Data.Severity
    ( HasSeverityAnnotation (..), Severity (..) )
import Database.SQLite.Simple
    ( Connection
    , Only (..)
    , Query (..)
    , SQLData (..)
    , ToRow (..)
    , execute
    , execute_
    , fold_
    , nextRow
    , withConnection
    , withStatement
    , withTransaction
    )
import GHC.TypeLits
    ( KnownSymbol, symbolVal )

import qualified Data.Text as T

class (Monad m, Monad (DBTransaction m)) => MonadDatabase (m :: Type -> Type) where
    type DBTransaction m :: (Type -> Type)
    data DBLock m :: Type

    newLock
        :: m (DBLock m)

    withDatabase
        :: Tracer m TraceDatabase
        -> Mode
        -> DBLock m
        -> LongestRollback
        -> FilePath
        -> (Database m -> m a)
        -> m a

data Mode = Write | ReadOnly
    deriving (Eq, Show)

newtype LongestRollback = LongestRollback
    { getLongestRollback :: Word64
    } deriving newtype (Integral, Real, Num, Enum, Ord, Eq)

data Database (m :: Type -> Type) = Database
    { insertInputs
        :: [ ( ByteString        -- output_reference
             , Text              -- address
             , ByteString        -- value
             , Maybe ByteString  -- datum_hash
             , Word64            -- slot_no
             )
           ]
        -> DBTransaction m ()

    , foldInputsByAddress
        :: Text  -- An address-like query
        -> (  ByteString         -- output_reference
           -> Text               -- address
           -> ByteString         -- value
           -> Maybe ByteString   -- datum_hash
           -> Word64             -- slot_no
           -> m ()
           )
        -> DBTransaction m ()

    , insertCheckpoint
        :: ByteString -- header_hash
        -> Word64     -- slot_no
        -> DBTransaction m ()

    , listCheckpointsDesc
        :: forall checkpoint. ()
        => (ByteString -> Word64 -> checkpoint)
        -> DBTransaction m [checkpoint]

    , rollbackTo
        :: Word64  -- slot_no
        -> DBTransaction m ()

    , runTransaction
        :: forall a. ()
        => DBTransaction m a
        -> m a
    }

--
-- IO
--

instance MonadDatabase IO where
    type DBTransaction IO = ReaderT Connection IO
    data DBLock IO = DBLock (TVar IO Word) (TVar IO Bool)

    newLock = DBLock <$> newTVarIO 0 <*> newTVarIO True

    withDatabase tr mode (DBLock readers writer) k filePath action = do
        withConnection filePath $ \conn -> do
            when (mode == Write) (databaseVersion conn >>= runMigrations tr conn)
            action (mkDatabase k (bracketConnection conn))
      where
        -- The heuristic below aims at favoring readers over writers. We assume
        -- that there is only one writer and possibly many readers. The count of
        -- readers is kept in a TVar and readers increment/decrement it around
        -- each transactions.
        -- The writer however will only attempt writing if there's currently no
        -- reader. In case where there's at least one reader waiting, it'll let
        -- them pass and wait until all readers are done.
        bracketConnection :: Connection -> (forall a. ((Connection -> IO a) -> IO a))
        bracketConnection conn between =
            case mode of
                ReadOnly -> bracket_
                    (do
                        atomically (modifyTVar' readers succ)
                        atomically (readTVar writer >>= check . not)
                    )
                    (atomically (modifyTVar' readers pred))
                    (between conn)

                Write -> bracket_
                    (do
                        n <- atomically $ do
                            n <- readTVar readers
                            when (n > 0) (writeTVar writer False)
                            return n
                        when (n > 0) $ atomically $ do
                            readTVar readers >>= check . (== 0)
                            writeTVar writer True
                    )
                    (pure ())
                    (between conn)

mkDatabase :: LongestRollback -> (forall a. (Connection -> IO a) -> IO a) -> Database IO
mkDatabase (toInteger -> longestRollback) bracketConnection = Database
    { insertInputs = \inputs -> ReaderT $ \conn ->
        mapM_
        (\(outputReference, address, value, datumHash, fromIntegral -> slotNo) ->
            insertRow @"inputs" conn
                [ SQLBlob outputReference
                , SQLText address
                , SQLBlob value
                , maybe SQLNull SQLBlob datumHash
                , SQLInteger slotNo
                ]
        )
        inputs

    , foldInputsByAddress = \addressLike yield -> ReaderT $ \conn -> do
        let matchMaybeDatumHash = \case
                SQLBlob datumHash -> Just datumHash
                _ -> Nothing
        let qry = "SELECT output_reference, address, value, datum_hash, slot_no, LENGTH(address) as len \
                  \FROM inputs WHERE address " <> addressLike <> " ORDER BY slot_no DESC"
        fold_ conn (Query qry) () $ \() -> \case
            [ SQLBlob outputReference
                , SQLText address
                , SQLBlob value
                , matchMaybeDatumHash -> datumHash
                , SQLInteger (fromIntegral -> slotNo)
                , _ -- LENGTH(address)
                ] -> yield outputReference address value datumHash slotNo
            (xs :: [SQLData]) -> throwIO (UnexpectedRow addressLike xs)

    , insertCheckpoint = \headerHash (toInteger -> slotNo) -> ReaderT $ \conn -> do
        insertRow @"checkpoints" conn
            [ SQLBlob headerHash
            , SQLInteger (fromIntegral slotNo)
            ]
        execute conn "DELETE FROM checkpoints WHERE slot_no < ?"
            [ SQLInteger (fromIntegral (slotNo - longestRollback))
            ]

    , listCheckpointsDesc = \mk -> ReaderT $ \conn -> do
        -- NOTE: fetching in *ASC*ending order because the list construction
        -- reverses it,
        fold_ conn "SELECT * FROM checkpoints ORDER BY slot_no ASC" []
            $ \xs (headerHash, slotNo) -> pure ((mk headerHash slotNo) : xs)

    , rollbackTo = \(fromIntegral -> slotNo) -> ReaderT $ \conn -> do
        execute conn "DELETE FROM inputs WHERE slot_no > ?"
            [ SQLInteger slotNo
            ]
        execute conn "DELETE FROM checkpoints WHERE slot_no > ?"
            [ SQLInteger slotNo
            ]

    , runTransaction = \r -> bracketConnection $ \conn ->
        withTransaction conn (runReaderT r conn)
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
            _ ->
                throwIO UnexpectedUserVersion

runMigrations :: Tracer IO TraceDatabase -> Connection -> MigrationRevision -> IO ()
runMigrations tr conn currentVersion = do
    let missingMigrations = drop currentVersion migrations
    traceWith tr (DatabaseCurrentVersion currentVersion)
    if null missingMigrations then
        traceWith tr DatabaseNoMigrationNeeded
    else do
        traceWith tr $ DatabaseRunningMigration currentVersion (length missingMigrations)
        void $ withTransaction conn $
            traverse (traverse (execute_ conn)) missingMigrations

migrations :: [Migration]
migrations =
    [ mkMigration (decodeUtf8 m)
    | m <-
        [ $(embedFile "db/001.sql")
        ]
    ]
  where
    mkMigration :: Text -> [Query]
    mkMigration =
        fmap Query . filter (not . T.null . T.strip) . T.splitOn ";"

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
    = UnexpectedRow Text [SQLData]
    deriving Show
instance Exception UnexpectedRowException

--
-- Tracer
--

data TraceDatabase where
    DatabaseFoundCheckpoints
        :: { checkpoints :: [Word64] }
        -> TraceDatabase
    DatabaseCurrentVersion
        :: { currentVersion :: Int }
        -> TraceDatabase
    DatabaseNoMigrationNeeded
        :: TraceDatabase
    DatabaseRunningMigration
        :: { from :: Int, to :: Int }
        -> TraceDatabase
    deriving stock (Generic, Show)

instance ToJSON TraceDatabase where
    toEncoding =
        defaultGenericToEncoding

instance HasSeverityAnnotation TraceDatabase where
    getSeverityAnnotation = \case
        DatabaseFoundCheckpoints{}  -> Info
        DatabaseCurrentVersion{}    -> Info
        DatabaseNoMigrationNeeded{} -> Debug
        DatabaseRunningMigration{}  -> Notice
