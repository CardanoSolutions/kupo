--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Kupo.App.Database
    ( -- * Database DSL
      Database (..)
    , DBTransaction

      -- ** Queries
      -- *** Inputs
    , deleteInputsQry
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
    , newDatabaseFile
    , createShortLivedConnection
    , withLongLivedConnection
    , copyDatabase
    , Connection
    , ConnectionType (..)
    , DatabaseFile (..)

      -- * Internal
    , installIndexes
    , installIndex

      -- ** Lock
    , DBLock
    , newLock

      -- * Tracer
    , TraceDatabase (..)
    ) where

import Kupo.Prelude

import Control.Exception
    ( IOException
    , handle
    , mask
    , onException
    , throwIO
    )
import Control.Tracer
    ( Tracer
    , traceWith
    )
import Data.FileEmbed
    ( embedFile
    )
import Data.Scientific
    ( scientific
    )
import Data.Severity
    ( HasSeverityAnnotation (..)
    , Severity (..)
    )
import Data.Text.Lazy.Builder.Scientific
    ( FPFormat (Fixed)
    , formatScientificBuilder
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
    , totalChanges
    , withConnection
    , withStatement
    )
import GHC.TypeLits
    ( KnownSymbol
    , symbolVal
    )
import Kupo.Control.MonadAsync
    ( concurrently_
    )
import Kupo.Control.MonadCatch
    ( catch
    )
import Kupo.Control.MonadDelay
    ( threadDelay
    )
import Kupo.Control.MonadLog
    ( TraceProgress (..)
    , nullTracer
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Control.MonadThrow
    ( bracket
    , bracket_
    )
import Kupo.Control.MonadTime
    ( DiffTime
    )
import Kupo.Data.Cardano
    ( BinaryData
    , DatumHash
    , Point
    , Script
    , ScriptHash
    , SlotNo (..)
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
    , Result
    , patternToText
    )
import Numeric
    ( Floating (..)
    )
import System.Directory
    ( Permissions (..)
    , copyFile
    , createDirectoryIfMissing
    , doesFileExist
    , getCurrentDirectory
    , getPermissions
    , removePathForcibly
    )
import System.FilePath
    ( (</>)
    )
import System.IO.Error
    ( isAlreadyExistsError
    )

import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder as TL
import qualified Database.SQLite.Simple as Sqlite
import qualified Kupo.Data.Configuration as Configuration
import qualified Kupo.Data.Database as DB

data Database (m :: Type -> Type) = Database
    { insertInputs
        :: [DB.Input]
        -> DBTransaction m ()

    , deleteInputs
        :: Set Pattern
        -> DBTransaction m Int

    , markInputs
        :: SlotNo
        -> Set Pattern
        -> DBTransaction m Int

    , pruneInputs
        :: DBTransaction m Int

    , foldInputs
        :: Pattern
        -> Range SlotNo
        -> StatusFlag
        -> SortDirection
        -> (Result -> m ())
        -> DBTransaction m ()

    , countInputs
        :: Pattern
        -> DBTransaction m Integer

    , foldPolicies
        :: Pattern
        -> (DB.Policy -> m ())
        -> DBTransaction m ()

    , countPolicies
        :: Pattern
        -> DBTransaction m Integer

    , insertPolicies
        :: Set DB.Policy
        -> DBTransaction m ()

    , insertCheckpoints
        :: [Point]
        -> DBTransaction m ()

    , listCheckpointsDesc
        :: DBTransaction m [Point]

    , listAncestorsDesc
        :: SlotNo
        -> Int64 -- Number of ancestors to retrieve
        -> DBTransaction m [Point]

    , insertPatterns
        :: Set Pattern
        -> DBTransaction m ()

    , deletePattern
        :: Pattern
        -> DBTransaction m Int

    , listPatterns
        :: DBTransaction m (Set Pattern)

    , insertBinaryData
        :: [DB.BinaryData]
        -> DBTransaction m ()

    , getBinaryData
        :: DatumHash
        -> DBTransaction m (Maybe BinaryData)

    , pruneBinaryData
        :: DBTransaction m Int

    , insertScripts
        :: [DB.ScriptReference]
        -> DBTransaction m ()

    , getScript
        :: ScriptHash
        -> DBTransaction m (Maybe Script)

    , rollbackTo
        :: SlotNo
        -> DBTransaction m (Maybe Point)

    , optimize
        :: DBTransaction  m ()

    , runTransaction
        :: forall a. ()
        => DBTransaction m a
        -> m a

    , longestRollback
        :: LongestRollback

    , close
        :: m ()
    }

type family DBTransaction (m :: Type -> Type) :: (Type -> Type) where
    DBTransaction IO = ReaderT Connection IO

data ConnectionType = ReadOnly | ReadWrite | WriteOnly
    deriving (Generic, Eq, Show)

instance ToJSON ConnectionType where
    toEncoding = defaultGenericToEncoding

data DatabaseFile = OnDisk !FilePath | InMemory !(Maybe FilePath)
    deriving (Generic, Eq, Show)

data NewDatabaseFileException
    = FailedToAccessOrCreateDatabaseFile { reason :: FailedToCreateDatabaseFileReason }
    deriving (Show)

instance Exception NewDatabaseFileException

data FailedToCreateDatabaseFileReason
    = SpecifiedPathIsAFile { path :: !FilePath }
    | SpecifiedPathIsReadOnly { path :: !FilePath }
    | SomeUnexpectedErrorOccured { error :: !IOException }
    deriving (Show)


-- | Create a new 'DatabaseFile' in the expected workding directory. Create the target
-- directory (recursively) if it doesn't exist.
newDatabaseFile
    :: (MonadIO m)
    => Tracer IO TraceDatabase
    -> Configuration.WorkDir
    -> m DatabaseFile
newDatabaseFile tr = \case
    Configuration.InMemory -> do
        return $ InMemory Nothing
    Configuration.Dir dir ->
        OnDisk <$> newDatabaseOnDiskFile tr (traceWith tr . DatabaseCreateNew) dir

newDatabaseOnDiskFile
    :: (MonadIO m)
    => Tracer IO TraceDatabase
    -> (FilePath -> IO ())
    -> FilePath
    -> m FilePath
newDatabaseOnDiskFile tr onFileMissing dir = liftIO $ do
    absoluteDir <- (</> dir) <$> getCurrentDirectory
    handle (onAlreadyExistsError absoluteDir) $ createDirectoryIfMissing True dir
    permissions <- getPermissions absoluteDir
    unless (writable permissions) $ bail (SpecifiedPathIsReadOnly absoluteDir)
    let dbFile = absoluteDir </> "kupo.sqlite3"
    unlessM (doesFileExist dbFile) $ onFileMissing dbFile
    return dbFile
  where
    bail absoluteDir = do
        traceWith tr $ DatabasePathMustBeDirectory
            { hint = "The path you've specified as working directory is a file; you probably meant to \
                     \point to the parent directory instead. Don't worry about the database file, \
                     \I'll manage it myself."
            }
        throwIO (FailedToAccessOrCreateDatabaseFile absoluteDir)
    onAlreadyExistsError absoluteDir e
      | isAlreadyExistsError e = do
          bail (SpecifiedPathIsAFile absoluteDir)
      | otherwise =
          bail (SomeUnexpectedErrorOccured e)

-- | Construct a connection string for the SQLite database. This utilizes (and assumes) the URI
-- recognition from SQLite to choose between read-only or read-write database. By default also, when
-- no filepath is provided, the database is created in-memory with a shared cache.
--
-- For testing purpose however, it is also possible to create a in-memory database in isolation by
-- simply passing `:memory:` as a filepath.
mkConnectionString
    :: DatabaseFile
    -> ConnectionType
    -> String
mkConnectionString filePath mode =
    case (filePath, mode) of
        (OnDisk fp, ReadOnly)  ->
           "file:" <> fp <> "?mode=ro"
        (OnDisk fp, ReadWrite) ->
           "file:" <> fp <> "?mode=rwc"
        (OnDisk fp, WriteOnly) ->
           "file:" <> fp <> "?mode=rwc"
        (InMemory Nothing, _ ) ->
           "file::kupo:?mode=memory&cache=shared"
        (InMemory (Just fp), _) ->
            fp

-- | A short-lived connection meant to be used in a resource-pool. These connections can be opened
-- either as read-only connection or read-write; depending on the client needs. Read-only connections
-- are non-blocking and can access data even when the database is being written concurrently.
createShortLivedConnection
    :: Tracer IO TraceDatabase
    -> ConnectionType
    -> DBLock IO
    -> LongestRollback
    -> DatabaseFile
    -> IO (Database IO)
createShortLivedConnection tr mode (DBLock shortLived longLived) k file = do
    traceWith tr $ DatabaseConnection ConnectionCreateShortLived{mode}
    let open = Sqlite.open (mkConnectionString file mode)
    conn <- handle (onOpenException open 0) open
    return $ mkDatabase (contramap DatabaseConnection tr) mode k (bracketConnection conn)
  where
    bracketConnection :: Connection -> (forall a. ((Connection -> IO a) -> IO a))
    bracketConnection conn between =
        case mode of
            WriteOnly ->
                between conn
            ReadOnly ->
                between conn
            ReadWrite ->
                bracket_
                    -- read-write connections only run when the longLived isn't busy working. Multiple
                    -- short-lived read-write connections may still conflict with one another, but
                    -- since they mostly are one-off requests, we simply retry them when busy/locked.
                    (atomically $ do
                        readTVar longLived >>= check . not
                        modifyTVar' shortLived next
                    )
                    (atomically (modifyTVar' shortLived prev))
                    (between conn)

    onOpenException :: IO Connection -> Int -> SQLError -> IO Connection
    onOpenException io n e
        | n > 2 =
            throwIO e
        | otherwise =
            case sqlError e of
                ErrorCan'tOpen ->
                    threadDelay 0.05 >> handle (onOpenException io (next n)) io
                ErrorLocked ->
                    threadDelay 0.05 >> handle (onOpenException io (next n)) io
                _ ->
                    throwIO e

withShortLivedConnection
    :: Tracer IO TraceDatabase
    -> ConnectionType
    -> DBLock IO
    -> LongestRollback
    -> DatabaseFile
    -> (Database IO -> IO a)
    -> IO a
withShortLivedConnection tr mode lock k file action = do
    bracket
        (createShortLivedConnection tr mode lock k file)
        (\Database{close} -> close)
        action

-- | A resource acquisition bracket for a single long-lived connection. The system is assumed to use
-- with only once, at the application start-up and provide this connection to a privileged one which
-- takes priority over any other connections.
--
-- It is therefore also the connection from which we check for and run database migrations when
-- needed. Note that this bracket will also create the database if it doesn't exist.
withLongLivedConnection
    :: Tracer IO TraceDatabase
    -> DBLock IO
    -> LongestRollback
    -> DatabaseFile
    -> DeferIndexesInstallation
    -> (Database IO -> IO a)
    -> IO a
withLongLivedConnection tr (DBLock shortLived longLived) k file deferIndexes action = do
    withConnection (mkConnectionString file ReadWrite) $ \conn -> do
        databaseVersion conn >>= runMigrations tr conn
        installIndexes tr conn deferIndexes
        execute_ conn "PRAGMA synchronous = NORMAL"
        execute_ conn "PRAGMA page_size = 16184"
        execute_ conn "PRAGMA journal_mode = TRUNCATE"
        execute_ conn "PRAGMA foreign_keys = ON"
        action (mkDatabase (contramap DatabaseConnection tr) ReadWrite k (bracketConnection conn))
  where
    bracketConnection :: Connection -> (forall a. ((Connection -> IO a) -> IO a))
    bracketConnection conn between =
        bracket_
            (do
                atomically (writeTVar longLived True) -- acquire
                atomically (readTVar shortLived >>= check . (== 0)) -- wait for read-write short-lived
            )
            (atomically $ writeTVar longLived False)
            (between conn)

-- It is therefore also the connection from which we check for and run database migrations when
-- needed. Note that this bracket will also create the database if it doesn't exist.
withWriteOnlyConnection
    :: DatabaseFile
    -> (Sqlite.Connection -> Database IO -> IO a)
    -> IO a
withWriteOnlyConnection file action = do
    withConnection (mkConnectionString file WriteOnly) $ \conn -> do
        databaseVersion conn >>= runMigrations nullTracer conn
        installIndexes nullTracer conn SkipNonEssentialIndexes
        execute_ conn "PRAGMA synchronous = OFF"
        execute_ conn "PRAGMA journal_mode = OFF"
        execute_ conn "PRAGMA locking_mode = EXCLUSIVE"
        execute_ conn "PRAGMA page_size = 65536"
        action conn (mkDatabase nullTracer ReadWrite k (bracketConnection conn))
  where
    k = LongestRollback maxBound

    bracketConnection :: Connection -> (forall a. ((Connection -> IO a) -> IO a))
    bracketConnection conn between =
        between conn

data CopyException
    = ErrCopyEmptyPatterns { hint :: Text }
    | ErrTargetAlreadyExists { target :: FilePath }
    | ErrMissingSourceDatabase { source :: FilePath }
    deriving (Show)

instance Exception CopyException

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
copyDatabase (tr, progress) fromDir intoDir patterns = do
    when (null patterns) $ do
        throwIO ErrCopyEmptyPatterns
            { hint = "No patterns provided for copy. At least one is required." }

    fromFile <- newDatabaseOnDiskFile tr (throwIO . ErrMissingSourceDatabase) fromDir
    intoFile <- newDatabaseOnDiskFile tr (traceWith tr . DatabaseCreateNew) intoDir

    cleanupFile <- newCleanupAction intoFile

    handle cleanupFile $ do
        traceWith tr DatabaseCloneSourceDatabase
        copyFile fromFile intoFile
        lock <- newLock
        withShortLivedConnection tr ReadOnly lock longestRollback (OnDisk fromFile) $ \from -> do
            withWriteOnlyConnection (OnDisk intoFile) $ \conn into -> do
                execute_ conn "PRAGMA foreign_keys = OFF"
                mapM_ (cleanup conn) ["inputs", "policies", "patterns"]
                runTransaction into (insertPatterns into patterns)
                forM_ patterns $ \pattern_ -> do
                    traceWith tr $ DatabaseImportTable { table = "inputs", pattern = patternToText pattern_ }
                    copyTable
                        (runTransaction from $ countInputs from pattern_)
                        (runTransaction from . foldInputs from pattern_ Whole NoStatusFlag Asc)
                        (runTransaction into . insertInputs into)
                        DB.resultToRow
                    traceWith tr $ DatabaseImportTable { table = "policies", pattern = patternToText pattern_ }
                    copyTable
                        (runTransaction from $ countPolicies from pattern_)
                        (runTransaction from . foldPolicies from pattern_)
                        (runTransaction into . insertPolicies into . fromList)
                        identity
                traceWith tr DatabaseCopyFinalize
                execute_ conn "VACUUM"
                execute_ conn "PRAGMA optimize"
  where
    longestRollback :: LongestRollback
    longestRollback =
        LongestRollback maxBound

    cleanup :: Connection -> Text -> IO ()
    cleanup conn table = do
        traceWith tr $ DatabaseCleanupOldData { table }
        execute_ conn $ Query $ "DELETE FROM " <> table

    newCleanupAction :: FilePath -> IO (SomeException -> IO a)
    newCleanupAction filePath = do
        whenM (doesFileExist filePath) (throwIO $ ErrTargetAlreadyExists { target = filePath })
        return $ \(e :: SomeException) -> do
            traceWith tr DatabaseRemoveIncompleteCopy { filePath }
            removePathForcibly filePath
            throwIO e

    copyTable
        :: IO Integer
        -> ((result -> IO ()) -> IO ())
        -> ([row] -> IO ())
        -> (result -> row)
        -> IO ()
    copyTable countTable foldTable insertTable mkRow = do
        queue <- newTBQueueIO 10_000
        done <- newTVarIO False
        total <- countTable
        concurrently_
            (do
                foldTable $ \result ->
                    atomically $ writeTBQueue queue (mkRow result)
                atomically $ writeTVar done True
            )
            ( let loop n = do
                    results <- atomically $ do
                        isDone <- readTVar done
                        isEmpty <- isEmptyTBQueue queue
                        check (not isEmpty || isDone)
                        flushTBQueue queue
                    insertTable results
                    let len = toInteger (length results)
                    unless (len == 0) $ do
                        traceWith progress $ ProgressStep (mkProgress total (n + len))
                        loop (n + len)
               in loop 0 >> do
                    traceWith progress ProgressDone
                    traceWith tr $ DatabaseImported { rows = total }
            )

    mkProgress :: Integer -> Integer -> Text
    mkProgress total n =
        scientific (round (double (n * 10000) / double total)) (-2)
        & formatScientificBuilder Fixed (Just 2)
        & T.toLazyText
        & toStrict
        & (<> "%")
      where
        double :: Integer -> Double
        double = fromIntegral

-- ** Lock

data DBLock (m :: Type -> Type) = DBLock !(TVar m Word) !(TVar m Bool)

newLock :: MonadSTM m => m (DBLock m)
newLock = DBLock <$> newTVarIO 0 <*> newTVarIO True

--
-- IO
--

mkDatabase
    :: Tracer IO TraceConnection
    -> ConnectionType
    -> LongestRollback
    -> (forall a. (Connection -> IO a) -> IO a)
    -> Database IO
mkDatabase tr mode longestRollback bracketConnection = Database
    { longestRollback

    , optimize = ReaderT $ \conn -> do
        -- NOTE: It is good to run the 'PRAGMA optimize' every now-and-then. The
        -- SQLite's official documentation recommend to do so either upon
        -- closing every connection, or, every few hours.
        traceExecute_ tr conn "PRAGMA optimize"

    , close = do
        traceWith tr ConnectionDestroyShortLived{mode}
        bracketConnection Sqlite.close

    , insertInputs = \inputs -> ReaderT $ \conn -> do
        mapM_
            (\DB.Input{..} -> do
                insertRow @"inputs" conn
                    [ SQLBlob extendedOutputReference
                    , SQLText address
                    , SQLBlob value
                    , maybe SQLNull SQLBlob datumInfo
                    , maybe SQLNull SQLBlob refScriptHash
                    , SQLInteger (fromIntegral createdAtSlotNo)
                    , maybe SQLNull (SQLInteger . fromIntegral) spentAtSlotNo
                    ]
                case datum of
                    Nothing ->
                        pure ()
                    Just DB.BinaryData{..} ->
                        insertRow @"binary_data" conn
                            [ SQLBlob binaryDataHash
                            , SQLBlob binaryData
                            ]
                case refScript of
                    Nothing ->
                        pure ()
                    Just DB.ScriptReference{..} ->
                        insertRow @"scripts" conn
                            [ SQLBlob scriptHash
                            , SQLBlob script
                            ]
            )
            inputs

    , deleteInputs = \refs -> ReaderT $ \conn -> do
        withTotalChanges conn $
            mapM_ (execute_ conn . deleteInputsQry) refs

    , markInputs = \(fromIntegral . unSlotNo -> slotNo) refs -> ReaderT $ \conn -> do
        withTotalChanges conn $
            forM_ refs $ \ref -> do
                execute conn (markInputsQry ref)
                    [ SQLInteger slotNo
                    ]

    , pruneInputs = ReaderT $ \conn -> do
        withTemporaryIndex tr conn "inputsBySpentAt" "inputs(spent_at)" $ do
            traceExecute tr conn pruneInputsQry [ SQLInteger (fromIntegral longestRollback) ]
        changes conn

    , foldInputs = \pattern_ slotRange statusFlag sortDirection yield -> ReaderT $ \conn -> do
        -- TODO: Allow resolving datums / scripts on demand through LEFT JOIN
        --
        -- See [#21](https://github.com/CardanoSolutions/kupo/issues/21)
        let (datum, refScript) = (Nothing, Nothing)
        Sqlite.fold_ conn (foldInputsQry pattern_ slotRange statusFlag sortDirection) () $ \() -> \case
            [  SQLBlob extendedOutputReference
             , SQLText address
             , SQLBlob value
             , matchMaybeBytes -> datumInfo
             , matchMaybeBytes -> refScriptHash
             , SQLInteger (fromIntegral -> createdAtSlotNo)
             , SQLBlob createdAtHeaderHash
             , matchMaybeWord64 -> spentAtSlotNo
             , matchMaybeBytes -> spentAtHeaderHash
             ] ->
                yield (DB.resultFromRow DB.Input{..})
            (xs :: [SQLData]) ->
                throwIO (UnexpectedRow (patternToText pattern_) [xs])

    , countInputs = \pattern_ -> ReaderT $ \conn -> do
        query_ conn (countInputsQry pattern_) >>= \case
            [[SQLInteger n]] ->
                pure (toInteger n)
            (xs :: [[SQLData]]) ->
                throwIO $ UnexpectedRow (fromQuery $ countInputsQry pattern_) xs

    , countPolicies = \pattern_ -> ReaderT $ \conn -> do
        query_ conn (countPoliciesQry pattern_) >>= \case
            [[SQLInteger n]] ->
                pure (toInteger n)
            (xs :: [[SQLData]]) ->
                throwIO $ UnexpectedRow (fromQuery $ countPoliciesQry pattern_) xs

    , foldPolicies = \pattern_ yield -> ReaderT $ \conn -> do
        Sqlite.fold_ conn (foldPoliciesQry pattern_) () $ \() -> \case
            [SQLBlob outputReference, SQLBlob policyId] ->
                yield DB.Policy{..}
            (xs :: [SQLData]) ->
                throwIO (UnexpectedRow "foldPolicies" [xs])

    , insertPolicies = \policies -> ReaderT $ \conn ->
        mapM_
            (\DB.Policy{..} -> do
                insertRow @"policies" conn
                    [ SQLBlob outputReference
                    , SQLBlob policyId
                    ]
            )
            policies

    , insertCheckpoints = \cps -> ReaderT $ \conn -> do
        mapM_
            (\(DB.pointToRow -> DB.Checkpoint{..}) ->
                insertRow @"checkpoints" conn
                    [ SQLBlob checkpointHeaderHash
                    , SQLInteger (fromIntegral checkpointSlotNo)
                    ]
            )
            cps

    , listCheckpointsDesc = ReaderT $ \conn -> do
        let k = fromIntegral longestRollback
        let points =
                [ 0, 10 .. k `div` 2 ^ n ]
                ++
                [ k `div` (2 ^ e) | (e :: Integer) <- [ n-1, n-2 .. 0 ] ]
              where
                n = ceiling (log (fromIntegral @_ @Double k))
        fmap (fmap DB.pointFromRow . nubOn DB.checkpointSlotNo . mconcat) $ forM points $ \pt ->
            Sqlite.fold conn listCheckpointsQry [SQLInteger pt] [] $
                \xs (checkpointHeaderHash, checkpointSlotNo) ->
                    pure (DB.Checkpoint{..} : xs)

    , listAncestorsDesc = \(SlotNo slotNo) n -> ReaderT $ \conn -> do
        fmap reverse $
            Sqlite.fold conn listAncestorQry (SQLInteger <$> [fromIntegral slotNo, n]) [] $
                \xs (checkpointHeaderHash, checkpointSlotNo) ->
                    pure ((DB.pointFromRow DB.Checkpoint{..}) : xs)

    , insertBinaryData = \bin -> ReaderT $ \conn -> do
        mapM_
            (\DB.BinaryData{..} ->
                insertRow @"binary_data" conn
                    [ SQLBlob binaryDataHash
                    , SQLBlob binaryData
                    ]
            )
            bin

    , getBinaryData = \(DB.datumHashToRow -> binaryDataHash) -> ReaderT $ \conn -> do
        Sqlite.query conn getBinaryDataQry (Only (SQLBlob binaryDataHash)) <&> \case
            [[SQLBlob binaryData]] ->
                Just (DB.binaryDataFromRow DB.BinaryData{..})
            _notSQLBlob ->
                Nothing

    , pruneBinaryData = ReaderT $ \conn -> do
        traceExecute_ tr conn pruneBinaryDataQry
        changes conn

    , insertScripts = \scripts -> ReaderT $ \conn -> do
        mapM_
            (\DB.ScriptReference{..} ->
                insertRow @"scripts" conn
                    [ SQLBlob scriptHash
                    , SQLBlob script
                    ]
            )
            scripts

    , getScript = \(DB.scriptHashToRow -> scriptHash)-> ReaderT $ \conn -> do
        Sqlite.query conn getScriptQry (Only (SQLBlob scriptHash)) <&> \case
            [[SQLBlob script]] ->
                Just (DB.scriptFromRow DB.ScriptReference{..})
            _notSQLBlob ->
                Nothing

    , insertPatterns = \patterns -> ReaderT $ \conn -> do
        mapM_
            (\pattern_ ->
                insertRow @"patterns" conn
                    [ SQLText (patternToText pattern_)
                    ]
            )
            patterns

    , deletePattern = \pattern_-> ReaderT $ \conn -> do
        execute conn "DELETE FROM patterns WHERE pattern = ?"
            [ SQLText (DB.patternToRow pattern_)
            ]
        changes conn

    , listPatterns = ReaderT $ \conn -> do
        fmap fromList
            $ fold_ conn "SELECT * FROM patterns" []
            $ \xs (Only x) -> pure (DB.patternFromRow x:xs)

    , rollbackTo = \(SQLInteger . fromIntegral . unSlotNo -> minSlotNo) -> ReaderT $ \conn -> do
        query_ conn selectMaxCheckpointQry >>= \case
            -- NOTE: Rolling back takes quite a bit of time and, when restarting
            -- the application, we'll always be asked to rollback to the
            -- _current tip_. In this case, there's nothing to delete or update,
            -- so we can safely skip it.
            [[currentSlotNo, _]] | currentSlotNo == minSlotNo -> do
                pure ()
            _otherwise -> do
                withTemporaryIndex tr conn "inputsByCreatedAt" "inputs(created_at)" $ do
                    withTemporaryIndex tr conn "inputsBySpentAt" "inputs(spent_at)" $ do
                        traceExecute tr conn rollbackQryDeleteInputs [ minSlotNo ]
                        traceExecute tr conn rollbackQryUpdateInputs [ minSlotNo ]
                        traceExecute tr conn rollbackQryDeleteCheckpoints [ minSlotNo ]
        query_ conn selectMaxCheckpointQry >>= \case
            [[SQLInteger (fromIntegral -> checkpointSlotNo), SQLBlob checkpointHeaderHash]] ->
                return $ Just (DB.pointFromRow DB.Checkpoint{..})
            [[SQLNull, SQLNull]] ->
                return Nothing
            xs ->
                throwIO $ UnexpectedRow (fromQuery selectMaxCheckpointQry) xs

    , runTransaction = \r -> bracketConnection $ \conn ->
        retryWhenBusy tr $ withTransaction conn mode (runReaderT r conn)
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

deleteInputsQry :: Pattern -> Query
deleteInputsQry pattern_ =
    Query $ unwords
        [ "DELETE FROM inputs"
        , additionalJoin
        , "WHERE"
        , whereClause
        ]
  where
    (whereClause, fromMaybe "" -> additionalJoin) = patternToSql pattern_

markInputsQry :: Pattern -> Query
markInputsQry pattern_ =
    Query $ unwords
        [ "UPDATE inputs SET spent_at = ?"
        , additionalJoin
        , "WHERE"
        , whereClause
        ]
  where
      (whereClause, fromMaybe "" -> additionalJoin) = patternToSql pattern_

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

countPoliciesQry :: Pattern -> Query
countPoliciesQry pattern_ = Query $
    "SELECT COUNT(*) \
    \FROM policies \
    \JOIN inputs \
    \ON inputs.output_reference = policies.output_reference"
    <> " WHERE "
    <> patternWhereClause
  where
    (patternWhereClause, _) =
        patternToSql pattern_

foldPoliciesQry :: Pattern -> Query
foldPoliciesQry pattern_ = Query $
    "SELECT policies.output_reference, policy_id \
    \FROM policies \
    \JOIN inputs \
    \ON inputs.output_reference = policies.output_reference"
    <> " WHERE "
    <> patternWhereClause
  where
    (patternWhereClause, _) =
        patternToSql pattern_

countInputsQry :: Pattern -> Query
countInputsQry pattern_ = Query $
    "SELECT COUNT(*) FROM inputs "
    <> additionalJoin
    <> " WHERE "
    <> patternWhereClause
  where
    (patternWhereClause, fromMaybe "" -> additionalJoin) =
        patternToSql pattern_

foldInputsQry
    :: Pattern
    -> Range SlotNo
    -> StatusFlag
    -> SortDirection
    -> Query
foldInputsQry pattern_ slotRange statusFlag sortDirection =
    Query $ "SELECT \
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
            [ patternWhereClause
            , slotRangeToSql slotRange
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
pruneBinaryDataQry :: Query
pruneBinaryDataQry =
    " DELETE FROM binary_data \
    \ WHERE binary_data_hash IN ( \
    \   SELECT binary_data_hash FROM binary_data \
    \   LEFT JOIN inputs \
    \   ON binary_data_hash = inputs.datum_hash \
    \   WHERE inputs.ext_output_reference IS NULL \
    \   ORDER BY inputs.datum_hash \
    \ )"

getScriptQry :: Query
getScriptQry =
    "SELECT script FROM scripts \
    \WHERE script_hash = ? \
    \LIMIT 1"

selectMaxCheckpointQry :: Query
selectMaxCheckpointQry =
    "SELECT MAX(slot_no),header_hash FROM checkpoints"

rollbackQryDeleteInputs :: Query
rollbackQryDeleteInputs =
    "DELETE FROM inputs WHERE created_at > ?"

rollbackQryUpdateInputs :: Query
rollbackQryUpdateInputs =
    "UPDATE inputs SET spent_at = NULL WHERE spent_at > ?"

rollbackQryDeleteCheckpoints :: Query
rollbackQryDeleteCheckpoints =
    "DELETE FROM checkpoints WHERE slot_no > ?"

--
-- Helpers
--

mkPreparedStatement :: Int -> Query
mkPreparedStatement n =
    Query ("(" <> T.intercalate "," (replicate n "?") <> ")")
{-# INLINABLE mkPreparedStatement #-}

retryWhenBusy :: Tracer IO TraceConnection -> IO a -> IO a
retryWhenBusy tr action =
    action `catch` (\e@SQLError{sqlError} -> case sqlError of
        ErrorLocked -> do
            traceWith tr $ ConnectionLocked { retryingIn }
            threadDelay retryingIn
            retryWhenBusy tr action
        ErrorBusy -> do
            traceWith tr $ ConnectionBusy { retryingIn }
            threadDelay retryingIn
            retryWhenBusy tr action
        _otherError ->
            throwIO e
    )
  where
    retryingIn = 0.1

-- NOTE: Not using sqlite-simple's version because it lacks the crucial
-- 'onException' on commits; The commit operation may throw an 'SQLiteBusy'
-- exception when concurrent transactions are begin executed.
-- Yet, because it doesn't rollback in this case, it leaves the transaction in
-- an odd shrodinger state and makes it hard for the caller to properly handle
-- the exception (was the transaction rolled back or not? Is it safe to retry
-- it?). So, this slightly modified version makes sure to also rollback on a
-- failed commit; allowing caller to simply retry the whole transaction on
-- failure.
withTransaction :: Connection -> ConnectionType -> IO a -> IO a
withTransaction conn mode action =
  mask $ \restore -> do
    begin mode
    r <- restore action `onException` rollback
    commit `onException` rollback
    return r
  where
    begin = \case
        ReadOnly ->
            execute_ conn "BEGIN DEFERRED TRANSACTION"
        ReadWrite ->
            execute_ conn "BEGIN IMMEDIATE TRANSACTION"
        WriteOnly ->
            execute_ conn "BEGIN EXCLUSIVE TRANSACTION"
    commit   = execute_ conn "COMMIT TRANSACTION"
    rollback = execute_ conn "ROLLBACK TRANSACTION"

-- Run one or more effectful queries (DELETE, UPDATE, ...) and return the total number
-- of changes.
withTotalChanges :: Connection -> IO () -> IO Int
withTotalChanges conn between = do
    n1 <- totalChanges conn
    between
    n2 <- totalChanges conn
    return (n2 - n1)

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
        dropIndexIfExists (contramap DatabaseConnection tr) conn "inputsByPaymentCredential" False
        dropIndexIfExists (contramap DatabaseConnection tr) conn "inputsByCreatedAt" False
        dropIndexIfExists (contramap DatabaseConnection tr) conn "inputsBySpentAt" False
        dropIndexIfExists (contramap DatabaseConnection tr) conn "policiesByPolicyId" False
    InstallIndexesIfNotExist -> do
        installIndex tr conn
            "inputsByAddress"
            "inputs(address COLLATE NOCASE)"
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
            execute_ conn $ Query $ unwords
                [ "CREATE INDEX IF NOT EXISTS"
                , name
                , "ON"
                , definition
                ]
        True ->
            traceWith tr (DatabaseIndexAlreadyExists name)

-- This creates an index on-the-fly if it is missing to make the subsequent queries fast-enough on
-- large databases. If the index was not there, it is removed afterwards. Otherwise, it is simply used
-- as such.
withTemporaryIndex :: Tracer IO TraceConnection -> Connection -> Text -> Text -> IO a -> IO a
withTemporaryIndex tr conn name definition action = do
    exists <- indexDoesExist conn name
    unless exists $ traceWith tr (ConnectionCreateTemporaryIndex name)
    execute_ conn $ Query $ unwords
        [ "CREATE INDEX IF NOT EXISTS"
        , name
        , "ON"
        , definition
        ]
    unless exists $ traceWith tr (ConnectionCreatedTemporaryIndex name)
    a <- action
    unless exists (dropIndexIfExists tr conn name True)
    return a

-- | Check whether an index exists in the database. Handy to customize the behavior (e.g. logging)
-- depending on whether or not indexes are already there since 'CREATE INDEX IF NOT EXISTS' will not
-- tell whether or not it has indeed created something.
indexDoesExist :: Connection -> Text -> IO Bool
indexDoesExist conn name =
    query_ @[SQLData] conn (Query $ "PRAGMA index_info('" <> name <> "')") <&> \case
        [] -> False
        _doesExist -> True

dropIndexIfExists :: Tracer IO TraceConnection -> Connection -> Text -> Bool -> IO ()
dropIndexIfExists tr conn indexName wasTemporary = do
    whenM (indexDoesExist conn indexName) $ traceWith tr $ if wasTemporary
        then ConnectionRemoveTemporaryIndex{indexName}
        else ConnectionRemoveIndex{indexName}
    execute_ conn $ Query $ unwords
        [ "DROP INDEX IF EXISTS"
        , indexName
        ]

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
            void $ withTransaction conn ReadWrite $ traverse (execute_ conn) instructions
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
        , $(embedFile "db/v2.1.0/001.sql")
        , $(embedFile "db/v2.1.0/002.sql")
        , $(embedFile "db/v2.1.0/003.sql")
        , $(embedFile "db/v2.2.0/001.sql")
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
-- Tracer
--

data TraceDatabase where
    DatabaseCreateNew
        :: { filePath :: FilePath }
        -> TraceDatabase
    DatabaseConnection
        :: { message :: TraceConnection }
        -> TraceDatabase
    DatabaseCurrentVersion
        :: { currentVersion :: Int }
        -> TraceDatabase
    DatabaseNoMigrationNeeded
        :: TraceDatabase
    DatabaseRunningMigration
        :: { from :: Int, to :: Int }
        -> TraceDatabase
    DatabaseCreateIndex
        :: { newIndex :: Text }
        -> TraceDatabase
    DatabaseIndexAlreadyExists
        :: { index :: Text }
        -> TraceDatabase
    DatabaseDeferIndexes
        :: { warning :: Text }
        -> TraceDatabase
    DatabaseRunningInMemory
        :: TraceDatabase
    DatabasePathMustBeDirectory
        :: { hint :: Text }
        -> TraceDatabase
    DatabaseCloneSourceDatabase
        :: TraceDatabase
    DatabaseCleanupOldData
        :: { table :: Text }
        -> TraceDatabase
    DatabaseImportTable
        :: { table :: Text, pattern :: Text }
        -> TraceDatabase
    DatabaseImported
        :: { rows :: Integer }
        -> TraceDatabase
    DatabaseRemoveIncompleteCopy
        :: { filePath :: FilePath }
        -> TraceDatabase
    DatabaseCopyFinalize
        :: TraceDatabase
    DatabaseDebug
        :: Text
        -> TraceDatabase
    deriving stock (Generic, Show)

instance ToJSON TraceDatabase where
    toEncoding =
        defaultGenericToEncoding

instance HasSeverityAnnotation TraceDatabase where
    getSeverityAnnotation = \case
        DatabaseCreateNew{}            -> Notice
        DatabaseConnection { message } -> getSeverityAnnotation message
        DatabaseCurrentVersion{}       -> Info
        DatabaseNoMigrationNeeded{}    -> Debug
        DatabaseRunningMigration{}     -> Notice
        DatabaseRunningInMemory{}      -> Warning
        DatabaseCreateIndex{}          -> Notice
        DatabaseIndexAlreadyExists{}   -> Debug
        DatabasePathMustBeDirectory{}  -> Error
        DatabaseDeferIndexes{}         -> Warning
        DatabaseCloneSourceDatabase{}  -> Notice
        DatabaseCleanupOldData{}       -> Info
        DatabaseImportTable{}          -> Notice
        DatabaseImported{}             -> Info
        DatabaseRemoveIncompleteCopy{} -> Notice
        DatabaseCopyFinalize{}         -> Notice
        DatabaseDebug{}                -> Warning

data TraceConnection where
    ConnectionCreateShortLived
        :: { mode :: ConnectionType }
        -> TraceConnection
    ConnectionDestroyShortLived
        :: { mode :: ConnectionType }
        -> TraceConnection
    ConnectionLocked
        :: { retryingIn :: DiffTime }
        -> TraceConnection
    ConnectionBusy
        :: { retryingIn :: DiffTime }
        -> TraceConnection
    ConnectionBeginQuery
        :: { beginQuery :: LText }
        -> TraceConnection
    ConnectionExitQuery
        :: { exitQuery :: LText }
        -> TraceConnection
    ConnectionCreateTemporaryIndex
        :: { newTemporaryIndex :: Text }
        -> TraceConnection
    ConnectionCreatedTemporaryIndex
        :: { newTemporaryIndex :: Text }
        -> TraceConnection
    ConnectionRemoveTemporaryIndex
        :: { indexName :: Text }
        -> TraceConnection
    ConnectionRemoveIndex
        :: { indexName :: Text }
        -> TraceConnection
    deriving stock (Generic, Show)

instance ToJSON TraceConnection where
    toEncoding =
        defaultGenericToEncoding

instance HasSeverityAnnotation TraceConnection where
    getSeverityAnnotation = \case
        ConnectionCreateShortLived{}          -> Debug
        ConnectionDestroyShortLived{}         -> Debug
        ConnectionLocked{}                    -> Debug
        ConnectionBusy{}                      -> Debug
        ConnectionBeginQuery{beginQuery}
            | beginQuery == "PRAGMA optimize" -> Notice
        ConnectionBeginQuery{}                -> Debug
        ConnectionExitQuery{exitQuery}
            | exitQuery == "PRAGMA optimize"  -> Notice
        ConnectionExitQuery{}                 -> Debug
        ConnectionCreateTemporaryIndex{}      -> Debug
        ConnectionCreatedTemporaryIndex{}     -> Debug
        ConnectionRemoveTemporaryIndex{}      -> Debug
        ConnectionRemoveIndex{}               -> Warning

traceExecute
    :: ToRow q
    => Tracer IO TraceConnection
    -> Connection
    -> Query
    -> q
    -> IO ()
traceExecute tr conn template qs = do
    traceWith tr $ ConnectionBeginQuery (trim template)
    execute conn template qs
    traceWith tr $ ConnectionExitQuery (trim template)

traceExecute_
    :: Tracer IO TraceConnection
    -> Connection
    -> Query
    -> IO ()
traceExecute_ tr conn template = do
    traceWith tr $ ConnectionBeginQuery (trim template)
    execute_ conn template
    traceWith tr $ ConnectionExitQuery (trim template)

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
    fromQuery
