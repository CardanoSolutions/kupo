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
    , Database (..)
    , LongestRollback (..)

      -- * Tracer
    , TraceDatabase (..)
    ) where

import Kupo.Prelude hiding
    ( fold )

import Control.Exception
    ( throwIO )
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
    , fold
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
    withDatabase
        :: Tracer m TraceDatabase
        -> LongestRollback
        -> FilePath
        -> (Database m -> m a)
        -> m a

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
        :: forall result. ()
        => Text  -- An address-like query
        -> result
        -> (  ByteString         -- output_reference
           -> Text               -- address
           -> ByteString         -- value
           -> Maybe ByteString   -- datum_hash
           -> Word64             -- slot_no
           -> result
           -> m result
           )
        -> DBTransaction m result

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

newtype WrappedIO a = WrappedIO { runIO :: IO a }
    deriving newtype (Functor, Applicative, Monad)

instance MonadDatabase IO where
    type DBTransaction IO = WrappedIO
    withDatabase tr k filePath action =
        withConnection filePath $ \conn -> do
            databaseVersion conn >>= runMigrations tr conn
            action (mkDatabase k conn)

mkDatabase :: LongestRollback -> Connection -> Database IO
mkDatabase (toInteger -> longestRollback) conn = Database
    { insertInputs = WrappedIO . mapM_
        (\(outputReference, address, value, datumHash, fromIntegral -> slotNo) ->
            insertRow @"inputs" conn
                [ SQLBlob outputReference
                , SQLText address
                , SQLBlob value
                , maybe SQLNull SQLBlob datumHash
                , SQLInteger slotNo
                ]
        )

    , foldInputsByAddress = \addressLike result0 yield -> WrappedIO $ do
        let matchMaybeDatumHash = \case
                SQLBlob datumHash -> Just datumHash
                _ -> Nothing
        fold conn "SELECT * FROM inputs WHERE address LIKE ?" (Only addressLike) result0 $ \result -> \case
            [ SQLBlob outputReference
                , SQLText address
                , SQLBlob value
                , matchMaybeDatumHash -> datumHash
                , SQLInteger (fromIntegral -> slotNo)
                ] -> yield outputReference address value datumHash slotNo result
            (xs :: [SQLData]) -> throwIO (UnexpectedRow addressLike xs)


    , insertCheckpoint = \headerHash (toInteger -> slotNo) -> WrappedIO $ do
        insertRow @"checkpoints" conn
            [ SQLBlob headerHash
            , SQLInteger (fromIntegral slotNo)
            ]
        execute conn "DELETE FROM checkpoints WHERE slot_no < ?"
            [ SQLInteger (fromIntegral (slotNo - longestRollback))
            ]

    , listCheckpointsDesc = \mk -> WrappedIO $ do
        -- NOTE: fetching in *ASC*ending order because the list construction
        -- reverses it,
        fold_ conn "SELECT * FROM checkpoints ORDER BY slot_no ASC" []
            $ \xs (headerHash, slotNo) -> pure ((mk headerHash slotNo) : xs)

    , rollbackTo = \(fromIntegral -> slotNo) -> WrappedIO $ do
        execute conn "DELETE FROM inputs WHERE slot_no > ?"
            [ SQLInteger slotNo
            ]
        execute conn "DELETE FROM checkpoints WHERE slot_no > ?"
            [ SQLInteger slotNo
            ]

    , runTransaction =
        withTransaction conn . runIO
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
