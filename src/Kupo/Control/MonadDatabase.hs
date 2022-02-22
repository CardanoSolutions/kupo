--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Kupo.Control.MonadDatabase
    ( -- * Database DSL
      MonadDatabase (..)
    , Database (..)

      -- * SQL Interface
    , Row (..)
    , SQLData (..)
    ) where

import Kupo.Prelude

import Control.Exception
    ( throwIO )
import Data.FileEmbed
    ( embedFile )
import Database.SQLite.Simple
    ( Connection
    , Only (..)
    , Query (..)
    , SQLData (..)
    , ToRow (..)
    , executeMany
    , execute_
    , nextRow
    , withConnection
    , withStatement
    , withTransaction
    )
import GHC.TypeLits
    ( KnownSymbol, Symbol, symbolVal )

import qualified Data.Text as T

class Monad m => MonadDatabase (m :: Type -> Type) where
    withDatabase
        :: FilePath
        -> (Database m -> m a)
        -> m a

data Database (m :: Type -> Type) = Database
    { insertMany
        :: forall tableName. (IsTable tableName)
        => [Row tableName]
        -> m ()
    }

class KnownSymbol tableName => IsTable (tableName :: Symbol) where
    numberOfColumns :: Int

    tableName :: Query
    tableName = fromString (symbolVal (Proxy @tableName))

-- | Change the underlying 'Monad' of a 'Database' using a natural transformation.
natDatabase :: (forall a. m a -> n a) -> Database m -> Database n
natDatabase nat Database{..} = Database
    { insertMany = nat . insertMany
    }

data Row (tableName :: Symbol) where
    Row :: [SQLData] -> Row tableName

instance ToRow (Row tableName) where
    toRow (Row fields) = fields

--
-- ReaderT
--

instance MonadDatabase m => MonadDatabase (ReaderT r m) where
    withDatabase filePath action = ReaderT $ \r ->
        withDatabase filePath ((`runReaderT` r) . action . natDatabase @m lift)

--
-- IO
--

instance MonadDatabase IO where
    withDatabase filePath action =
        withConnection filePath $ \conn -> do
            databaseVersion conn >>= runMigrations conn
            action (mkDatabase conn)

mkDatabase :: Connection -> Database IO
mkDatabase conn = Database
    { insertMany = safeInsertMany conn
    }

--
-- Helpers
--

safeInsertMany
    :: forall tableName.
        ( IsTable tableName
        )
    => Connection
    -> [Row tableName]
    -> IO ()
safeInsertMany conn rows
    | length rows > limit = do
        let (front, rear) = splitAt (limit `div` (numberOfColumns @tableName)) rows
        insertRows conn front
        safeInsertMany conn rear
    | otherwise = do
        insertRows conn rows
  where
    -- NOTE:
    -- There's a (non-configurable) limit of 999 values per query in SQLite.
    limit = 999

insertRows
    :: forall tableName.
        ( IsTable tableName
        )
    => Connection
    -> [Row tableName]
    -> IO ()
insertRows conn = \case
    [] -> pure ()
    rows@(h:_) ->
        let
            values = mkPreparedStatement (length (toRow h))
            qry = "INSERT OR IGNORE INTO " <> tableName @tableName <> " VALUES " <> values
         in
            executeMany conn qry rows

mkPreparedStatement :: Int -> Query
mkPreparedStatement n =
    Query ("(" <> T.intercalate "," (replicate n "?") <> ")")

--
-- Migrations
--

instance IsTable "inputs" where
    numberOfColumns = 5
instance IsTable "addresses" where
    numberOfColumns = 2

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

runMigrations :: Connection -> MigrationRevision -> IO ()
runMigrations conn currentVersion = do
    let missingMigrations = drop currentVersion migrations
    if null missingMigrations then
        putStrLn $ "No migration to run; version=" <> show currentVersion
    else do
        putStrLn $ "Running " <> show (length missingMigrations) <> " migration(s) from version=" <> show currentVersion
        void $ withTransaction conn $ traverse (traverse (execute_ conn)) missingMigrations

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
