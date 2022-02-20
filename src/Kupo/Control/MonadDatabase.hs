--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Kupo.Control.MonadDatabase
    ( -- * Database DSL
      MonadDatabase (..)
    , Database (..)
    , databaseFilePath

      -- ** Tables
    , TableName
    , tableInputs
    , tableAddresses

      -- * SQL Interface
    , ToRow (..)
    , ToField (..)
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
import Database.SQLite.Simple.ToField
    ( ToField (..) )
import System.FilePath
    ( (</>) )

import qualified Data.Text as T

class Monad m => MonadDatabase (m :: Type -> Type) where
    withDatabase
        :: FilePath
        -> (Database m -> m a)
        -> m a

databaseFilePath :: FilePath -> FilePath
databaseFilePath workDir = workDir </> "kupo.sqlite3"

data Database (m :: Type -> Type) = Database
    { insertMany :: forall row. (ToRow row) => TableName -> [row] -> m ()
    }

-- | Change the underlying 'Monad' of a 'Database' using a natural transformation.
natDatabase :: (forall a. m a -> n a) -> Database m -> Database n
natDatabase nat Database{..} = Database
    { insertMany = \a0 a1 -> nat (insertMany a0 a1)
    }

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
    { insertMany = \(fromString -> tableName) -> \case
        [] ->
            pure ()
        rows@(h:_) ->
            let
                values = mkPreparedStatement (length (toRow h))
                query = "INSERT INTO " <> tableName <> " VALUES " <> values
             in
                executeMany conn query rows
    }

--
-- Helpers
--

mkPreparedStatement :: Int -> Query
mkPreparedStatement n =
    Query ("(" <> T.intercalate "," (replicate n "?") <> ")")

--
-- Tables
--

type TableName = String

tableInputs :: TableName
tableInputs = "inputs"

tableAddresses :: TableName
tableAddresses = "addresses"

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
