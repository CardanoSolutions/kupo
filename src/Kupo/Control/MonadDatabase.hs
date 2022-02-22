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
    , execute
    , execute_
    , nextRow
    , withConnection
    , withStatement
    , withTransaction
    )
import GHC.TypeLits
    ( KnownSymbol, Symbol, symbolVal )

import qualified Data.Text as T

class (Monad m, Monad (Transaction m)) => MonadDatabase (m :: Type -> Type) where
    type Transaction m :: (Type -> Type)
    withDatabase
        :: FilePath
        -> (Database m -> m a)
        -> m a

data Database (m :: Type -> Type) = Database
    { insertMany
        :: forall tableName. (KnownSymbol tableName)
        => [Row tableName]
        -> Transaction m ()

    , runTransaction
        :: Transaction m ()
        -> m ()
    }

data Row (tableName :: Symbol) where
    Row :: [SQLData] -> Row tableName

instance ToRow (Row tableName) where
    toRow (Row fields) = fields

--
-- IO
--

newtype WrappedIO a = WrappedIO { runIO :: IO a }
    deriving newtype (Functor, Applicative, Monad)

instance MonadDatabase IO where
    type Transaction IO = WrappedIO
    withDatabase filePath action =
        withConnection filePath $ \conn -> do
            databaseVersion conn >>= runMigrations conn
            action (mkDatabase conn)

mkDatabase :: Connection -> Database IO
mkDatabase conn = Database
    { insertMany = WrappedIO . mapM_ (insertRow conn)
    , runTransaction = withTransaction conn . runIO
    }

insertRow
    :: forall tableName.
        ( KnownSymbol tableName
        )
    => Connection
    -> Row tableName
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
