--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Kupo.Control.MonadDatabase
    ( -- * Database DSL
      MonadDatabase (..)
    , Database (..)

      -- * SQL Interface
    , Row (..)
    , Reference (ConcreteReference)
    , ReferenceKind (..)
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
    , query
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
        :: forall tableName referenceTableName.
            ( KnownSymbol tableName
            , KnownSymbol referenceTableName
            )
        => [Row tableName (Concrete referenceTableName)]
        -> m ()
    }

-- | Change the underlying 'Monad' of a 'Database' using a natural transformation.
natDatabase :: (forall a. m a -> n a) -> Database m -> Database n
natDatabase nat Database{..} = Database
    { insertMany = nat . insertMany
    }

data Row (tableName :: Symbol) (reference :: ReferenceKind) where
    Row :: Reference reference -> [SQLData] -> Row tableName reference

instance ToRow (Row tableName Symbolic) where
    toRow (Row (SymbolicReference ref) fields) =
        SQLInteger ref : fields

instance ToRow (Row tableName (Concrete any)) where
    toRow (Row (ConcreteReference (_columnName, field)) _fields) =
        [field]

data ReferenceKind = Concrete Symbol | Symbolic

data Reference (reference :: ReferenceKind) where
    ConcreteReference
        :: forall tableName. KnownSymbol tableName
        => (ColumnName, SQLData)
        -> Reference (Concrete tableName)
    SymbolicReference
        :: Int64
        -> Reference Symbolic

type ColumnName = String

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
    { insertMany = \rows ->
        withTransaction conn $ do
            insertReferences conn rows
            resolveReferences conn rows >>= insertRows conn
    }

--
-- Helpers
--

insertReferences
    :: forall tableName any.
        ( KnownSymbol tableName
        )
    => Connection
    -> [Row any (Concrete tableName)]
    -> IO ()
insertReferences conn = \case
    [] -> pure ()
    rows@((Row (ConcreteReference (fromString -> columnName, _)) _fields):_rest) ->
        let
            tableName = fromString (symbolVal (Proxy @tableName))
            values = mkPreparedStatement 1
            qry = "INSERT OR IGNORE INTO " <> tableName <> "(" <> columnName <> ") VALUES " <> values
         in
            executeMany conn qry rows

resolveReferences
    :: forall tableName any.
        ( KnownSymbol tableName
        )
    => Connection
    -> [Row any (Concrete tableName)]
    -> IO [Row any Symbolic]
resolveReferences conn = \case
    [] -> pure []
    rows@((Row (ConcreteReference (fromString -> columnName, _)) _fields):_rest) -> do
        let
            tableName = fromString (symbolVal (Proxy @tableName))
            values = mkPreparedStatement (length rows)
            qry = "SELECT id FROM " <> tableName <> " WHERE " <> columnName <> " IN " <> values
         in do
            ids <- query conn qry $ (\(Row (ConcreteReference (_, f)) _) -> f) <$> rows
            pure $ zipWith (\(Only i) (Row _ fields) -> Row (SymbolicReference i) fields) ids rows

insertRows
    :: forall tableName.
        ( KnownSymbol tableName
        )
    => Connection
    -> [Row tableName Symbolic]
    -> IO ()
insertRows conn = \case
    [] -> pure ()
    rows@(h:_) ->
        let
            tableName = fromString (symbolVal (Proxy @tableName))
            values = mkPreparedStatement (length (toRow h))
            qry = "INSERT OR IGNORE INTO " <> tableName <> " VALUES " <> values
         in
            executeMany conn qry rows

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
