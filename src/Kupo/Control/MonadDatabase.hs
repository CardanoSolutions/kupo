--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module Kupo.Control.MonadDatabase
    ( -- * Database DSL
      MonadDatabase (..)
    , databaseFilePath
    , Database (..)
    , TableName

      -- * SQL Interface
    , ToRow (..)
    , ToField (..)
    , SQLData (..)
    ) where

import Kupo.Prelude

import Control.Exception
    ( throwIO )
import Database.SQLite.Simple
    ( Connection
    , Only (..)
    , Query (..)
    , SQLData (..)
    , ToRow (..)
    , executeMany
    , nextRow
    , withConnection
    , withStatement
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

type TableName = String

databaseFilePath :: FilePath -> FilePath
databaseFilePath workDir = workDir </> "kupo.sqlite3"

data Database (m :: Type -> Type) = Database
    { insertMany :: forall row. (ToRow row) => TableName -> [row] -> m ()
    , mostRecentMigration :: m Integer
    }

-- | Change the underlying 'Monad' of a 'Database' using a natural transformation.
natDatabase :: (forall a. m a -> n a) -> Database m -> Database n
natDatabase nat Database{..} = Database
    { insertMany = \a0 a1 -> nat (insertMany a0 a1)
    , mostRecentMigration = nat mostRecentMigration
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
        withConnection filePath (action . mkDatabase)

mkDatabase :: Connection -> Database IO
mkDatabase conn = Database
    { insertMany = \(fromString -> tableName) -> \case
        [] ->
            pure ()
        rows@(h:_) ->
            let
                values = mkRowStatement (length (toRow h))
             in
                executeMany conn
                    ("INSERT INTO " <> tableName <> " VALUES " <> values)
                    rows

    , mostRecentMigration = do
        withStatement conn "PRAGMA user_version" $ \stmt -> do
            nextRow stmt >>= \case
                Just (Only version) ->
                    pure version
                _ ->
                    throwIO UnexpectedUserVersion
    }

--
-- Helpers
--

mkRowStatement :: Int -> Query
mkRowStatement n =
    Query ("(" <> T.intercalate "," (replicate n "?") <> ")")

--
-- Exceptions
--

-- | Somehow, a 'PRAGMA user_version' didn't yield a number but, either nothing
-- or something else?
data UnexpectedUserVersionException
    = UnexpectedUserVersion
    deriving Show
instance Exception UnexpectedUserVersionException
