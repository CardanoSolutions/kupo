--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Control.MonadDatabase
    ( -- * Database DSL
      MonadDatabase (..)
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

import qualified Data.Text as T

class MonadDatabase (m :: Type -> Type) where
    withDatabase
        :: FilePath
        -> (Database m -> m a)
        -> m a

type TableName = String

data Database m = Database
    { insertMany :: forall row. (ToRow row) => TableName -> [row] -> m ()
    , mostRecentMigration :: m Integer
    }

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
