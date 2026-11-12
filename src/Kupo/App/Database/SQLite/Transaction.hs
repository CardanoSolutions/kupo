--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.Database.SQLite.Transaction
    ( withTransaction
    ) where

import Kupo.Prelude

import Control.Exception
    ( mask
    , onException
    )
import Database.SQLite.Simple
    ( Connection
    , execute_
    )
import Kupo.App.Database.Types
    ( ConnectionType (..)
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
