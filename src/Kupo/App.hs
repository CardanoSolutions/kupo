--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module Kupo.App
    ( producer
    , consumer
    ) where

import Kupo.Prelude

import Kupo.App.ChainSync
    ( ChainSyncHandler (..) )
import Kupo.App.Mailbox
    ( Mailbox, flushMailbox, putMailbox )
import Kupo.Configuration
    ( StandardCrypto )
import Kupo.Control.MonadDatabase
    ( Database (..)
    , Reference (..)
    , ReferenceKind (..)
    , Row (..)
    , SQLData (..)
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..) )
import Kupo.Data.ChainSync
    ( Block, SlotNo (..) )
import Kupo.Data.Pattern
    ( Pattern, Result (..), matchBlock )

import qualified Prelude

--
-- Producer / Consumer
--

producer
    :: forall m.
        ( MonadSTM m
        , MonadIO m -- FIXME, temporary for strawman logging until proper tracer.
        )
    => Mailbox m (Block StandardCrypto)
    -> ChainSyncHandler m (Block StandardCrypto)
producer mailbox = ChainSyncHandler
    { onRollBackward = \pt -> do
        -- FIXME: rollbacks should rewind the database.
        liftIO $ putStrLn $ "RollBack to " <> show pt <> "; doing nothing about it."
    , onRollForward =
        atomically . putMailbox mailbox
    }

consumer
    :: forall m.
        ( MonadSTM m
        , MonadIO m
        )
    => Mailbox m (Block StandardCrypto)
    -> [Pattern StandardCrypto]
    -> Database m
    -> m ()
consumer mailbox patterns Database{insertMany} = forever $ do
    blks <- atomically (flushMailbox mailbox)
    let rows = concatMap (matchBlock resultToRow patterns) blks
    let len = length rows
    when (len > 0) $ do
        liftIO $ putStrLn $ "Inserting " <> show len <> " rows."
        let maxSlot = (\(Row _ fields) -> show (Prelude.last fields)) $ Prelude.last rows
        liftIO $ putStrLn $ "Last known slot: " <> maxSlot
    insertMany rows

--
-- SQL interface
--

resultToRow
    :: Result StandardCrypto
    -> Row "inputs" (Concrete "addresses")
resultToRow Result{..} = Row
    (ConcreteReference ("address", SQLBlob (serialize' address)))
    [ SQLBlob (serialize' reference)
    , SQLBlob (serialize' value)
    , maybe SQLNull (SQLBlob . serialize') datumHash
    , SQLInteger (fromIntegral (unSlotNo slotNo))
    ]
