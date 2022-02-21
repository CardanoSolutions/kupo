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

--
-- Producer / Consumer
--

producer
    :: forall m.
        ( MonadSTM m
        , MonadIO m -- FIXME, temporary for strawman logging until proper tracer.
        )
    => Mailbox m (Row "inputs" (Concrete "addresses"))
    -> [Pattern StandardCrypto]
    -> ChainSyncHandler m (Block StandardCrypto)
producer mailbox patterns = ChainSyncHandler
    { onRollBackward = \pt -> do
        -- FIXME: rollbacks should rewind the database.
        liftIO $ putStrLn $ "RollBack to " <> show pt <> "; doing nothing about it."
    , onRollForward = \blk -> do
        atomically $ mapM_
            (putMailbox mailbox . resultToRow)
            (matchBlock patterns blk)
    }

consumer
    :: forall m.
        ( MonadSTM m
        )
    => Mailbox m (Row "inputs" (Concrete "addresses"))
    -> Database m
    -> m ()
consumer mailbox Database{insertMany} = forever $ do
    atomically (flushMailbox mailbox) >>= insertMany

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
