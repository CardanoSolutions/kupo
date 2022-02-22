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
    ( Database (..), Row (..), SQLData (..) )
import Kupo.Control.MonadSTM
    ( MonadSTM (..) )
import Kupo.Data.ChainSync
    ( Block
    , SlotNo (..)
    , addressToBytes
    , getDelegationPartBytes
    , getPaymentPartBytes
    )
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
    let (addresses, inputs) = unzip $ concatMap (matchBlock resultToRow patterns) blks
    let len = length inputs
    when (len > 0) $ do
        liftIO $ putStrLn $ "Inserting " <> show len <> " UTXO entries."
        let maxSlot = (\(Row fields) -> show (Prelude.last fields)) $ Prelude.last inputs
        liftIO $ putStrLn $ "Last known slot: " <> maxSlot
    -- TODO: TRANSACTION
    insertMany inputs
    insertMany addresses

--
-- SQL interface
--

resultToRow
    :: Result StandardCrypto
    -> (Row "addresses", Row "inputs")
resultToRow Result{..} =
    ( Row
        [ maybe (SQLBlob (addressToBytes address)) SQLBlob (getPaymentPartBytes address)
        , maybe SQLNull SQLBlob (getDelegationPartBytes address)
        ]
    , Row
        [ SQLBlob (serialize' reference)
        , SQLBlob (addressToBytes address)
        , SQLBlob (serialize' value)
        , maybe SQLNull (SQLBlob . serialize') datumHash
        , SQLInteger (fromIntegral (unSlotNo slotNo))
        ]
    )
