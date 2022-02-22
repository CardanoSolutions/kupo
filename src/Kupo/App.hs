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
    ( Database (..), MonadDatabase (..), Row (..), SQLData (..) )
import Kupo.Control.MonadSTM
    ( MonadSTM (..) )
import Kupo.Data.ChainSync
    ( Block
    , SlotNo (..)
    , addressToBytes
    , getDelegationPartBytes
    , getHeaderHash
    , getPaymentPartBytes
    , getSlotNo
    )
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
        , Monad (DBTransaction m)
        )
    => Mailbox m (Block StandardCrypto)
    -> [Pattern StandardCrypto]
    -> Database m
    -> m ()
consumer mailbox patterns Database{..} = forever $ do
    blks <- atomically (flushMailbox mailbox)
    let lastKnownBlk = last blks
    let (addresses, inputs) = unzip $ concatMap
            (matchBlock resultToRow patterns)
            blks
    liftIO $ putStrLn $ "Last known slot: " <> show (getSlotNo lastKnownBlk)
    let rollbackFrontier = getRollbackFrontier lastKnownBlk
    runTransaction $ do
        insertMany [blockToRow lastKnownBlk]
        deleteWhere "slot_no < ?" (slotToRow rollbackFrontier)
        insertMany inputs
        insertMany addresses

 -- TODO: 43200 - k/f, we should pull k and f from the protocol params!
 -- Otherwise, this may start to be insufficient if the values of `k` or `f`
 -- would change in the future.
getRollbackFrontier
    :: Block StandardCrypto
    -> SlotNo
getRollbackFrontier blk =
    getSlotNo blk - 43200

--
-- SQL interface
--

blockToRow
    :: Block StandardCrypto
    -> Row "checkpoints"
blockToRow blk =
    let Row fields = slotToRow (getSlotNo blk)
     in Row (SQLBlob (getHeaderHash blk) : fields)

slotToRow
    :: SlotNo
    -> Row "checkpoints"
slotToRow sl =
    Row
        [ SQLInteger (fromIntegral (unSlotNo sl))
        ]

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
