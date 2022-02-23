--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module Kupo.App
    ( startOrResume
    , producer
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
    ( Database (..), MonadDatabase (..) )
import Kupo.Control.MonadSTM
    ( MonadSTM (..) )
import Kupo.Data.ChainSync
    ( Block
    , Point
    , SlotNo (..)
    , addressToBytes
    , getDelegationPartBytes
    , getHeaderHash
    , getPaymentPartBytes
    , getSlotNo
    , unsafeMkPoint
    )
import Kupo.Data.Pattern
    ( Pattern, Result (..), matchBlock )

--
-- Application Bootstrapping
--

startOrResume
    :: forall m.
        ( MonadFail m
        , MonadIO m  -- TODO logger
        )
    => Database m
    -> Maybe (Point (Block StandardCrypto))
    -> m [Point (Block StandardCrypto)]
startOrResume Database{..} since = do
    checkpoints <- runTransaction (listCheckpointsDesc unsafeMkPoint)
    case (since, checkpoints) of
        (Nothing, []) ->
            -- TODO: Use logging instead.
            fail "No starting points provided and found no previous checkpoints!"
        (Just{}, _:_) ->
            -- TODO: Use logging instead.
            fail "--since provided but found existing database checkpoints!"
        (Nothing, pts) -> do
            -- TODO: Use logging instead.
            liftIO $ putStrLn $ "Found " <> show (length pts) <> " likely checkpoints."
            pure pts
        (Just pt, []) ->
            pure [pt]

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
    let lastKnownSlot = getSlotNo lastKnownBlk
    let (addresses, inputs) = unzip $ concatMap
            (matchBlock resultToRow patterns)
            blks
    liftIO $ putStrLn $ "Last known slot: " <> show lastKnownSlot
    runTransaction $ do
        insertCheckpoint (getHeaderHash lastKnownBlk) (unSlotNo lastKnownSlot)
        insertInputs inputs
        insertAddresses addresses

--
-- SQL interface
--

resultToRow
    :: Result StandardCrypto
    -> ( ( ByteString, Maybe ByteString )
       , ( ByteString, ByteString, ByteString, Maybe ByteString, Word64 )
       )
resultToRow Result{..} =
    ( ( fromMaybe (addressToBytes address) (getPaymentPartBytes address)
      , getDelegationPartBytes address
      )
    , ( serialize' reference
      , addressToBytes address
      , serialize' value
      , serialize' <$> datumHash
      , unSlotNo slotNo
      )
    )
