--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}

module Kupo.App.MailboxSpec
    ( spec
    ) where

import Kupo.Prelude

import Control.Monad.IOSim
    ( pattern SimTrace, pattern TraceMainReturn, runSimTrace )
import Kupo.App.Mailbox
    ( Mailbox, flushMailbox, newMailbox, putMailbox )
import Kupo.Control.MonadAsync
    ( MonadAsync (..) )
import Kupo.Control.MonadDelay
    ( MonadDelay (..) )
import Kupo.Control.MonadSTM
    ( MonadSTM (..) )
import Kupo.Control.MonadTime
    ( DiffTime, Time (..), secondsToDiffTime )
import Test.Hspec
    ( Spec, context, expectationFailure, parallel, shouldSatisfy, specify )
import Test.QuickCheck
    ( generate, vector )

spec :: Spec
spec = parallel $ context "Mailbox" $ do
    specify "producer / consumer simulations" $ do
        let n = 10_000
        msgs <- generate (vector n)
        let simulation = runSimTrace $ do
                mailbox <- atomically (newMailbox mailboxCapacity)
                concurrently (consumer mailbox n) (producer mailbox msgs)
        let analyze = \case
                TraceMainReturn t _ _ -> do
                    let n' = secondsToDiffTime (toInteger n)
                    let maxTime = n' * producerWorkTime + consumerWorkTime
                    t `shouldSatisfy` (<= Time maxTime)
                SimTrace _time _threadId _label _event next ->
                    analyze next
                _ ->
                    expectationFailure "Simulation failed."
        analyze simulation

-- Producer / Consumer
--
-- We model a producer consumer assuming that the consumer is 100x slower than
-- the consumer at his work, but, it can process multiple messages at once. On
-- his side, the producer is steadily producing messages at a fixed pace.

consumer :: (MonadDelay m, MonadSTM m) => Mailbox m Int -> Int -> m ()
consumer mailbox = \case
    0 -> pure ()
    n -> do
        xs <- atomically (flushMailbox mailbox)
        threadDelay consumerWorkTime
        consumer mailbox (n - length xs)

producer :: (MonadDelay m, MonadSTM m) => Mailbox m Int -> [Int] -> m ()
producer mailbox = \case
    []  -> pure ()
    f:r -> do
        atomically (putMailbox mailbox f)
        threadDelay producerWorkTime
        producer mailbox r

mailboxCapacity :: Natural
mailboxCapacity = 100

producerWorkTime :: DiffTime
producerWorkTime = 1

consumerWorkTime :: DiffTime
consumerWorkTime =
    secondsToDiffTime (toInteger mailboxCapacity) * producerWorkTime
