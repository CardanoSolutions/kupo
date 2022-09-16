--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}

module Test.Kupo.App.MailboxSpec
    ( spec
    ) where

import Kupo.Prelude

import Control.Monad.IOSim
    ( pattern SimTrace
    , pattern TraceMainReturn
    , runSimTrace
    )
import Kupo.App.Mailbox
    ( Mailbox
    , flushMailbox
    , newMailbox
    , putHighFrequencyMessage
    , putIntermittentMessage
    )
import Kupo.Control.MonadAsync
    ( MonadAsync (..)
    )
import Kupo.Control.MonadDelay
    ( MonadDelay (..)
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Control.MonadTime
    ( DiffTime
    , Time (..)
    , secondsToDiffTime
    )
import Test.Hspec
    ( Spec
    , context
    , expectationFailure
    , parallel
    , shouldBe
    , shouldSatisfy
    , specify
    )
import Test.QuickCheck
    ( arbitrary
    , frequency
    , generate
    , vectorOf
    )

spec :: Spec
spec = parallel $ context "Mailbox" $ do
    specify "producer / consumer simulations" $ do
        let n = 10_000
        msgs <- generate $ vectorOf n $ frequency
            [ (1, Right <$> arbitrary)
            , (100, Left <$> arbitrary)
            ]
        let flatten = foldMap $ \case
                Left a -> [a]
                Right b -> [fromIntegral b]
        let simulation = runSimTrace $ do
                mailbox <- atomically (newMailbox mailboxCapacity)
                (msgs', ()) <- concurrently
                    (consumer mailbox n)
                    (producer mailbox msgs)
                pure msgs'
        let analyze = \case
                TraceMainReturn t msgs' _ -> do
                    -- NOTE: The diving factor is 'arbitrary' but it just shows
                    -- that it is much faster to do this work concurrently than
                    -- it would be to process all messages one-by-one. We show
                    -- that the concurrent setup is at least an order of
                    -- magnitude faster than the time it would take to process
                    -- messages fully sequentially.
                    let n' = secondsToDiffTime (toInteger n `div` 20)
                    let maxTime = n' * (producerWorkTime + consumerWorkTime)
                    t `shouldSatisfy` (<= Time maxTime)
                    msgs' `shouldBe` flatten msgs
                SimTrace _time _threadId _label _event step ->
                    analyze step
                e ->
                    expectationFailure ("Simulation failed: " <> show e)
        analyze simulation

-- Producer / Consumer
--
-- We model a producer consumer assuming that the consumer is 100x slower than
-- the consumer at his work, but, it can process multiple messages at once. On
-- his side, the producer is steadily producing messages at a fixed pace.

consumer :: (MonadDelay m, MonadSTM m) => Mailbox m Int Word -> Int -> m [Int]
consumer mailbox = \case
    0 -> pure []
    n -> do
        atomically (flushMailbox mailbox) >>= \case
            Left xs -> do
                threadDelay consumerWorkTime
                rest <- consumer mailbox (n - length xs)
                pure (toList xs ++ rest)
            Right i -> do
                threadDelay consumerWorkTime
                rest <- consumer mailbox (n - 1)
                pure (fromIntegral i : rest)

producer :: (MonadDelay m, MonadSTM m) => Mailbox m Int Word -> [Either Int Word] -> m ()
producer mailbox = \case
    []  -> pure ()
    f:r -> do
        case f of
            Left a ->
                atomically (putHighFrequencyMessage mailbox a)
            Right b ->
                atomically (putIntermittentMessage mailbox b)
        threadDelay producerWorkTime
        producer mailbox r

mailboxCapacity :: Natural
mailboxCapacity = 100

producerWorkTime :: DiffTime
producerWorkTime = 1

consumerWorkTime :: DiffTime
consumerWorkTime =
    secondsToDiffTime (toInteger mailboxCapacity) * producerWorkTime
