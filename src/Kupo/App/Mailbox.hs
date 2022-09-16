--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- | A simple eDSL on top of TBQueue to have a concurrent producer / consumer
-- interface to decouple two processes. The main hypotheseses are:
--
-- - The producer produces message one-by-one
-- - The produces produces message a lot faster than the consumer can consome
--   then sequentially
-- - The consumer can however consume multiple messages at once.
module Kupo.App.Mailbox
    ( Mailbox
    , newMailbox
    , putHighFrequencyMessage
    , putIntermittentMessage
    , flushMailbox
    ) where

import Kupo.Prelude

import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )

data Mailbox m a b = Mailbox
    { highFrequencyMessages :: !(TBQueue m a)
    , intermittentMessages :: !(TMVar m b)
    }

newMailbox
    :: (MonadSTM m)
    => Natural
        -- ^ Size of the mailbox.
    -> STM m (Mailbox m a b)
newMailbox sz =
    Mailbox <$> newTBQueue sz <*> newEmptyTMVar
{-# INLINEABLE newMailbox #-}

putHighFrequencyMessage
    :: (MonadSTM m)
    => Mailbox m a b
    -> a
    -> STM m ()
putHighFrequencyMessage Mailbox{highFrequencyMessages, intermittentMessages} a = do
    check =<< isEmptyTMVar intermittentMessages
    writeTBQueue highFrequencyMessages a
{-# INLINEABLE putHighFrequencyMessage #-}

putIntermittentMessage
    :: (MonadSTM m)
    => Mailbox m a b
    -> b
    -> STM m ()
putIntermittentMessage Mailbox{intermittentMessages} =
    putTMVar intermittentMessages
{-# INLINEABLE putIntermittentMessage #-}

flushMailbox
    :: (MonadSTM m)
    => Mailbox m a b
    -> STM m (Either (NonEmpty a) b)
flushMailbox Mailbox{highFrequencyMessages, intermittentMessages} = do
    -- NOTE: This favor high frequency messages over intermittent messages so
    -- that intermitted messages are only consumed when there's no more high
    -- frequency messages. Plus, it isn't possible to enqueue new high frequency
    -- messages if there's a pending intermitted message. This ensures liveness
    -- of the intermittent messages as well as enforcing sequentiality between
    -- high-frequency and intermitted message so long as there's only a single
    -- producer of those messages.
    a_or_b <-
        (Left <$> readTBQueue highFrequencyMessages)
        `orElse`
        (Right <$> takeTMVar intermittentMessages)

    case a_or_b of
        Left a -> do
            rest <- flushTBQueue highFrequencyMessages
            pure (Left (a :| rest))
        Right b -> do
            pure (Right b)
{-# INLINEABLE flushMailbox #-}
