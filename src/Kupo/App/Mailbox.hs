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
    , putMailbox
    , flushMailbox
    ) where

import Kupo.Prelude

import Kupo.Control.MonadSTM
    ( MonadSTM (..) )

type Mailbox m a = TBQueue m a

newMailbox
    :: (MonadSTM m)
    => Natural
        -- ^ Size of the mailbox.
    -> STM m (Mailbox m a)
newMailbox =
    newTBQueue
{-# INLINEABLE newMailbox #-}

putMailbox
    :: (MonadSTM m)
    => Mailbox m a
    -> a
    -> STM m ()
putMailbox =
    writeTBQueue
{-# INLINEABLE putMailbox #-}

flushMailbox
    :: (MonadSTM m)
    => Mailbox m a
    -> STM m (NonEmpty a)
flushMailbox q = do
    h <- readTBQueue q
    rest <- flushTBQueue q
    pure (h :| rest)
{-# INLINEABLE flushMailbox #-}
