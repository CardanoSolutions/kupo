--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Kupo
    ( -- * Running
      Kupo (..)
    , kupo
    , runWith
    , version

    -- * Environment
    , Env (..)
    , newEnvironment

    -- * Command & Options
    , Command (..)
    , parseOptions
    ) where

import Kupo.Prelude

import Kupo.App
    ( consumer, producer )
import Kupo.App.ChainSync
    ( mkChainSyncClient )
import Kupo.App.Mailbox
    ( newMailbox )
import Kupo.Configuration
    ( Configuration (..), NetworkParameters (..) )
import Kupo.Control.MonadAsync
    ( MonadAsync (..) )
import Kupo.Control.MonadDatabase
    ( MonadDatabase (..) )
import Kupo.Control.MonadOuroboros
    ( MonadOuroboros (..), NodeToClientVersion (..) )
import Kupo.Control.MonadSTM
    ( MonadSTM (..) )
import Kupo.Data.ChainSync
    ( pattern GenesisPoint )
import Kupo.Options
    ( Command (..), parseOptions )
import Kupo.Version
    ( version )
import System.FilePath
    ( (</>) )

--
-- Environment
--

-- | Main application monad.
newtype Kupo a = Kupo
    { unKupo :: ReaderT (Env Kupo) IO a
    } deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader (Env Kupo)
        , MonadIO
        )

-- | Application entry point.
kupo :: Kupo ()
kupo = hijackSigTerm *> do
    Env{
        networkParameters = NetworkParameters
            { networkMagic
            , slotsPerEpoch
            }
        , configuration = Configuration
            { nodeSocket
            , workDir
            , since
            , patterns
            }
        } <- ask

    let chainSyncServer mailbox =
            withChainSyncServer [ NodeToClientV_12 ] networkMagic slotsPerEpoch nodeSocket $
            -- TODO: Instead of defaulting to origin, which is NOT a sensible
            -- default, we should:
            --
            -- 1. Get the most recent points from the database and resume the
            --    synchronization from there.
            --
            -- 2. If (1) yields nothing, then we should rely on --since
            --
            -- 3. If (1) yields something and --since is still provided, we
            -- should fail with an informative error message.
            let points = maybe [GenesisPoint] pure since
             in mkChainSyncClient (producer mailbox) points

     -- 43200 slots = k/f slots
     --
     -- TODO: k and f should be pulled from the protocol parameters (e.g. via
     -- local-state-query).
     --
     -- Otherwise, the current value may start to be insufficient if the values
     -- of `k` or `f` change in the future. In practice,
     --
     -- (a) there's very little chance that it will change significantly;
     -- (b) this kind of change are announced upfront days, if not weeks / months before
     --
     -- Arbitrarily larger values could be used to cope with future changes
     -- since the only impact is on the 'checkpoints' table in the database. We
     -- use this value to trim old checkpoints that are not needed for
     -- re-establishing an intersection. Since checkpoints are small, storing
     -- more is cheap.
    let longestRollback = 43200

    liftIO $ withDatabase longestRollback (workDir </> "kupo.sqlite3") $ \db -> do
        mailbox <- atomically (newMailbox 100)
        concurrently_
            (chainSyncServer mailbox)
            (consumer mailbox patterns db)



--
-- Environment
--

-- | Application runner with an instantiated environment. See 'newEnvironment'.
runWith :: forall a. Kupo a -> Env Kupo -> IO a
runWith app = runReaderT (unKupo app)

data Env (m :: Type -> Type) = Env
    { networkParameters :: !NetworkParameters
    , configuration :: !Configuration
    } deriving stock (Generic)

newEnvironment
    :: NetworkParameters
    -> Configuration
    -> IO (Env Kupo)
newEnvironment networkParameters configuration =
    pure Env{networkParameters, configuration}
