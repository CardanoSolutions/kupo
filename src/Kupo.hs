--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

    -- * Tracers
    , withStdoutTracers
    ) where

import Kupo.Prelude

import Kupo.App
    ( Tracers, Tracers' (..), consumer, producer, startOrResume )
import Kupo.App.ChainSync
    ( IntersectionNotFoundException (..)
    , TraceChainSync (..)
    , mkChainSyncClient
    )
import Kupo.App.Http
    ( runServer )
import Kupo.App.Mailbox
    ( newMailbox )
import Kupo.Configuration
    ( Configuration (..)
    , NetworkParameters (..)
    , TraceConfiguration (..)
    , WorkDir (..)
    )
import Kupo.Control.MonadAsync
    ( concurrently3 )
import Kupo.Control.MonadCatch
    ( MonadCatch (..) )
import Kupo.Control.MonadDatabase
    ( Mode (..), MonadDatabase (..) )
import Kupo.Control.MonadLog
    ( MonadLog (..), nullTracer, withStdoutTracers )
import Kupo.Control.MonadOuroboros
    ( MonadOuroboros (..), NodeToClientVersion (..) )
import Kupo.Control.MonadSTM
    ( MonadSTM (..) )
import Kupo.Options
    ( Command (..), parseOptions )
import Kupo.Version
    ( version )
import System.Exit
    ( ExitCode (..) )
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
kupo :: Tracers -> Kupo ()
kupo tr@Tracers{tracerChainSync, tracerHttp, tracerDatabase} = hijackSigTerm *> do
    Env { network = NetworkParameters
            { networkMagic
            , slotsPerEpoch
            }
        , configuration = Configuration
            { nodeSocket
            , serverHost
            , serverPort
            , workDir
            , since
            , patterns
            }
        } <- ask

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
    let dbFile = case workDir of
            Dir dir  -> dir </> "kupo.sqlite3"
            InMemory -> ":memory:"

    lock <- liftIO newLock
    liftIO $ withDatabase tracerDatabase Write lock longestRollback dbFile $ \db -> do
        checkpoints <- startOrResume tr db since
        -- TODO: Make the mailbox's size configurable? Larger sizes means larger
        -- memory usage at the benefits of slightly faster sync time... Though I
        -- am not sure it's worth enough to bother users with this. Between 100
        -- and 1000, the heap increases from ~150MB to ~1GB, for a 5-10%
        -- synchronization time decrease.
        mailbox <- atomically (newMailbox 100)
        concurrently3
            -- HTTP Server
            ( runServer
                tracerHttp
                (withDatabase nullTracer ReadOnly lock longestRollback dbFile)
                serverHost
                serverPort
            )

            -- Chain-Sync handler fueling the database
            ( consumer tr mailbox patterns db )

            -- Chain-Sync client, fetching blocks from the network
            ( let client = mkChainSyncClient (producer tr mailbox db) checkpoints
              in withChainSyncServer [ NodeToClientV_9 .. NodeToClientV_12 ]
                    networkMagic
                    slotsPerEpoch
                    nodeSocket
                    client
                & handle (\IntersectionNotFoundException{points} -> do
                    logWith tracerChainSync ChainSyncIntersectionNotFound{points}
                    exitWith (ExitFailure 1)
                  )
            )

--
-- Environment
--

-- | Application runner with an instantiated environment. See 'newEnvironment'.
runWith :: forall a. Kupo a -> Env Kupo -> IO a
runWith app = runReaderT (unKupo app)

data Env (m :: Type -> Type) = Env
    { network :: !NetworkParameters
    , configuration :: !Configuration
    } deriving stock (Generic)

newEnvironment
    :: Tracers
    -> NetworkParameters
    -> Configuration
    -> IO (Env Kupo)
newEnvironment Tracers{tracerConfiguration} network configuration = do
    logWith tracerConfiguration (ConfigurationNetwork network)
    pure Env{network, configuration}
