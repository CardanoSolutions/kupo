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
    , healthCheck

    -- * Environment
    , Env (..)
    , newEnvironment

    -- * Command & Options
    , Command (..)
    , parseOptions

    -- * Tracers
    , withTracers
    ) where

import Kupo.Prelude

import Kupo.App
    ( Tracers
    , Tracers' (..)
    , consumer
    , newPatternsCache
    , startOrResume
    , withChainProducer
    )
import Kupo.App.ChainSync
    ( withChainSyncExceptionHandler )
import Kupo.App.Health
    ( connectionStatusToggle, readHealth, recordCheckpoint )
import Kupo.App.Http
    ( healthCheck, httpServer )
import Kupo.Configuration
    ( Configuration (..), WorkDir (..) )
import Kupo.Control.MonadAsync
    ( concurrently3 )
import Kupo.Control.MonadDatabase
    ( ConnectionType (..), MonadDatabase (..) )
import Kupo.Control.MonadLog
    ( nullTracer, withTracers )
import Kupo.Control.MonadSTM
    ( MonadSTM (..) )
import Kupo.Data.Health
    ( Health, emptyHealth )
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
kupo :: Tracers -> Kupo ()
kupo tr@Tracers{tracerChainSync, tracerConfiguration, tracerHttp, tracerDatabase} =
  hijackSigTerm *> do
    Env { health
        , configuration = cfg@Configuration
            { serverHost
            , serverPort
            , chainProducer
            , workDir
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
    liftIO $ withDatabase tracerDatabase LongLived lock longestRollback dbFile $ \db -> do
        patterns <- newPatternsCache tr cfg db
        let notifyTip = recordCheckpoint health
        let statusToggle = connectionStatusToggle health
        withChainProducer tracerConfiguration chainProducer $ \mailbox producer -> do
            concurrently3
                -- HTTP Server
                ( httpServer
                    tracerHttp
                    -- NOTE: This should / could probably use a resource pool to
                    -- avoid re-creating a new connection on every requests. This is
                    -- however pretty cheap with SQLite anyway and the HTTP server
                    -- isn't meant to be a public-facing web server serving millions
                    -- of clients.
                    (withDatabase nullTracer ShortLived lock longestRollback dbFile)
                    patterns
                    (readHealth health)
                    serverHost
                    serverPort
                )

                -- Block consumer fueling the database
                ( consumer
                    tracerChainSync
                    notifyTip
                    mailbox
                    patterns
                    db
                )

                -- Block producer, fetching blocks from the network
                ( withChainSyncExceptionHandler tracerChainSync statusToggle $ do
                    checkpoints <- startOrResume tr cfg db
                    producer
                        tracerChainSync
                        checkpoints
                        notifyTip
                        statusToggle
                        db
                )

--
-- Environment
--

-- | Application runner with an instantiated environment. See 'newEnvironment'.
runWith :: forall a. Kupo a -> Env Kupo -> IO a
runWith app = runReaderT (unKupo app)

data Env (m :: Type -> Type) = Env
    { configuration :: !Configuration
    , health :: !(TVar IO Health)
    } deriving stock (Generic)

newEnvironment
    :: Configuration
    -> IO (Env Kupo)
newEnvironment configuration = do
    health <- newTVarIO emptyHealth
    pure Env{configuration, health}
