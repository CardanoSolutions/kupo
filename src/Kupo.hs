--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Kupo
    ( -- * Commands
      runWith
    , version
    , healthCheck
    , copyDatabase

    -- * Kupo
    , Kupo (..)
    , kupo
    , kupoWith

    -- * Environment
    , Env (..)
    , newEnvironment

    -- * Command & Options
    , Command (..)
    , parseOptions

    -- * Tracers
    , Tracers(..)
    , TracersCopy(..)
    , Severity(..)
    , withTracers
    , defaultTracers
    ) where

import Kupo.Prelude

import Data.Pool
    ( defaultPoolConfig
    , destroyAllResources
    , newPool
    , tryWithResource
    , withResource
    )
import GHC.Conc
    ( getNumCapabilities
    )
import Kupo.App
    ( ChainSyncClient
    , TraceConsumer (..)
    , consumer
    , gardener
    , newProducer
    , withFetchBlockClient
    )
import Kupo.App.ChainSync
    ( withChainSyncExceptionHandler
    )
import Kupo.App.Configuration
    ( newPatternsCache
    , startOrResume
    )
import Kupo.App.Database
    ( ConnectionType (..)
    , Database (..)
    , copyDatabase
    , createShortLivedConnection
    , newDatabaseFile
    , newLock
    , withLongLivedConnection
    )
import Kupo.App.Health
    ( connectionStatusToggle
    , initializeHealth
    , readHealth
    , recordCheckpoint
    )
import Kupo.App.Http
    ( healthCheck
    , httpServer
    )
import Kupo.App.Mailbox
    ( Mailbox
    )
import Kupo.Control.MonadAsync
    ( concurrently4
    )
import Kupo.Control.MonadLog
    ( Severity (..)
    , TracerDefinition (..)
    , defaultTracers
    , withTracers
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Control.MonadThrow
    ( finally
    )
import Kupo.Data.Cardano
    ( IsBlock
    , Point
    , Tip
    )
import Kupo.Data.ChainSync
    ( ForcedRollbackHandler
    )
import Kupo.Data.Configuration
    ( Configuration (..)
    )
import Kupo.Data.FetchBlock
    ( FetchBlockClient
    )
import Kupo.Data.Health
    ( Health
    , emptyHealth
    )
import Kupo.Options
    ( Command (..)
    , Tracers (..)
    , TracersCopy (..)
    , parseOptions
    )
import Kupo.Version
    ( version
    )

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
kupo :: Tracers IO Concrete -> Kupo ()
kupo tr = do
    Env { configuration = Configuration
            { chainProducer
            }
        } <- ask
    kupoWith tr
        (newProducer (tracerConfiguration tr) chainProducer)
        (withFetchBlockClient chainProducer)

-- | Same as 'kupo', but allows specifying the chain producer component.
kupoWith
    :: Tracers IO Concrete
    -> ( ( forall block. IsBlock block
          => (Point -> ForcedRollbackHandler IO -> IO ())
          -> Mailbox IO (Tip, block) (Tip, Point)
          -> ChainSyncClient IO block
          -> IO ()
         )
         -> IO ()
       )
       -- ^ Chain producer acquisition bracket
    -> ( ( forall block. IsBlock block
          => FetchBlockClient IO block
          -> IO ()
         )
         -> IO ()
       )
       -- ^ FetchBlockClient acquisition bracket
    -> Kupo ()
kupoWith tr withProducer withFetchBlock =
  hijackSigTerm *> do
    Env { health
        , configuration = config@Configuration
            { serverHost
            , serverPort
            , workDir
            , inputManagement
            , longestRollback
            , deferIndexes
            }
        } <- ask

    (minConcurrency, maxConcurrency) <- liftIO getNumCapabilities <&> \n -> (n, 2 * n)

    dbFile <- newDatabaseFile (tracerDatabase tr) workDir

    lock <- liftIO newLock

    readOnlyPool <- liftIO $ newPool $ defaultPoolConfig
        (createShortLivedConnection (tracerDatabase tr) ReadOnly lock longestRollback dbFile)
        (\Database{close} -> close)
        30
        maxConcurrency

    readWritePool <- liftIO $ newPool $ defaultPoolConfig
        (createShortLivedConnection (tracerDatabase tr) ReadWrite lock longestRollback dbFile)
        (\Database{close} -> close)
        30
        minConcurrency

    let run action =
            withLongLivedConnection (tracerDatabase tr) lock longestRollback dbFile deferIndexes action
                `finally` do
                    destroyAllResources readOnlyPool
                    destroyAllResources readWritePool

    liftIO $ run $ \db -> do
        patterns <- newPatternsCache (tracerConfiguration tr) config db
        (mostRecentCheckpoint, checkpoints) <- startOrResume (tracerConfiguration tr) config db
        let notifyTip = recordCheckpoint health
        let statusToggle = connectionStatusToggle health
        let tracerChainSync =  contramap ConsumerChainSync . tracerConsumer
        withProducer $ \forceRollback mailbox producer -> do
            withFetchBlock $ \fetchBlock -> do
                concurrently4
                    -- HTTP Server
                    ( httpServer
                        (tracerHttp tr)
                        (\case
                            ReadOnly  -> tryWithResource readOnlyPool
                            ReadWrite -> tryWithResource readWritePool
                            WriteOnly -> const (fail "impossible: tried to acquire WriteOnly database?")
                        )
                        forceRollback
                        fetchBlock
                        patterns
                        (readHealth health)
                        serverHost
                        serverPort
                    )

                    -- Block consumer fueling the database
                    ( consumer
                        (tracerConsumer tr)
                        inputManagement
                        notifyTip
                        mailbox
                        patterns
                        db
                    )

                    -- Database garbage-collector
                    ( gardener
                        (tracerGardener tr)
                        config
                        patterns
                        (withResource readWritePool)
                    )

                    -- Block producer, fetching blocks from the network
                    ( withChainSyncExceptionHandler (tracerChainSync tr) statusToggle $ do
                        initializeHealth health mostRecentCheckpoint
                        producer
                            (tracerChainSync tr)
                            checkpoints
                            statusToggle
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
