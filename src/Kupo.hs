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
    , newEnvironmentWith

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

import Control.Exception.Safe
    ( isAsyncException
    )
import Kupo.App
    ( ChainSyncClient
    , TraceConsumer (..)
    , TraceKupo (..)
    , consumer
    , gardener
    , idle
    , mkNotifyTip
    , newFetchTipClient
    , newProducer
    , readOnlyConsumer
    , withFetchBlockClient
    )
import Kupo.App.ChainSync
    ( withChainSyncExceptionHandler
    )
import Kupo.App.Configuration
    ( TraceConfiguration (..)
    , newPatternsCache
    , startOrResume
    )
import Kupo.App.Database
    ( copyDatabase
    , newDBPool
    )
import Kupo.App.Database.Types
    ( ConnectionType (..)
    , DBPool (..)
    )
import Kupo.App.Health
    ( connectionStatusToggle
    , initializeHealth
    , readHealth
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
import Kupo.Control.MonadCatch
    ( handle
    )
import Kupo.Control.MonadLog
    ( Severity (..)
    , TracerDefinition (..)
    , defaultTracers
    , logWith
    , traceWith
    , withTracers
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Control.MonadThrow
    ( finally
    , throwIO
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
    , DeferIndexesInstallation (..)
    , NodeTipHasBeenReachedException (..)
    , isReadOnlyReplica
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
import System.Exit
    ( ExitCode (..)
    )

import qualified Kupo.Data.Health as Health

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
        , crashWith
        , configuration = config@Configuration
            { serverHost
            , serverPort
            , databaseLocation
            , inputManagement
            , longestRollback
            , deferIndexes
            , chainProducer
            }
        } <- ask

    dbPool@DBPool { maxConcurrentReaders, maxConcurrentWriters } <- liftIO $ newDBPool
        (tracerDatabase tr)
        (isReadOnlyReplica config)
        databaseLocation
        longestRollback

    liftIO $ logWith (tracerConfiguration tr) $
      ConfigurationMaxConcurrency
          { maxConcurrentReaders
          , maxConcurrentWriters
          }

    let run action
            | isReadOnlyReplica config =
                -- NOTE: 'ShortLived' is a bad name here. What it really means is 'occasional
                -- writers but mostly readers'. However, in the 'ReadOnlyReplica' mode we only
                -- ever allow read-only connections and never perform a single write.
                (withDatabaseBlocking dbPool) ReadOnly (action InstallIndexesIfNotExist)
            | otherwise =
                handle
                    (\NodeTipHasBeenReached{distance} -> do
                        traceWith (tracerKupo tr) KupoRestartingWithIndexes { distance }
                        io InstallIndexesIfNotExist
                    )
                    (io deferIndexes)
              where
                io indexMode =
                  (withDatabaseExclusiveWriter dbPool) indexMode (action indexMode)
                    `finally` destroyResources dbPool

    liftIO $ handle (onUnknownException crashWith) $ run $ \indexMode db -> do
        patterns <- newPatternsCache (tracerConfiguration tr) config db
        let statusToggle = connectionStatusToggle health
        let tracerChainSync =  contramap ConsumerChainSync . tracerConsumer
        unless (isReadOnlyReplica config) $ do
            atomically $ modifyTVar' health $ \h -> h { Health.configuration = Just indexMode }
        withProducer $ \forceRollback mailbox producer -> do
            withFetchBlock $ \fetchBlock -> do
                concurrently4
                    -- HTTP Server
                    ( httpServer
                        (tracerHttp tr)
                        (tryWithDatabase dbPool)
                        forceRollback
                        fetchBlock
                        patterns
                        (readHealth health)
                        serverHost
                        serverPort
                    )

                    -- Block consumer fueling the database
                    ( if isReadOnlyReplica config then
                        readOnlyConsumer
                            health
                            db
                      else
                        consumer
                            (tracerConsumer tr)
                            inputManagement
                            (mkNotifyTip indexMode health)
                            mailbox
                            patterns
                            db
                    )

                    -- Database garbage-collector
                    ( if isReadOnlyReplica config then
                        idle
                      else
                        gardener
                            (tracerGardener tr)
                            config
                            patterns
                            (withDatabaseBlocking dbPool ReadWrite)
                    )

                    -- Block producer, fetching blocks from the network
                    ( if isReadOnlyReplica config then
                        toggleConnected statusToggle *> idle
                      else
                        withChainSyncExceptionHandler (tracerChainSync tr) statusToggle $ do
                            (mostRecentCheckpoint, checkpoints) <- startOrResume
                                (tracerConfiguration tr)
                                config
                                db
                                (newFetchTipClient chainProducer)

                            initializeHealth health mostRecentCheckpoint

                            producer
                                (tracerChainSync tr)
                                checkpoints
                                statusToggle
                    )

  where
    onUnknownException :: (SomeException -> IO ()) -> SomeException -> IO ()
    onUnknownException crashWith e
        | isAsyncException e = do
            throwIO e
        | otherwise = do
            logWith (tracerKupo tr) $ KupoUnexpectedError (toText (displayException e))
            crashWith e

--
-- Environment
--

-- | Application runner with an instantiated environment. See 'newEnvironment'.
runWith :: forall a. Kupo a -> Env Kupo -> IO a
runWith app = runReaderT (unKupo app)

data Env (m :: Type -> Type) = Env
    { crashWith :: SomeException -> IO ()
    , configuration :: !Configuration
    , health :: !(TVar IO Health)
    } deriving stock (Generic)

newEnvironment
    :: Configuration
    -> IO (Env Kupo)
newEnvironment =
    newEnvironmentWith (const (exitWith (ExitFailure 1)))

newEnvironmentWith
    :: (SomeException -> IO ())
    -> Configuration
    -> IO (Env Kupo)
newEnvironmentWith crashWith configuration = do
    health <- newTVarIO emptyHealth
    pure Env{configuration, health, crashWith}
