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

import Kupo.App.ChainSync
    ( ChainSyncHandler (..), mkChainSyncClient )
import Kupo.Configuration
    ( Configuration (..), NetworkParameters (..) )
import Kupo.Control.MonadDatabase
    ( Database (..), MonadDatabase (..), databaseFilePath, tableInputs )
import Kupo.Control.MonadOuroboros
    ( MonadOuroboros (..), NodeToClientVersion (..) )
import Kupo.Control.MonadThrow
    ( MonadThrow )
import Kupo.Data.ChainSync
    ( pattern GenesisPoint )
import Kupo.Data.Pattern
    ( matchBlock )
import Kupo.Options
    ( Command (..), parseOptions )
import Kupo.Version
    ( version )

--
-- Environment
--

-- | Main application monad.
newtype Kupo a = Kupo
    { unKupo :: ReaderT (Env Kupo) IO a
    } deriving newtype
        ( Functor, Applicative, Monad
        , MonadReader (Env Kupo)
        , MonadIO
        , MonadDatabase
        , MonadOuroboros
        , MonadThrow
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

    liftIO $ withDatabase (databaseFilePath workDir) $ \db ->
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
             in mkChainSyncClient (handlers patterns db) points
  where
    -- TODO: Move under app/
    handlers patterns db = ChainSyncHandler
        { onRollBackward = \pt -> do
            liftIO $ putStrLn $ "RollBack to " <> show pt <> "; doing nothing about it."
        , onRollForward = \blk -> do
            let matches = matchBlock patterns blk
            insertMany db tableInputs matches
        }

-- | Application runner with an instantiated environment. See 'newEnvironment'.
runWith :: forall a. Kupo a -> Env Kupo -> IO a
runWith app = runReaderT (unKupo app)

--
-- Environment
--

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
