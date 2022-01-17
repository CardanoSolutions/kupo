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
    ( Handler (..), mkChainSyncClient )
import Kupo.Configuration
    ( Configuration (..), NetworkParameters (..) )
import Kupo.Control.MonadOuroboros
    ( MonadOuroboros (..), NodeToClientVersion (..) )
import Kupo.Control.MonadThrow
    ( MonadThrow )
import Kupo.Data.ChainSync
    ( pattern GenesisPoint )
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
            }
        } <- ask
    withChainSyncServer [ NodeToClientV_12 ] networkMagic slotsPerEpoch nodeSocket $
        mkChainSyncClient handlers points
 where
    -- TODO:
    -- - Get points from command-line and/or from database state
    -- -
    points = [GenesisPoint]
    handlers = Handler
        { onRollBackward = print
        , onRollForward = print
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
