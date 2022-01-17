--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kupo
    ( -- * App
      App (..)
    , application
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

import Kupo.Options
    ( Command (..), parseOptions )
import Kupo.Version
    ( version )

--
-- Environment
--

-- | Main application monad.
newtype App a = App
    { unApp :: ReaderT (Env App) IO a
    } deriving newtype
        ( Functor, Applicative, Monad
        , MonadReader (Env App)
        , MonadIO
        )

-- | Application entry point.
application :: App ()
application = pure ()

-- | Application runner with an instantiated environment. See 'newEnvironment'.
runWith :: forall a. App a -> Env App -> IO a
runWith app = runReaderT (unApp app)

--
-- Environment
--

data Env (m :: Type -> Type) = Env
    deriving stock
        ( Generic
        )

newEnvironment :: IO (Env App)
newEnvironment = pure Env
