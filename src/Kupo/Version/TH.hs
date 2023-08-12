--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Version.TH where

import Kupo.Prelude

import Language.Haskell.TH
    ( Exp (..)
    , Lit (..)
    , Q
    , runIO
    )

-- | Dynamically lookup a git revision in the environment as `GIT_SHA` at compile-time to enhance
-- the version number.
gitRevisionTH :: Q Exp
gitRevisionTH = do
    LitE . StringL <$> runIO lookupGitSha
  where
    lookupGitSha :: IO String
    lookupGitSha = do
        lookupEnv "GIT_SHA" <&> \case
            Nothing ->
                ""
            Just sha ->
                "+" <> take 6 sha
