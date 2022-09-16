--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}

module Kupo.Data.Http.Status
    ( Status (statusCode, statusMessage)
    , mkStatus
    ) where

import Kupo.Prelude

import qualified Network.HTTP.Types.Status as Http

data Status = Status
    { statusCode :: !Int
    , statusMessage :: !Text
    } deriving stock (Generic)
      deriving anyclass (ToJSON)

mkStatus :: Http.Status -> Status
mkStatus status = Status
    { statusCode = Http.statusCode status
    , statusMessage = decodeUtf8 (Http.statusMessage status)
    }
