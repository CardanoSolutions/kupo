--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}

module Kupo.Data.Http.Error where

import Kupo.Prelude

import Kupo.Data.Http.Response
    ( responseJson )
import Network.HTTP.Types.Status
    ( status400, status404, status406 )
import Network.Wai
    ( Response )

import qualified Kupo.Data.Http.Default as Default

data HttpError = HttpError
    { hint :: Text }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

invalidPattern :: Response
invalidPattern =
    responseJson status400 Default.headers $ HttpError
        { hint = "Invalid pattern! To fetch matches, you may provide any valid \
                 \pattern, including wildcards ('*') or full addresses. Make \
                 \sure to double-check the documentation at: \
                 \<https://cardanosolutions.github.io/kupo>!"
        }

invalidFilterQuery :: Response
invalidFilterQuery =
    responseJson status400 Default.headers $ HttpError
        { hint = "Invalid filter query! Matches can be filtered by status using \
                 \HTTP query flags. Provide either '?spent' or '?unspent' to \
                 \filter accordingly. Anything else is an error. In case of \
                 \doubts, check the documentation at: <https://cardanosolutions.github.io/kupo>!"
        }

stillActivePattern :: Response
stillActivePattern =
    responseJson status400 Default.headers $ HttpError
        { hint = "Beware! You've just attempted to remove matches using a pattern \
                 \that overlaps with another still active pattern! This is not \
                 \allowed as it could lead to very confusing index states. \
                 \Make sure to remove conflicting patterns first AND THEN, \
                 \clean-up obsolete matches if necessary."
        }

notFound :: Response
notFound =
    responseJson status404 Default.headers $ HttpError
        { hint = "Endpoint not found. Make sure to double-check the \
                 \documentation at: <https://cardanosolutions.github.io/kupo>!"
        }

methodNotAllowed :: Response
methodNotAllowed =
    responseJson status406 Default.headers $ HttpError
        { hint = "Unsupported method called on known endpoint. Make sure to \
                 \double-check the documentation at: \
                 \<https://cardanosolutions.github.io/kupo>!"
        }
