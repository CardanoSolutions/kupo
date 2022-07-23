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

invalidStatusFlag :: Response
invalidStatusFlag =
    responseJson status400 Default.headers $ HttpError
        { hint = "Invalid filter query! Matches can be filtered by status using \
                 \HTTP query flags. Provide either '?spent' or '?unspent' to \
                 \filter accordingly. Anything else is an error. In case of \
                 \doubts, check the documentation at: <https://cardanosolutions.github.io/kupo>!"
        }

invalidMatchFilter :: Response
invalidMatchFilter =
    responseJson status400 Default.headers $ HttpError
        { hint = "Invalid or incomplete filter query parameters! 'policy_id' and \
                 \'asset_name' query values must be encoded in base16. Be aware \
                 \that you MUST specify a 'policy_id' if you specify an 'asset_name'. \
                 \In case of doubts, check the documentation at: <https://cardanosolutions.github.io/kupo>!"
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

invalidStrictMode :: Response
invalidStrictMode =
    responseJson status400 Default.headers $ HttpError
        { hint = "Invalid strict-mode query flag! This endpoint only accepts a \
                 \single query flag '?strict' which semantics is defined in the \
                 \documentation: <https://cardanosolutions.github.io/kupo>. \
                 \Any other query parameter is treated as an error."
        }

invalidSlotNo :: Response
invalidSlotNo =
    responseJson status400 Default.headers $ HttpError
        { hint = "The path parameter for the endpoint must be an absolute slot \
                 \number. That is, an non-negative integer."
        }

malformedDatumHash :: Response
malformedDatumHash =
    responseJson status400 Default.headers $ HttpError
        { hint = "The given path parameter isn't a well-formed datum hash digest. \
                 \This must be a blake2b-256 hash digest encoded in base16 (\
                 \thus, 64 characters once encoded)."
        }

malformedScriptHash :: Response
malformedScriptHash =
    responseJson status400 Default.headers $ HttpError
        { hint = "The given path parameter isn't a well-formed script hash digest. \
                 \This must be a blake2b-224 hash digest encoded in base16 (\
                 \thus, 56 characters once encoded)."
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
