--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.Http.Error where

import Kupo.Prelude

import Data.Aeson
    ( (.=)
    )
import Kupo.Data.Cardano.Point
    ( Point
    , pointToJson
    , pointToText
    )
import Kupo.Data.Http.Response
    ( responseJson
    )
import Network.HTTP.Types.Status
    ( status400
    , status404
    , status406
    , status500
    , status503
    )
import Network.Wai
    ( Response
    )

import qualified Data.Aeson as Json
import qualified Data.Text as T
import qualified Kupo.Data.Http.Default as Default

data HttpError = HttpError
    { hint :: !Text
    , details :: !(Maybe Json.Value)
    }
    deriving stock (Generic)

instance ToJSON HttpError where
    toEncoding = Json.genericToEncoding options
      where
        options = Json.defaultOptions { Json.omitNothingFields = True }
    toJSON = Json.genericToJSON options
      where
        options = Json.defaultOptions { Json.omitNothingFields = True }

invalidPattern :: Response
invalidPattern =
    responseJson status400 Default.headers $ HttpError
        { hint = "Invalid pattern! To fetch matches or add a pattern, you must provide \
                 \a valid pattern, including wildcards ('*') or full addresses. \
                 \Make sure to double-check the documentation at: \
                 \<https://cardanosolutions.github.io/kupo#section/Patterns>!"
        , details = Nothing
        }

invalidQueryPattern :: Response
invalidQueryPattern =
    responseJson status400 Default.headers $ HttpError
        { hint = "Invalid query pattern! It isn't possible to query results via this pattern. \
                 \This is an index-only pattern. You may want to fetch all results in a paginated \
                 \fashion instead; or use another pattern for filtering. If you do strongly believe \
                 \that it should be possible to query results using this pattern, please open a \
                 \Github discussion: <https://github.com/CardanoSolutions/kupo/discussions/new?category=ideas> \
                 \or contribute to an existing one regarding this matter."
        , details = Nothing
        }

invalidPatterns :: [Text] -> Response
invalidPatterns validPatterns =
    responseJson status400 Default.headers $ HttpError
        { hint = "Found an invalid pattern in the provided list! Here's a list of all \
                 \the *valid* patterns successfully recognised in your request: "
                 <> T.intercalate ", " validPatterns
                 <> ". Any doubts? Check the documentation at: \
                 \<https://cardanosolutions.github.io/kupo#section/Patterns>!"
        , details = Nothing
        }

invalidStatusFlag :: Response
invalidStatusFlag =
    responseJson status400 Default.headers $ HttpError
        { hint = "Invalid filter query! Matches can be filtered by status using \
                 \HTTP query flags. Provide either '?spent' or '?unspent' to \
                 \filter accordingly. Anything else is an error. In case of \
                 \doubts, check the documentation at: <https://cardanosolutions.github.io/kupo#operation/getAllMatches>!"
        , details = Nothing
        }

invalidSlotRange :: Response
invalidSlotRange =
    responseJson status400 Default.headers $ HttpError
        { hint = "Unprocessable slot range! Slot ranges can be specified in the form of \
                 \lower and upper bound, in absolute slots. Either bound is optional and \
                 \you can only provide each bound once. Anything else is an error. In case \
                 \of doubts, check the documentation at: <https://cardanosolutions.github.io/kupo#operation/getAllMatches>!"
        , details = Nothing
        }

invalidMatchFilter :: Response
invalidMatchFilter =
    responseJson status400 Default.headers $ HttpError
        { hint = "Invalid or incomplete filter query parameters! 'policy_id' and \
                 \'asset_name' query values must be encoded in base16. Be aware \
                 \that you MUST specify a 'policy_id' if you specify an 'asset_name'. \
                 \In case of doubts, check the documentation at: <https://cardanosolutions.github.io/kupo>!"
        , details = Nothing
        }

invalidMetadataFilter :: Response
invalidMetadataFilter =
    responseJson status400 Default.headers $ HttpError
        { hint = "Invalid or incomplete filter query parameters! You can (optionally) filter \
                 \metadata by 'transaction_id', which must be a valid text string encoded in base16. \
                 \In case of doubts, check the documentation at: <https://cardanosolutions.github.io/kupo#operation/getMetadataBySlot>!"
        , details = Nothing
        }

stillActivePattern :: Response
stillActivePattern =
    responseJson status400 Default.headers $ HttpError
        { hint = "Beware! You've just attempted to remove matches using a pattern \
                 \that overlaps with another still active pattern! This is not \
                 \allowed as it could lead to very confusing index states. \
                 \Make sure to remove conflicting patterns first AND THEN, \
                 \clean-up obsolete matches if necessary."
        , details = Nothing
        }

invalidStrictMode :: Response
invalidStrictMode =
    responseJson status400 Default.headers $ HttpError
        { hint = "Invalid strict-mode query flag! This endpoint only accepts a \
                 \single query flag '?strict' which semantics is defined in the \
                 \documentation: <https://cardanosolutions.github.io/kupo>. \
                 \Any other query parameter is treated as an error."
        , details = Nothing
        }

invalidSlotNo :: Response
invalidSlotNo =
    responseJson status400 Default.headers $ HttpError
        { hint = "The path parameter for the endpoint must be an absolute slot \
                 \number. That is, an non-negative integer."
        , details = Nothing
        }

noAncestor :: Response
noAncestor =
    responseJson status400 Default.headers $ HttpError
        { hint = "There's no known-ancestor to the point you've provided! \
                 \No worries, if you're querying a very recent point, it may \
                \just be the case that your request arrived in between a \
                \rollback. Note also that there's (obviously) no ancesto to \
                \the slot number `0`."
        , details = Nothing
        }

malformedDatumHash :: Response
malformedDatumHash =
    responseJson status400 Default.headers $ HttpError
        { hint = "The given path parameter isn't a well-formed datum hash digest. \
                 \This must be a blake2b-256 hash digest encoded in base16 (\
                 \thus, 64 characters once encoded)."
        , details = Nothing
        }

malformedScriptHash :: Response
malformedScriptHash =
    responseJson status400 Default.headers $ HttpError
        { hint = "The given path parameter isn't a well-formed script hash digest. \
                 \This must be a blake2b-224 hash digest encoded in base16 (\
                 \thus, 56 characters once encoded)."
        , details = Nothing
        }

malformedPoint :: Response
malformedPoint =
    responseJson status400 Default.headers $ HttpError
        { hint = "Couldn't decode the provided point (or slot); Either it is \
                 \missing from the request body or it is not well-formed. You \
                 \can provide either a slot, or a slot and a header hash (a.k.a \
                 \a point). Please refer to the API reference for details <https://cardanosolutions.github.io/kupo>."
        , details = Nothing
        }

invalidSortDirection :: Response
invalidSortDirection =
    responseJson status400 Default.headers $ HttpError
        { hint = "Invalid sort direction provided as query parameter. \
                 \You can specify either 'order=most_recent_first' or \
                 \'order=oldest_first'. Please refer to the API reference for details <https://cardanosolutions.github.io/kupo#operation/getAllMatches>."
        , details = Nothing
        }

unsafeRollbackBeyondSafeZone :: Response
unsafeRollbackBeyondSafeZone =
    responseJson status400 Default.headers $ HttpError
        { hint = "Beware! The point you have provided is beyond the safe zone. \
                 \It means that while the server is synchronizing, the content \
                 \of the database may not fully reflect the state of the chain \
                 \at that point in time. This is only a transient state and the \
                 \server will eventually recover the latest state. You can go \
                 \as far as 36h in the past without issue (the safe zone), but \
                 \beyond this limit, the server can't guarantee data integrity \
                 \during syncing. If you still want to proceed, pass 'unsafe_allow_beyond_safe_zone' \
                 \as 'limit' to the request. See also the API reference for more \
                 \details about the safe zone: <https://cardanosolutions.github.io/kupo#putPattern1Ary>."
        , details = Nothing
        }

pointMismatch :: Point -> Point -> Response
pointMismatch requested ancestor =
    responseJson status400 Default.headers $ HttpError
        { hint = "There's no point corresponding to " <> pointToText requested <> ". \
                 \This probably means that a different chain fork that doesn't \
                 \include this point was adopted (i.e. our local copy of the chain \
                 \rolled back). The field 'details.ancestor' contains the closest \
                 \ancestor to the point you provided. Should you have been fetching \
                 \some data collection using slot ranges, you likely want to fetch \
                 \that collection from the beginning."
        , details = Just $ Json.object
            [ "ancestor" .= encodingToValue (pointToJson ancestor)
            ]
        }

nonExistingPoint :: Response
nonExistingPoint =
    responseJson status400 Default.headers $ HttpError
        { hint = "The provided point (or slot) is unknown. Please provide an \
                 \existing point from the past. Note that if you provide a very \
                 \recent point (e.g. 1-3 blocks old), it can happen that the \
                 \point disappear should a rollback happen between the moment \
                 \you looked it up and the moment you make this query."
        , details = Nothing
        }

failedToRollback :: Response
failedToRollback =
    responseJson status500 Default.headers $ HttpError
        { hint = "Whoops, the server was unable to rollback to the given point. \
                 \This can happen in rare cases (especially on recent points) \
                 \when the target point 'disappeared' from the chain while \
                 \trying to rollback to it. This is a transient error, retrying \
                 \should work. In the meantime, the server has restarted syncing \
                 \back where it was and your request has had no effect."
        , details = Nothing
        }

notFound :: Response
notFound =
    responseJson status404 Default.headers $ HttpError
        { hint = "Endpoint not found. Make sure to double-check the \
                 \documentation at: <https://cardanosolutions.github.io/kupo>!"
        , details = Nothing
        }

methodNotAllowed :: Response
methodNotAllowed =
    responseJson status406 Default.headers $ HttpError
        { hint = "Unsupported method called on known endpoint. Make sure to \
                 \double-check the documentation at: \
                 \<https://cardanosolutions.github.io/kupo>!"
        , details = Nothing
        }

unsupportedContentType :: [Text] -> Response
unsupportedContentType types =
    responseJson status400 Default.headers $ HttpError
        { hint = "Unsupported content-type requested in 'Accept' header. \
                 \This endpoint only understands: " <> T.intercalate " or " types <> "."
        , details = Nothing
        }

serviceUnavailable :: Response
serviceUnavailable =
    responseJson status503 Default.headers $ HttpError
        { hint = "The server is (too) busy! There's no resource available to handle this request. \
                 \While the requested *could be* enqueued for later, this error is meant to give \
                 \that choice to you, the client, to chose whether to retry it now or prioritize \
                 \some other more important query. Depending on your hardware resources, you may \
                 \also want to increase the server's maximum internal connections using \
                 \'--max-concurrency'."
        , details = Nothing
        }

serverError :: Response
serverError =
    responseJson status500 Default.headers $ HttpError
        { hint = "Unexpected server error. See server logs for details."
        , details = Nothing
        }
