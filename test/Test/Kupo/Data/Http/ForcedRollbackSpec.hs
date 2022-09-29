-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.Data.Http.ForcedRollbackSpec
    ( spec
    ) where

import Kupo.Prelude

import Kupo.Data.Http.ForcedRollback
    ( decodeForcedRollback
    , forcedRollbackToJson
    )
import Test.Hspec
    ( Spec
    , context
    , parallel
    )
import Test.Hspec.QuickCheck
    ( prop
    )
import Test.Kupo.Data.Generators
    ( genForcedRollback
    )
import Test.QuickCheck
    ( Property
    , counterexample
    , forAll
    , (===)
    )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Types as Json

spec :: Spec
spec = parallel $ do
    context "ForcedRollback" $ do
        prop "JSON roundtrip" $
            forAll genForcedRollback
                (roundtripJSON forcedRollbackToJson decodeForcedRollback)

roundtripJSON
    :: (Show a, Eq a)
    => (a -> Json.Encoding)
    -> (Json.Value -> Json.Parser a)
    -> a
    -> Property
roundtripJSON encode decode x = do
    let bytes = Json.encodingToLazyByteString (encode x)
    case eitherDecodeJson decode bytes of
        Right x' ->
            (x === x') & counterexample (decodeUtf8 $ toStrict bytes)
        Left e ->
            False & counterexample e
