-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.Data.OgmiosSpec
    ( spec
    ) where

import Kupo.Prelude

import Kupo.Data.Ogmios
    ( decodeFindIntersectResponse, decodeRequestNextResponse )
import System.Directory
    ( listDirectory )
import System.FilePath
    ( (</>) )
import Test.Hspec
    ( Spec, SpecWith, context, expectationFailure, parallel, runIO, specify )

import Data.Aeson as Json
import Data.Aeson.Types as Json

spec :: Spec
spec = parallel $ context "can decode relevant Ogmios' test vectors" $ do
    context "decodeFindIntersectResponse" $ do
        let dir = "./test/ogmios/server/test/vectors/ChainSync/Response/FindIntersect"
        vectors <- runIO (listDirectory dir)
        mapM_ (specifyVector (decodeFindIntersectResponse []) . (dir </>)) vectors

    context "decodeRequestNextResponse" $ do
        let dir = "./test/ogmios/server/test/vectors/ChainSync/Response/RequestNext"
        vectors <- runIO (listDirectory dir)
        mapM_ (specifyVector decodeRequestNextResponse . (dir </>)) vectors

specifyVector
    :: (Json.Value -> Json.Parser a)
    -> FilePath
    -> SpecWith ()
specifyVector decoder vector = do
    specify vector $ do
        let errDecode = "Failed to decode JSON"
        value <- maybe (fail errDecode) pure =<< Json.decodeFileStrict vector
        case Json.parse decoder value of
            Json.Error str ->
                expectationFailure $ errDecode <> ": " <> str
            Json.Success{} ->
                pure ()
