-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Kupo.Data.OgmiosSpec
    ( spec
    ) where

import Kupo.Prelude

import Data.List
    ( (!!)
    )
import Kupo.App.ChainSync.Ogmios
    ( intersectionNotFound
    )
import Kupo.Data.Ogmios
    ( decodeFindIntersectResponse
    , decodeRequestNextResponse
    )
import System.Directory
    ( listDirectory
    )
import System.FilePath
    ( (</>)
    )
import Test.Hspec
    ( Spec
    , SpecWith
    , context
    , parallel
    , runIO
    )
import Test.Hspec.QuickCheck
    ( modifyMaxSize
    , prop
    )
import Test.QuickCheck
    ( counterexample
    , forAll
    , sized
    , withMaxSuccess
    )
import Test.QuickCheck.Monadic
    ( assert
    , monadicIO
    , monitor
    , run
    )

import Data.Aeson as Json
import Data.Aeson.Types as Json

spec :: Spec
spec = parallel $ context "can decode relevant Ogmios' test vectors" $ do
    context "decodeFindIntersectResponse" $ do
        let dir = "./test/vectors/ogmios/server/test/vectors/ChainSync/Response/FindIntersect"
        vectors <- runIO (listDirectory dir)
        propVector ((dir </>) <$> vectors) (decodeFindIntersectResponse $ intersectionNotFound [])

    context "decodeRequestNextResponse" $ do
        let dir = "./test/vectors/ogmios/server/test/vectors/ChainSync/Response/RequestNext"
        vectors <- runIO (listDirectory dir)
        propVector ((dir </>) <$> vectors) decodeRequestNextResponse

propVector
    :: [FilePath]
    -> (Json.Value -> Json.Parser a)
    -> SpecWith ()
propVector vectors decoder = do
    modifyMaxSize (const (length vectors)) $ do
        prop "decode test vectors" $
            withMaxSuccess (length vectors) $
                forAll (sized $ \i -> pure (vectors !! i)) p
  where
    p vector = monadicIO $ do
        let errDecode = "Failed to decode JSON"
        value <- maybe (fail errDecode) pure =<< run (Json.decodeFileStrict vector)
        case Json.parse decoder value of
            Json.Error str -> do
                monitor $ counterexample (decodeUtf8 (Json.encode value))
                monitor $ counterexample str
                assert False
            Json.Success{} -> do
                assert True
