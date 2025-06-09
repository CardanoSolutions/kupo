-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Kupo.Data.HydraSpec
    ( spec
    ) where

import Kupo.Prelude

import Data.Aeson.Lens
    ( _Array
    , key
    )
import Kupo.Data.Hydra
    ( decodeHydraMessage
    )
import System.Directory
    ( listDirectory
    )
import Test.Hspec
    ( Spec
    , context
    , parallel
    , runIO
    )
import Test.Hspec.QuickCheck
    ( prop
    )
import Test.QuickCheck
    ( Property
    , conjoin
    , counterexample
    , withMaxSuccess
    )
import Test.QuickCheck.Monadic
    ( assert
    , monadicIO
    , monitor
    , run
    )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import System.FilePath
    ( (</>)
    )

spec :: Spec
spec = parallel $ do
  context "JSON decoders" $ do
      context "decodeHydraMessage" $ do
          let dir = "./test/vectors/hydra/hydra-node/golden/ServerOutput"
          files <- runIO (listDirectory dir) <&> map (dir </>)
          prop "can decode test vectors" $ do
              withMaxSuccess 1 $
                  conjoin $ map (prop_canDecodeFile (mapM decodeHydraMessage . getSamples)) files

prop_canDecodeFile
    :: (Json.Value -> Json.Parser b)
    -> FilePath
    -> Property
prop_canDecodeFile decoder vector = monadicIO $ do
    let errDecode = "Failed to decode JSON"
    value <- maybe (fail errDecode) pure =<< run (Json.decodeFileStrict vector)
    case Json.parse decoder value of
        Json.Error str -> do
            monitor $ counterexample (decodeUtf8 (Json.encode value))
            monitor $ counterexample str
            assert False
        Json.Success{} -> do
            assert True

getSamples :: Json.Value -> [Json.Value]
getSamples v =
  toList $ v ^. key "samples" . _Array
