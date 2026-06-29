{- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

{- Tests for some corner cases that were flaky when run with `cabal test unit`.
 - These tests actually run the executable made with `cabal build`
 - So they are external integration/end-to-end acceptance tests.
 - To run: `cabal build exe:kupo && cabal exec cabal test accept`
 -}

module Test.KupoSpec
    ( spec
    ) where

import Prelude

import Control.Concurrent
    ( threadDelay
    )
import Control.Exception
    ( assert
    , bracket
    , try
    )
import Network.HTTP.Client
    ( HttpException
    , defaultManagerSettings
    , httpNoBody
    , newManager
    , parseRequest
    , responseStatus
    )
import Network.HTTP.Types.Status
    ( status202
    )
import System.Directory
    ( Permissions (..)
    , getPermissions
    , removePathForcibly
    , setPermissions
    )
import System.Environment
    ( getEnv
    )
import System.IO.Temp
    ( createTempDirectory
    )
import System.Process
    ( proc
    , withCreateProcess
    )
import Test.Hspec
    ( Spec
    , around
    , describe
    , it
    , shouldReturn
    )

spec :: Spec
spec = around withDir $ do
    describe "Kupo server start/restart corner cases" $ do
        it "Can start in readonly mode on readonly DB" $ \dir -> do
            createDatabaseFiles dir
            makeUnwritable dir
            startReadOnly dir `shouldReturn` True

withDir :: (FilePath -> IO ()) -> IO ()
withDir = bracket (createTempDirectory "." "test-tmp") removePathForcibly

createDatabaseFiles :: FilePath -> IO ()
createDatabaseFiles dir = do
    socket <- getEnv "CARDANO_NODE_SOCKET"
    config <- getEnv "CARDANO_NODE_CONFIG"
    let kupo = proc "kupo"
            ["--node-socket", socket
            ,"--node-config", config
            ,"--workdir",     dir
            ,"--since",       "tip"
            ,"--match",       "*/*"
            ]
    withCreateProcess kupo $ \ _ _ _ _ -> do
        ready <- eventuallyReady
        pure (assert ready ())

startReadOnly :: FilePath -> IO Bool
startReadOnly dir = do
    let kupo = proc "kupo"
            ["--read-only"
            ,"--workdir", dir
            ]
    withCreateProcess kupo $ \ _ _ _ _ -> do
        eventuallyReady

makeUnwritable :: FilePath -> IO ()
makeUnwritable = setWritable False

setWritable :: Bool -> FilePath -> IO ()
setWritable x path = do
    p <- getPermissions path
    setPermissions path (p {writable = x})

eventuallyReady :: IO Bool
eventuallyReady = go (0::Int)
    where
        go attempt
            | attempt > 20 = pure False
            | otherwise    = check attempt
        check attempt = do
            ready <- tryHttp isReady
            case ready of
                Right True -> pure True
                _          -> wait attempt
        wait attempt = do
            threadDelay 1_000_000  -- 1s between retries
            go (attempt + 1)

isReady :: IO Bool
isReady = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest (url <> "/health")
  response <- httpNoBody request manager
  pure (responseStatus response == status202)

tryHttp :: IO a -> IO (Either HttpException a)
tryHttp m = try m

url :: String
url = "http://127.0.0.1:1442"


