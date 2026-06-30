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
import Data.Char
    (ord
    )
import Network.HTTP.Client
    ( HttpException
    , Request
    , defaultManagerSettings
    , httpLbs
    , newManager
    , parseRequest
    , responseBody
    , responseStatus
    )
import Network.HTTP.Types.Status
    ( Status
    , status202
    , status200
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
    ( CreateProcess
    , proc
    , withCreateProcess
    )
import Test.Hspec
    ( Spec
    , around
    , describe
    , it
    , shouldReturn
    )

import qualified Data.ByteString.Lazy as B

type ResponseCheck = Status -> B.ByteString -> Bool

spec :: Spec
spec = do

    describe "Kupo server start/restart corner cases (in-memory DB)" $ do

        it "Can connect" $ do
            startInMemorySinceOriginMatchShelley `shouldReturn` True

    around withDir $ do

        describe "Kupo server start/restart corner cases (file DB)" $ do

            it "Can start in readonly mode on readonly DB" $ \dir -> do
                createDatabaseFiles dir
                makeUnwritable dir
                startReadOnly dir `shouldReturn` True

startInMemorySinceOriginMatchShelley :: IO Bool
startInMemorySinceOriginMatchShelley = do
    process <- kupo
        ["--since"      , "origin"
        ,"--match"      , "*/*"
        ,"--in-memory"
        ]
    withCreateProcess process $ \ _ _ _ _ -> do
        eventually isConnected

withDir :: (FilePath -> IO ()) -> IO ()
withDir = bracket (createTempDirectory "." "test-tmp") removePathForcibly

createDatabaseFiles :: FilePath -> IO ()
createDatabaseFiles dir = do
    process <- kupo
        ["--since"  , "tip"
        ,"--match"  , "*/*"
        ,"--workdir", dir
        ]
    withCreateProcess process $ \ _ _ _ _ -> do
        ready <- eventually isReady
        pure (assert ready ())

kupo :: [String] -> IO CreateProcess
kupo options = do
    socket <- getEnv "CARDANO_NODE_SOCKET"
    config <- getEnv "CARDANO_NODE_CONFIG"
    pure $ proc "kupo"
        (["--node-socket", socket
         ,"--node-config", config
         ]
         ++ options
        )

makeUnwritable :: FilePath -> IO ()
makeUnwritable = setWritable False

startReadOnly :: FilePath -> IO Bool
startReadOnly dir = do
    let kupo = proc "kupo"
            ["--read-only"
            ,"--workdir", dir
            ]
    withCreateProcess kupo $ \ _ _ _ _ -> do
        eventually isReady

setWritable :: Bool -> FilePath -> IO ()
setWritable x path = do
    p <- getPermissions path
    setPermissions path (p {writable = x})

eventually :: IO Bool -> IO Bool
eventually p = go (0::Int)
    where
        go attempt
            | attempt > 20 = pure False
            | otherwise    = check attempt
        check attempt = do
            ready <- tryHttp p
            case ready of
                Right True -> pure True
                _          -> wait attempt
        wait attempt = do
            threadDelay 1_000_000  -- 1s between retries
            go (attempt + 1)

isConnected :: IO Bool
isConnected = checkResponse "/checkpoints" containsNonEmptyList

isReady :: IO Bool
isReady = checkResponse "/health" isHealthy

checkResponse :: String -> ResponseCheck -> IO Bool
checkResponse path check = do
    manager  <- newManager defaultManagerSettings
    request' <- request path
    response <- httpLbs request' manager
    let
        status = responseStatus response
        body   = responseBody   response
    pure (check status body)

containsNonEmptyList :: Status -> B.ByteString -> Bool
containsNonEmptyList status body =
    status == status200
    && B.length body > 2
    && startsWithBracket body
    && endsWithBracket   body

startsWithBracket :: B.ByteString -> Bool
startsWithBracket xs =
    case B.uncons xs of
        Nothing     -> False
        Just (w, _) -> w == fromIntegral (ord '[')

endsWithBracket :: B.ByteString -> Bool
endsWithBracket xs =
    case B.unsnoc xs of
        Nothing     -> False
        Just (_, w) -> w == fromIntegral (ord ']')

isHealthy :: ResponseCheck
isHealthy status _ = status == status202

tryHttp :: IO a -> IO (Either HttpException a)
tryHttp m = try m

url :: String
url = "http://127.0.0.1:1442"

request :: String -> IO Request
request = parseRequest . (url <>)
