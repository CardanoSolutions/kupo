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
import Data.Aeson
    ( (.:)
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

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as B

type ResponseCheck = Status -> B.ByteString -> Bool

newtype Slot = Slot Integer deriving (Eq, Ord, Show)

spec :: Spec
spec = do

    describe "Kupo server start/restart corner cases (in-memory DB)" $ do

        it "Can connect" $ do
            -- Start a kupo and check that it eventually has checkpoints
            withKupo
                ["--since"      , "origin"
                ,"--match"      , "*/*"
                ,"--in-memory"
                ]
                $ eventually isConnected `shouldReturn` True

    around withDir $ do

        describe "Kupo server start/restart corner cases (file DB)" $ do

            it "Can start in readonly mode on readonly DB" $ \dir -> do
                -- Start a kupo on fresh database until it's ready
                withKupo
                    ["--since"  , "tip"
                    ,"--match"  , "*/*"
                    ,"--workdir", dir
                    ]
                    $ do
                        ready <- eventually isReady
                        pure (assert ready ())
                -- Change permissions on database's directory
                makeUnwritable dir
                -- Start a read-only kupo on dir and check that it's also ready
                let process = proc "kupo"
                        ["--read-only"
                        ,"--workdir", dir
                        ]
                withCreateProcess process $ \ _ _ _ _ -> do
                    eventually isReady `shouldReturn` True

            it "Can restart with same arguments" $ \dir -> do
                let options =
                        ["--since"  , somePoint
                        ,"--match"  , "*/*"
                        ,"--workdir", dir
                        ]
                -- Start kupo on fresh dir until reaches somePoint
                withKupo options $ do
                    reached <- eventually hasReachedSomePoint
                    pure (assert reached ())
                -- Restart kupo on same dir and check that it is immediately
                -- already at or past same point
                withKupo options $ do
                    ready <- eventually isReady
                    pure (assert ready ())
                    hasReachedSomePoint `shouldReturn` True

withKupo :: [String] -> IO a -> IO a
withKupo options action = do
    process <- kupo options
    withCreateProcess process $ \_ _ _ _ -> action

withDir :: (FilePath -> IO ()) -> IO ()
withDir = bracket (createTempDirectory "." "test-tmp") removePathForcibly

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
isConnected = checkResponse "/checkpoints" containsCheckpoints

isReady :: IO Bool
isReady = checkResponse "/health" isHealthy

hasReachedSomePoint :: IO Bool
hasReachedSomePoint = checkResponse "/checkpoints" $ hasReached $ Slot 11017324

checkResponse :: String -> ResponseCheck -> IO Bool
checkResponse path check = do
    manager  <- newManager defaultManagerSettings
    request' <- request path
    response <- httpLbs request' manager
    let
        status = responseStatus response
        body   = responseBody   response
    pure (check status body)

containsCheckpoints :: ResponseCheck
containsCheckpoints status body =
    status == status200 && hasSlots (A.decode body :: Maybe [Slot])
    where
        hasSlots Nothing   = False
        hasSlots (Just []) = False
        hasSlots (Just _ ) = True

hasReached :: Slot -> ResponseCheck
hasReached slot status body =
    status == status200 && hasReached' slot (A.decode body :: Maybe [Slot])
    where
        hasReached' _ Nothing      = False
        hasReached' _ (Just [])    = False
        hasReached' x (Just (y:_)) = y >= x

isHealthy :: ResponseCheck
isHealthy status _ = status == status202 || status == status200

tryHttp :: IO a -> IO (Either HttpException a)
tryHttp m = try m

url :: String
url = "http://127.0.0.1:1442"

request :: String -> IO Request
request = parseRequest . (url <>)

somePoint :: String
somePoint =
    "11017324.195908564a66d713bd2b71a9b1f290be6853cb31085fe7371276a35a2f8f7e62"

instance A.FromJSON Slot where
    parseJSON = A.withObject "Slot" $ \o -> Slot <$> o .: "slot_no"
