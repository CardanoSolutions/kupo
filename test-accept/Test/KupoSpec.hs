{- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

{- Tests for some corner cases that were flaky when run with `cabal test unit`.
 - These tests actually run the executable made with `cabal build`
 - So they are external integration/end-to-end acceptance tests.
 - To run on development server (in current repo):
 -    `cabal build exe:kupo && cabal exec cabal test accept`
 - To run on installed server (in PATH, e.g. with brew install):
 -    `cabal test accept`
 - To run on server in an alternative repo (e.g. different branch):
 -    ```
 -    pushd ../kupo-pr-204
 -    tmpPath=$(dirname $(cabal list-bin exe:kupo))
 -    popd
 -    PATH=$tmpPath:$PATH cabal test accept
 -    ```
 -}

module Test.KupoSpec
    ( spec
    , currentNetworkTip
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
    , (.=)
    )
import Network.HTTP.Client
    ( HttpException
    , Request
    , RequestBody (..)
    , defaultManagerSettings
    , httpLbs
    , method
    , newManager
    , parseRequest
    , requestBody
    , requestHeaders
    , responseBody
    , responseStatus
    )
import Network.HTTP.Types.Header
    (hAccept
    )
import Network.HTTP.Types.Status
    ( Status (..)
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
import System.Exit
    ( ExitCode (..)
    )
import System.Process
    ( CreateProcess
    , ProcessHandle
    , getProcessExitCode
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
import qualified System.Process.Typed as TP

type ResponseCheck = Status -> B.ByteString -> Bool

newtype Slot = Slot Integer deriving (Eq, Ord, Read, Show)

data Point = Point Slot String
    deriving (Eq, Ord, Read, Show)

-- Wrapper type needed to have a separate FromJSON instance
-- since the cardano-cli formats slot/hash differently from Kupo
newtype CliPoint = CliPoint Point
    deriving (Eq, Ord, Read, Show)

-- Wrapper type useful to have a specific ToJSON instance
newtype PutPatternBody = PutPatternBody Point

newtype Indexes = Indexes String deriving (Eq, Ord, Show)

type StakeKey = String

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

        it "Auto-magically restarts when --defer-db-indexes is enabled)" $ do
            -- Read from file a valid point recent but before tip
            point <- recentPoint
            -- Start kupo since that point and with --defer-db-indexes
            withKupo
                ["--since"      , fromPoint point
                ,"--match"      , "*"
                ,"--in-memory"
                ,"--defer-db-indexes"
                ]
                $ eventually (hasIndexes point) `shouldReturn` True

        it "Dynamically adds pattern and rolls back to given past point" $ do
            -- Start a kupo since "pointB" and matching on stakeB
            withKupo
                ["--since"      , fromPoint pointB
                ,"--match"      , "*/" <> stakeB
                ,"--in-memory"
                ]
                $ do
                    -- Wait for indexing to have started
                    reached <- eventually (hasReachedPoint pointB)
                    pure (assert reached ())
                    later <- allCheckpointsLaterThan pointB
                    pure (assert later ())
                    -- Add other pattern forcing rollback to earlier pointA
                    putNewPattern stakeA pointA `shouldReturn` True

    around withDir $ do

        describe "Kupo server start/restart corner cases (file DB)" $ do

            it "Can start in readonly mode on readonly DB" $ \dir -> do
                -- Start a kupo on fresh database until it's ready
                withKupo
                    ["--since"  , fromPoint pointA
                    ,"--match"  , "*/*"
                    ,"--workdir", dir
                    ]
                    $ do
                        reached <- eventually (hasReachedPoint pointB)
                        pure (assert reached ())
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
                        ["--since"  , fromPoint pointA
                        ,"--match"  , "*/*"
                        ,"--workdir", dir
                        ]
                -- Start kupo on fresh dir until reaches pointA
                withKupo options $ do
                    reached <- eventually (hasReachedPoint pointA)
                    pure (assert reached ())
                -- Restart kupo on same dir and check that it is immediately
                -- already at or past same point
                withKupo options $ do
                    connected <- eventually isConnected
                    pure (assert connected ())
                    (hasReachedPoint pointA) `shouldReturn` True

            it "Cannot restart with later '--since'" $ \dir -> do
                let options =
                        ["--match"  , "*/*"
                        ,"--workdir", dir
                        ]
                -- Start kupo on fresh dir until reaches pointA
                withKupo (options ++ ["--since", fromPoint pointA]) $ do
                    reached <- eventually (hasReachedPoint pointA)
                    pure (assert reached ())
                -- Restart kupo on same dir but later "--since" and check
                -- that its exits with error code
                withKupoH (options ++ ["--since", fromPoint pointB]) $ \h -> do
                    eventually (exitsWithError h) `shouldReturn` True

            it "Cannot restart with different patterns" $ \dir -> do
                let options =
                        ["--since", fromPoint pointA
                        ,"--workdir", dir
                        ]
                -- Start kupo on fresh dir until reaches pointA
                withKupo (options ++ ["--match"  , "*/*"]) $ do
                    reached <- eventually (hasReachedPoint pointA)
                    pure (assert reached ())
                -- Restart kupo on same dir but with different pattern and check
                -- that its exits with error code
                withKupoH (options ++ ["--match", "*"]) $ \h -> do
                    eventually (exitsWithError h) `shouldReturn` True

withKupoH :: [String] -> (ProcessHandle -> IO a) -> IO a
withKupoH options action = do
    process <- kupo options
    withCreateProcess process $ \_ _ _ h -> action h

withKupo :: [String] -> IO a -> IO a
withKupo options action = withKupoH options (\_ -> action)

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

hasIndexes :: Point -> IO Bool
hasIndexes pt = do
    indexes <- checkResponse "/health" hasIndexesInstalled
    chckpts <- checkResponse "/checkpoints" (laterThan pt)
    pure (indexes && chckpts)

hasReachedPoint :: Point -> IO Bool
hasReachedPoint = checkResponse "/checkpoints" . hasReached

allCheckpointsLaterThan :: Point -> IO Bool
allCheckpointsLaterThan = checkResponse "/checkpoints" . laterThan

exitsWithError :: ProcessHandle -> IO Bool
exitsWithError h = do
    e <- getProcessExitCode h
    pure $ case e of
        Nothing              -> False
        Just ExitSuccess     -> False
        Just (ExitFailure _) -> True

checkResponse :: String -> ResponseCheck -> IO Bool
checkResponse path check = do
    manager  <- newManager defaultManagerSettings
    request' <- request path
    let request'' = request'
            {requestHeaders = [(hAccept, "application/json; charset=utf-8")]
            }
    response <- httpLbs request'' manager
    let
        status = responseStatus response
        body   = responseBody   response
    pure (check status body)

putNewPattern :: StakeKey -> Point -> IO Bool
putNewPattern key point = do
    manager  <- newManager defaultManagerSettings
    request' <- request ("/patterns/*/" <> key)
    let request'' = request'
            { method = "PUT"
            , requestBody = RequestBodyLBS (A.encode (PutPatternBody point))
            }
    response <- httpLbs request'' manager
    case responseStatus response of
        (Status 200 _) -> pure True
        _              -> pure False

containsCheckpoints :: ResponseCheck
containsCheckpoints status body =
    status == status200 && hasSlots (A.decode body :: Maybe [Slot])
    where
        hasSlots Nothing   = False
        hasSlots (Just []) = False
        hasSlots (Just _ ) = True

hasReached :: Point -> ResponseCheck
hasReached (Point slot _) status body =
    status == status200 && hasReached' slot (A.decode body :: Maybe [Slot])
    where
        hasReached' _ Nothing      = False
        hasReached' _ (Just [])    = False
        hasReached' x (Just (y:_)) = y >= x

isHealthy :: ResponseCheck
isHealthy status _ = status == status202 || status == status200

hasIndexesInstalled :: ResponseCheck
hasIndexesInstalled status body =
    (status == status200 || status == status202)
    && (indexesFlag body) == Just (Indexes "installed")

laterThan :: Point -> ResponseCheck
laterThan (Point slot _) status body =
    status == status200 && laterThanSlot (A.decode body :: Maybe [Slot])
    where
        laterThanSlot Nothing      = False
        laterThanSlot (Just slots) = all (>= slot) slots

indexesFlag :: B.ByteString -> Maybe Indexes
indexesFlag = A.decode

tryHttp :: IO a -> IO (Either HttpException a)
tryHttp m = try m

url :: String
url = "http://127.0.0.1:1442"

request :: String -> IO Request
request = parseRequest . (url <>)

-- A point that exists on-chain. Earlier than pointB.
-- (Called somePoint in other 'unit' test suite)
pointA :: Point
pointA =
    Point
        (Slot 11017324)
        "195908564a66d713bd2b71a9b1f290be6853cb31085fe7371276a35a2f8f7e62"

-- A point that exists on-chain. Later than pointA.
-- (Called someOtherPoint in other 'unit' test suite)
pointB :: Point
pointB =
    Point
        (Slot 36492716)
        "d51095ef5405d83e7a1c82b98d12b357ba6b95f070f684bb38ab47ef90b21688"

-- Some stake key in Shelley, present in addresses from early blocks of Shelley.
-- (Called someStakeKey in other 'unit' test suite)
stakeA :: StakeKey
stakeA =
    "968d1021ebd7178e1fb0e79676982825cabc779b653e1234d58ce3c6"

-- Similar to stakeA but distinct from it.
-- (Called someOtherStakeKey in other 'unit' test suite)
stakeB :: StakeKey
stakeB =
    "f130204b518f70c19995449e3737eded3d9ffc31cb50ec0e45010ba3"

recentPoint :: IO Point
recentPoint = fmap read (readFile "test-accept/point-recent.dat")

fromPoint :: Point -> String
fromPoint (Point (Slot slot) hash) = show slot ++ "." ++ hash

instance A.FromJSON Slot where
    parseJSON = A.withObject "Slot" $ \o -> Slot <$> o .: "slot_no"

instance A.FromJSON Indexes where
    parseJSON = A.withObject "Health" $ \o -> do
        c <- o .: "configuration"
        i <- c .: "indexes"
        pure (Indexes i)

instance A.FromJSON Point where
    parseJSON = A.withObject "Point" $ \o -> do
        s <- o .: "slot_no"
        h <- o .: "header_hash"
        pure (Point s h)

instance A.FromJSON CliPoint where
    parseJSON = A.withObject "CliPoint" $ \o -> do
        s <- o .: "slot"
        h <- o .: "hash"
        pure (CliPoint (Point (Slot s) h))

instance A.ToJSON PutPatternBody where
    toJSON (PutPatternBody (Point (Slot slot) hash)) = A.object
        [ "rollback_to" .= A.object
            [ "slot_no"     .= slot
            , "header_hash" .= hash
            ]
        , "limit" .= ("unsafe_allow_beyond_safe_zone" :: String)
        ]

currentNetworkTip :: IO (Maybe CliPoint)
currentNetworkTip = do
    let args =
            [ "query", "tip"
            , "--testnet-magic", "1"
            ]
    (_, out) <- TP.readProcessStdout (TP.proc "cardano-cli" args)
    pure (A.decode out)


