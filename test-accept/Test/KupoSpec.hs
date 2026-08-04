{- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

{- Tests for some corner cases that were flaky when run with `cabal test unit`.
 - These tests actually run the executable made with `cabal build`
 - So they are external integration/end-to-end acceptance tests.
 -}

module Test.KupoSpec
    ( spec
    ) where

import Prelude

import Control.Concurrent
    ( threadDelay
    )
import Control.Concurrent.Async
    ( withAsync
    )
import Control.Exception
    ( SomeException
    , assert
    , bracket
    , try
    )
import Control.Exception.Base
    ( throwIO
    )
import Data.Aeson
    ( (.:)
    , (.=)
    )
import Data.Maybe
    ( fromJust
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
    , responseTimeout
    , responseTimeoutNone
    )
import Network.HTTP.Types.Header
    ( hAccept
    )
import Network.HTTP.Types.Status
    ( Status (..)
    , status200
    , status202
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
import System.Exit
    ( ExitCode (..)
    )
import System.IO.Temp
    ( createTempDirectory
    )
import System.Process
    ( CreateProcess (..)
    , ProcessHandle
    , StdStream (CreatePipe)
    , getProcessExitCode
    , proc
    , terminateProcess
    , waitForProcess
    , withCreateProcess
    )
import Test.Hspec
    ( Spec
    , around
    , describe
    , it
    , shouldReturn
    )

import qualified Control.Concurrent.Async as Async
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Process.Typed as TP
import qualified Test.Hspec.Expectations.Contrib as HSpec

type ResponseCheck = Status -> BL.ByteString -> Bool

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

type Pattern = String

data Match = Match Integer String Int Int
    deriving (Eq, Ord, Read, Show)

spec :: Spec
spec = do

    describe "Kupo server start/restart corner cases (in-memory DB)" $ do

        it "Can connect" $ do
            -- Start a kupo and check that it eventually has checkpoints
            withKupo
                ["--port"       , "1443"
                ,"--since"      , "origin"
                ,"--match"      , "*/*"
                ,"--in-memory"
                ]
                $ eventually (isConnected 1443)

        it "Auto-magically restarts when --defer-db-indexes is enabled)" $ do
            -- Read from file a valid point recent but before tip
            point <- recentPoint
            -- Start kupo since that point and with --defer-db-indexes
            withKupo
                ["--port"       , "1444"
                ,"--since"      , fromPoint point
                ,"--match"      , "*"
                ,"--in-memory"
                ,"--defer-db-indexes"
                ]
                $ eventually (hasIndexes 1444 point)

        it "Dynamically adds pattern and rolls back (when at tip)" $ do
            maybeTip <- currentNetworkTip
            case maybeTip of
                Nothing -> fail "Need to be able to use cardano-cli to get tip"
                Just (CliPoint tip) ->
                    -- Start a kupo since "tip" and matching on stakeA
                    withKupo
                        ["--port"       , "1445"
                        ,"--since"      , fromPoint tip
                        ,"--match"      , stakeA
                        ,"--in-memory"
                        ]
                        $ do
                            -- Wait for indexing to have finished
                            eventually (hasReachedPoint 1445 tip)
                            getPatterns 1445 `shouldReturn` Just [stakeA]
                            -- Add stakeB pattern forcing rollback to same point
                            putNewPattern 1445 stakeB tip `shouldReturn` True
                            getPatterns 1445 `shouldReturn` Just [stakeA,stakeB]

        it "Dynamically adds pattern and rolls back (when syncing)" $ do
            let options =
                    ["--since"      , fromPoint lastByron
                    ,"--in-memory"
                    ]
            -- Start a kupo since last Byron block and matching on stakeA
            withKupo (options ++ ["--match", stakeA, "--port", "1446"]) $ do
                -- Wait for indexing at least 100_000 slots
                eventually (hasReachedPoint 1446 lastByron136K)
                getPatterns 1446 `shouldReturn` Just [stakeA]
                getMatchesInWindow 1446 lastByron lastByron136K
                    `shouldReturn` Just
                        -- slot, hash, transaction idx, output idx
                        [Match 86440 "49ef96" 0 1]
            -- Start over, same again except matching on stakeB
            withKupo (options ++ ["--match", stakeB, "--port", "1447"]) $ do
                -- Wait for indexing at least 100_000 slots
                eventually (hasReachedPoint 1447 lastByron136K)
                getPatterns 1447 `shouldReturn` Just [stakeB]
                getMatchesInWindow 1447 lastByron lastByron136K
                    `shouldReturn` Just
                        -- output index is different from stakeA's match
                        [Match 86440 "49ef96" 0 2]
                -- Add stakeA pattern forcing rollback to last Byron block
                putNewPattern 1447 stakeA lastByron `shouldReturn` True
                -- Wait for rollback (checkpoints should go back in time)
                eventually (hasNotReachedPoint 1447 lastByron136K)
                getPatterns 1447 `shouldReturn` Just [stakeA, stakeB]
                getMatchesInWindow 1447 lastByron lastByron136K
                    `shouldReturn` Just
                        -- now we have both matches together, proving rollback
                        [ Match 86440 "49ef96" 0 2
                        , Match 86440 "49ef96" 0 1
                        ]

    around withDir $ do

        describe "Kupo server start/restart corner cases (file DB)" $ do

            it "Can start in readonly mode on readonly DB" $ \dir -> do
                -- Start a kupo on fresh database until it's ready
                withKupo
                    ["--port"   , "1448"
                    ,"--since"  , fromPoint pointA
                    ,"--match"  , "*/*"
                    ,"--workdir", dir
                    ] $ eventually (hasReachedPoint 1448 pointB)
                -- Change permissions on database's directory
                makeUnwritable dir
                -- Start a read-only kupo on dir and check that it's also ready
                withKupoUnconnected
                    ["--port"   , "1449"
                    ,"--read-only"
                    ,"--workdir", dir
                    ] $ eventually (isReady 1449)

            it "Can restart with same arguments" $ \dir -> do
                let options =
                        ["--since"  , fromPoint pointA
                        ,"--match"  , "*/*"
                        ,"--workdir", dir
                        ]
                -- Start kupo on fresh dir until reaches pointA
                withKupo (options ++ ["--port","1450"]) $
                    eventually (hasReachedPoint 1450 pointA)
                -- Restart kupo on same dir and check that it is immediately
                -- already at or past same point
                withKupo (options ++ ["--port","1451"]) $
                    eventually (hasCheckpointsAllLaterThan 1451 pointA)

            it "Cannot restart with later '--since'" $ \dir -> do
                let options =
                        ["--match"  , "*/*"
                        ,"--workdir", dir
                        ]
                -- Start kupo on fresh dir until reaches pointA
                let optionsA = options ++
                        [ "--port" , "1452"
                        , "--since", fromPoint pointA
                        ]
                withKupo optionsA $ eventually (hasReachedPoint 1452 pointA)
                -- Restart kupo on same dir but later "--since" and check
                -- that its exits with error code
                let optionsB = options ++
                        [ "--port" , "1453"
                        , "--since", fromPoint pointB
                        ]
                withKupoH optionsB $ \h -> eventually (exitsWithError h)

            it "Cannot restart with different patterns" $ \dir -> do
                let options =
                        ["--since", fromPoint pointA
                        ,"--workdir", dir
                        ]
                -- Start kupo on fresh dir until reaches pointA
                let optionsA = options ++
                        [ "--port" , "1454"
                        , "--match", "*/*"
                        ]
                withKupo optionsA $ eventually (hasReachedPoint 1454 pointA)
                -- Restart kupo on same dir but with different pattern and check
                -- that its exits with error code
                let optionsB = options ++
                        [ "--port" , "1455"
                        , "--match", "*"
                        ]
                withKupoH optionsB $ \h -> eventually (exitsWithError h)

withKupoProcess :: [String] -> (ProcessHandle -> IO a) -> IO a
withKupoProcess options action = do
    process <- kupo options
    withCreateProcess process $ \_stdin stdout _stderr h -> do
        withAsync (T.hGetContents (fromJust stdout)) $ \logsAsync -> do
            outcome <- try @SomeException (action h)
            stopProcess h
            logs <- Async.wait logsAsync
            T.length logs `seq` case outcome of
                Right a ->
                    pure a
                Left err ->
                    HSpec.annotate
                        ("Kupo logs:\n" <> T.unpack logs)
                        (throwIO err)

stopProcess :: ProcessHandle -> IO ()
stopProcess h = do
    exitCode <- getProcessExitCode h
    case exitCode of
        Just _ ->
            pure ()
        Nothing -> do
            terminateProcess h
            _ <- waitForProcess h
            pure ()

withKupoH :: [String] -> (ProcessHandle -> IO a) -> IO a
withKupoH options action = do
    socket <- getEnv "CARDANO_NODE_SOCKET"
    config <- getEnv "CARDANO_NODE_CONFIG"
    let nodeOptions = ["--node-socket", socket ,"--node-config", config]
    withKupoProcess (nodeOptions ++ options) action

withKupo :: [String] -> IO a -> IO a
withKupo options action = do
    withKupoH options (const action)

withKupoUnconnected :: [String] -> IO a -> IO a
withKupoUnconnected options action =
    withKupoProcess options (const action)

withDir :: (FilePath -> IO ()) -> IO ()
withDir = bracket (createTempDirectory "." "test-tmp") removePathForcibly

kupo :: [String] -> IO CreateProcess
kupo options = do
    pure $ (proc "kupo" options) { std_out = CreatePipe }

makeUnwritable :: FilePath -> IO ()
makeUnwritable = setWritable False

setWritable :: Bool -> FilePath -> IO ()
setWritable x path = do
    p <- getPermissions path
    setPermissions path (p {writable = x})

eventually :: IO Bool -> IO ()
eventually p = go (0::Int) >>= flip assert (pure ())
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

isConnected :: Int -> IO Bool
isConnected port = checkResponse port "/checkpoints" containsCheckpoints

isReady :: Int -> IO Bool
isReady port = checkResponse port "/health" isHealthy

hasIndexes :: Int -> Point -> IO Bool
hasIndexes port point = do
    indexes <- checkResponse port "/health" hasIndexesInstalled
    chckpts <- checkResponse port "/checkpoints" (laterThan point)
    pure (indexes && chckpts)

hasReachedPoint :: Int -> Point -> IO Bool
hasReachedPoint port = checkResponse port "/checkpoints" . hasReached

hasNotReachedPoint :: Int -> Point -> IO Bool
hasNotReachedPoint port = checkResponse port "/checkpoints" . hasNotReached

hasCheckpointsAllLaterThan :: Int -> Point -> IO Bool
hasCheckpointsAllLaterThan port =
    checkResponse port "/checkpoints" . laterThan

exitsWithError :: ProcessHandle -> IO Bool
exitsWithError h = do
    e <- getProcessExitCode h
    pure $ case e of
        Nothing              -> False
        Just ExitSuccess     -> False
        Just (ExitFailure _) -> True

checkResponse :: Int -> String -> ResponseCheck -> IO Bool
checkResponse port path check = do
    manager  <- newManager defaultManagerSettings
    request' <- request port path
    let request'' = request'
            {requestHeaders = [(hAccept, "application/json; charset=utf-8")]
            }
    response <- httpLbs request'' manager
    let
        status = responseStatus response
        body   = responseBody   response
    pure (check status body)

getPatterns :: Int -> IO (Maybe [Pattern])
getPatterns port = do
    manager <- newManager defaultManagerSettings
    request' <- request port "/patterns"
    let request'' = request'
            {requestHeaders = [(hAccept, "application/json; charset=utf-8")]
            }
    response <- httpLbs request'' manager
    case responseStatus response of
        (Status 200 _) -> do
            let body = responseBody response
            pure (Aeson.decode body :: Maybe [Pattern])
        _              ->
            pure Nothing


putNewPattern :: Int -> Pattern -> Point -> IO Bool
putNewPattern port pattern point = do
    manager  <- newManager defaultManagerSettings
    request' <- request port ("/patterns/" <> pattern)
    let body = RequestBodyLBS (Aeson.encode (PutPatternBody point))
    let request'' = request'
            { method          = "PUT"
            , requestBody     =  body
            , responseTimeout = responseTimeoutNone
            }
    response <- httpLbs request'' manager
    case responseStatus response of
        (Status 200 _) -> pure True
        _              -> pure False

getMatchesInWindow :: Int -> Point -> Point -> IO (Maybe [Match])
getMatchesInWindow port from to = do
    manager <- newManager defaultManagerSettings
    request' <- request port "/matches"
    let request'' = request'
            {requestHeaders   = [(hAccept, "application/json; charset=utf-8")]
            , responseTimeout = responseTimeoutNone
            }
    response <- httpLbs request'' manager
    case responseStatus response of
        (Status 200 _) -> do
            let body = responseBody response
            let maybeMatches = Aeson.decode body :: Maybe [Match]
            pure (filter (matchInWindow from to) <$> maybeMatches)
        _              ->
            pure Nothing

matchInWindow :: Point -> Point -> Match -> Bool
matchInWindow (Point (Slot s1) _) (Point (Slot s2) _) (Match s _ _ _) =
    s1 < s && s < s2

currentNetworkTip :: IO (Maybe CliPoint)
currentNetworkTip = do
    socket <- getEnv "CARDANO_NODE_SOCKET"
    let args =
            [ "query", "tip"
            , "--testnet-magic", "1"
            , "--socket-path", socket
            ]
    (_, out) <- TP.readProcessStdout (TP.proc "cardano-cli" args)
    pure (Aeson.decode out)

containsCheckpoints :: ResponseCheck
containsCheckpoints = checkpoints (const True)

hasReached :: Point -> ResponseCheck
hasReached (Point slot _) = checkpoints ((>= slot) . head)

hasNotReached :: Point -> ResponseCheck
hasNotReached (Point slot _) = checkpoints ((< slot) . head)

laterThan :: Point -> ResponseCheck
laterThan (Point slot _) = checkpoints (all (>= slot))

isHealthy :: ResponseCheck
isHealthy status _ = status == status202 || status == status200

hasIndexesInstalled :: ResponseCheck
hasIndexesInstalled status body =
    (status == status200 || status == status202)
    && (indexesFlag body) == Just (Indexes "installed")

checkpoints :: ([Slot] -> Bool) -> ResponseCheck
checkpoints f status body =
    status == status200 && hasSlotsThat (Aeson.decode body :: Maybe [Slot])
    where
        hasSlotsThat Nothing       = False
        hasSlotsThat (Just [])     = False
        hasSlotsThat (Just slots ) = f slots

indexesFlag :: BL.ByteString -> Maybe Indexes
indexesFlag = Aeson.decode

tryHttp :: IO a -> IO (Either HttpException a)
tryHttp = try

url :: Int -> String
url port = "http://127.0.0.1:" <> show port

request :: Int -> String -> IO Request
request port = parseRequest . (url port <>)

recentPoint :: IO Point
recentPoint = fmap read (readFile "test-accept/point-recent.dat")

fromPoint :: Point -> String
fromPoint (Point (Slot slot) hash) = show slot ++ "." ++ hash

instance Aeson.FromJSON Slot where
    parseJSON = Aeson.withObject "Slot" $ \o -> Slot <$> o .: "slot_no"

instance Aeson.FromJSON Indexes where
    parseJSON = Aeson.withObject "Health" $ \o -> do
        c <- o .: "configuration"
        i <- c .: "indexes"
        pure (Indexes i)

instance Aeson.FromJSON Point where
    parseJSON = Aeson.withObject "Point" $ \o -> do
        s <- o .: "slot_no"
        h <- o .: "header_hash"
        pure (Point s h)

instance Aeson.FromJSON CliPoint where
    parseJSON = Aeson.withObject "CliPoint" $ \o -> do
        s <- o .: "slot"
        h <- o .: "hash"
        pure (CliPoint (Point (Slot s) h))

instance Aeson.ToJSON PutPatternBody where
    toJSON (PutPatternBody (Point (Slot slot) hash)) = Aeson.object
        [ "rollback_to" .= Aeson.object
            [ "slot_no"     .= slot
            , "header_hash" .= hash
            ]
        , "limit" .= ("unsafe_allow_beyond_safe_zone" :: String)
        ]

instance Aeson.FromJSON Match where
    parseJSON = Aeson.withObject "Match" $ \o -> do
        tidx <- o .: "transaction_index"
        oidx <- o .: "output_index"
        c    <- o .: "created_at"
        slot <- c .: "slot_no"
        hash <- c .: "header_hash"
        pure (Match slot (take 6 hash) tidx oidx)

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

-- A point that exists on-chain. Last Byron block.
lastByron :: Point
lastByron =
    Point
        (Slot 84242)
        "45899e8002b27df291e09188bfe3aeb5397ac03546a7d0ead93aa2500860f1af"

-- A point that exists on-chain. Over 100_000 slots after lastByron
lastByron136K :: Point
lastByron136K =
    Point
        (Slot 220860)
        "10915760772b5cf13b5138eda8a4ea36f871ece619547bc4eec63831008ebf69"

-- Some stake key in Shelley, present in addresses from early blocks of Shelley.
-- (Called someStakeKey in other 'unit' test suite)
stakeA :: Pattern
stakeA =
    "*/968d1021ebd7178e1fb0e79676982825cabc779b653e1234d58ce3c6"

-- Similar to stakeA but distinct from it.
-- (Called someOtherStakeKey in other 'unit' test suite)
stakeB :: Pattern
stakeB =
    "*/f130204b518f70c19995449e3737eded3d9ffc31cb50ec0e45010ba3"
