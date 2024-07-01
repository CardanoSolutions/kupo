-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- Used to partially pattern match result of parsing default arguments. Okay-ish
-- because it's test code and, having it fail would be instantly caught.
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{-# LANGUAGE PatternSynonyms #-}

module Test.Kupo.OptionsSpec
    ( spec
    ) where

import Kupo.Prelude

import Data.List
    ( isInfixOf
    )
import Kupo.Control.MonadLog
    ( Severity (..)
    , TracerDefinition (..)
    , defaultTracers
    )
import Kupo.Data.Cardano
    ( mkOutputReference
    , pattern GenesisPoint
    , unsafeAssetNameFromBytes
    , unsafePolicyIdFromBytes
    , unsafeTransactionIdFromBytes
    )
import Kupo.Data.Configuration
    ( ChainProducer (..)
    , Configuration (..)
    , DatabaseLocation (..)
    , InputManagement (..)
    , Since (..)
    )
import Kupo.Data.Pattern
    ( MatchBootstrap (..)
    , Pattern (..)
    )
import Kupo.Options
    ( Command (..)
    , Tracers (..)
    , parseOptionsPure
    )
import Test.Hspec
    ( Expectation
    , Spec
    , context
    , expectationFailure
    , parallel
    , shouldBe
    , shouldSatisfy
    , specify
    )
import Test.Kupo.Fixture
    ( somePoint
    )

spec :: Spec
spec = parallel $ do
    context "parseOptions(Pure)" $ do
        forM_ matrix $ \(args, expect) -> do
            let title = toString $ unwords $ toText <$> args
            specify title $ expect (parseOptionsPure args)

        specify "invalid" $ do
            case parseOptionsPure ["--nope"] of
                Right{} -> expectationFailure "Expected error but got success."
                Left e -> e `shouldSatisfy` isInfixOf "Invalid option"

        specify "test completion" $ do
            case parseOptionsPure ["--node-so\t"] of
                Right{} -> expectationFailure "Expected error but got success."
                Left e -> e `shouldSatisfy` isInfixOf "Invalid option"
  where
    matrix =
        [ ( []
          , shouldFail
          )
        , ( [ "--node-socket", "./node.socket" ]
          , shouldFail
          )
        , ( [ "--node-config", "./node.config" ]
          , shouldFail
          )
        , ( [ "--ogmios-host", "localhost" ]
          , shouldFail
          )
        , ( [ "--ogmios-port", "1337" ]
          , shouldFail
          )
        , ( [ "--hydra-host", "localhost" ]
          , shouldFail
          )
        , ( [ "--hydra-port", "4001" ]
          , shouldFail
          )
        , ( defaultArgs
          , shouldParseAppConfiguration $ defaultConfiguration
            { chainProducer = CardanoNode
                { nodeSocket = "./node.socket"
                , nodeConfig = "./node.config"
                }
            , databaseLocation = InMemory Nothing
            }
          )
        , ( defaultArgs'
          , shouldParseAppConfiguration $ defaultConfiguration
            { chainProducer = Ogmios
                { ogmiosHost = "localhost"
                , ogmiosPort = 1337
                }
            , databaseLocation = InMemory Nothing
            }
          )
        , ( defaultArgs''
          , shouldParseAppConfiguration $ defaultConfiguration
            { chainProducer = Hydra
                { hydraHost = "localhost"
                , hydraPort = 4001
                }
            , databaseLocation = InMemory Nothing
            }
          )
        , ( filter (/= "--in-memory") defaultArgs ++
            [ "--workdir", "./workdir"
            ]
          , shouldParseAppConfiguration $ defaultConfiguration
            { databaseLocation = Dir "./workdir"
            }
          )
        , ( defaultArgs ++ [ "--host", "0.0.0.0" ]
          , shouldParseAppConfiguration $ defaultConfiguration
                { serverHost = "0.0.0.0" }
          )
        , ( defaultArgs ++ [ "--port", "42" ]
          , shouldParseAppConfiguration $ defaultConfiguration
            { serverPort = 42 }
          )
        , ( defaultArgs ++ [ "--port", "#" ]
          , shouldFail
          )
        , ( defaultArgs ++ [ "--prune-utxo" ]
          , shouldParseAppConfiguration $ defaultConfiguration
            { inputManagement = RemoveSpentInputs
            }
          )
        , ( defaultArgs ++ [ "--since", "11017324.195908564a66d713bd2b71a9b1f290be6853cb31085fe7371276a35a2f8f7e62" ]
          , shouldParseAppConfiguration $ defaultConfiguration
            { since = Just (SincePoint somePoint) }
          )
        , ( defaultArgs ++ [ "--since", "origin" ]
          , shouldParseAppConfiguration $ defaultConfiguration
            { since = Just (SincePoint GenesisPoint) }
          )
        , ( defaultArgs ++ [ "--since", "tip" ]
          , shouldParseAppConfiguration $ defaultConfiguration
            { since = Just SinceTip }
          )
        , ( defaultArgs ++ [ "--since", "#" ]
          , shouldFail
          )
        , ( defaultArgs ++ [ "--match", "*" ]
          , shouldParseAppConfiguration $ defaultConfiguration
            { patterns = fromList [ MatchAny IncludingBootstrap ]
            }
          )
        , ( defaultArgs ++ [ "--match", "*" ] ++ [ "--match", "*" ]
          , shouldParseAppConfiguration $ defaultConfiguration
            { patterns = fromList [ MatchAny IncludingBootstrap ]
            }
          )
        , ( defaultArgs ++ [ "--match", "*", "--match", "*/*" ]
          , shouldParseAppConfiguration $ defaultConfiguration
            { patterns = fromList [ MatchAny IncludingBootstrap, MatchAny OnlyShelley ]
            }
          )
        , ( defaultArgs ++ [ "--match", "stake1uxuweccthd5qqgfh49uq592qr8mh7a78g89jjv0u74vnzasr3safd" ]
          , shouldParseAppConfiguration $ defaultConfiguration
            { patterns = fromList [ MatchDelegation $ unsafeDecodeBase16 "b8ece30bbb68002137a9780a154019f77f77c741cb2931fcf5593176" ]
            }
          )
        , ( defaultArgs ++ [ "--match", "14@2e7ee124eccbc648789008f8669695486f5727cada41b2d86d1c36355c76b771" ]
          , shouldParseAppConfiguration $ defaultConfiguration
            { patterns =
                let
                    str =
                        "2e7ee124eccbc648789008f8669695486f5727cada41b2d86d1c36355c76b771"
                    outRef =
                        mkOutputReference
                            (unsafeTransactionIdFromBytes $ unsafeDecodeBase16 str)
                            14
                 in
                    fromList [ MatchOutputReference outRef ]
            }
          )
        , ( defaultArgs ++ [ "--match", "*@2e7ee124eccbc648789008f8669695486f5727cada41b2d86d1c36355c76b771" ]
          , shouldParseAppConfiguration $ defaultConfiguration
            { patterns =
                let
                    str =
                        "2e7ee124eccbc648789008f8669695486f5727cada41b2d86d1c36355c76b771"
                    txId =
                        unsafeTransactionIdFromBytes $ unsafeDecodeBase16 str
                 in
                    fromList [ MatchTransactionId txId ]
            }
          )
        , ( defaultArgs ++ [ "--match", "2e7ee124eccbc648789008f8669695486f5727cada41b2d86d1c3635.*" ]
          , shouldParseAppConfiguration $ defaultConfiguration
            { patterns =
                let
                    str =
                        "2e7ee124eccbc648789008f8669695486f5727cada41b2d86d1c3635"
                    policyId =
                        unsafePolicyIdFromBytes $ unsafeDecodeBase16 str
                 in
                    fromList [ MatchPolicyId policyId ]
            }
          )
        , ( defaultArgs ++ [ "--match", "2e7ee124eccbc648789008f8669695486f5727cada41b2d86d1c3635.706174617465" ]
          , shouldParseAppConfiguration $ defaultConfiguration
            { patterns =
                let
                    str =
                        "2e7ee124eccbc648789008f8669695486f5727cada41b2d86d1c3635"
                    policyId =
                        unsafePolicyIdFromBytes $ unsafeDecodeBase16 str
                    assetName =
                        unsafeAssetNameFromBytes $ unsafeDecodeBase16 "706174617465"
                 in
                    fromList [ MatchAssetId (policyId, assetName) ]
            }
          )
        , ( defaultArgs ++ [ "--match", "{1234}" ]
          , shouldParseAppConfiguration $ defaultConfiguration
            { patterns = fromList [ MatchMetadataTag 1234 ]
            }
          )
        , ( defaultArgs ++ [ "--match", "NOT-A-PATTERN" ]
          , shouldFail
          )
        , ( defaultArgs ++ [ "--gc-interval", "42" ]
          , shouldParseAppConfiguration $ defaultConfiguration
            { garbageCollectionInterval = 42
            }
          )
        , ( defaultArgs ++ [ "--gc-interval", "foo" ]
          , shouldFail
          )
        , ( defaultArgs ++ [ "--gc-interval", "14.42" ]
          , shouldFail
          )
        , ( [ "--in-memory", "--read-only" ]
          , shouldParseAppConfiguration $ defaultConfiguration
            { chainProducer = ReadOnlyReplica
            }
          )
        , (  [ "--in-memory", "--read-only", "foo" ]
          , shouldFail
          )
        , ( defaultArgs ++ [ "--read-only" ]
          , shouldFail
          )
        , ( defaultArgs' ++ [ "--read-only" ]
          , shouldFail
          )
        , ( defaultArgs'' ++ [ "--read-only" ]
          , shouldFail
          )
        ]
        ++
        [ ( defaultArgs ++ [ "--log-level", str ]
          , shouldParseTracersConfiguration tracers
          )
        | ( str, tracers) <-
            [ ( "Debug",   defaultTracersDebug   )
            , ( "debug",   defaultTracersDebug   )
            , ( "Info",    defaultTracersInfo    )
            , ( "info",    defaultTracersInfo    )
            , ( "Notice",  defaultTracersNotice  )
            , ( "notice",  defaultTracersNotice  )
            , ( "Warning", defaultTracersWarning )
            , ( "warning", defaultTracersWarning )
            , ( "Error",   defaultTracersError   )
            , ( "error",   defaultTracersError   )
            , ( "Off",     defaultTracersOff     )
            , ( "off",     defaultTracersOff     )
            ]
        ]
        ++
        [ ( defaultArgs ++ [ "--log-level-http-server", "Notice" ]
          , shouldParseTracersConfiguration $ defaultTracersInfo
            { tracerHttp = Const (Just Notice) }
          )
        , ( defaultArgs ++
            [ "--log-level-database", "Debug"
            , "--log-level-consumer", "Warning"
            , "--log-level-garbage-collector", "Error"
            ]
          , shouldParseTracersConfiguration $ defaultTracersInfo
            { tracerDatabase  = Const (Just Debug)
            , tracerConsumer = Const (Just Warning)
            , tracerGardener = Const (Just Error)
            }
          )
        , ( defaultArgs ++
            [ "--log-level", "Error"
            , "--log-level-configuration", "Debug"
            ]
          , shouldFail
          )
        , ( [ "copy", "--from", "foo", "--into", "bar" ]
          , flip shouldBe $ Right $ Copy "foo" "bar" $ fromList []
          )
        , ( [ "copy", "--from", "foo", "--into", "bar", "--match", "*" ]
          , flip shouldBe $ Right $ Copy "foo" "bar" $ fromList
                [ MatchAny IncludingBootstrap
                ]
          )
        , ( [ "copy", "--from", "foo", "--into", "bar", "--match", "*", "--match", "*/*" ]
          , flip shouldBe $ Right $ Copy "foo" "bar" $ fromList
                [ MatchAny IncludingBootstrap
                , MatchAny OnlyShelley
                ]
          )
        , ( [ "copy", "--into", "bar" ]
          , shouldFail
          )
        , ( [ "copy", "--from", "foo" ]
          , shouldFail
          )
        , ( [ "version" ], flip shouldBe $ Right Version )
        , ( [ "-v" ], flip shouldBe $ Right Version )
        , ( [ "--version" ], flip shouldBe $ Right Version )
        , ( [ "--help" ]
          , flip shouldSatisfy $ isLeftWith $ \help ->
            help `deepseq` ("Usage:" `isInfixOf` help)
          )
        , ( [ "-h" ]
          , flip shouldSatisfy $ isLeftWith $ \help ->
            help `deepseq` ("Usage:" `isInfixOf` help)
          )
        ]

--
-- Helper
--

defaultConfiguration :: Configuration
defaultConfiguration = parseOptionsPure defaultArgs
    & either (error . toText) (\(Run cfg _) -> cfg)

defaultTracersOff :: Tracers IO 'MinSeverities
defaultTracersOff = defaultTracers Nothing

defaultTracersDebug :: Tracers IO 'MinSeverities
defaultTracersDebug = defaultTracers (Just Debug)

defaultTracersInfo :: Tracers IO 'MinSeverities
defaultTracersInfo = defaultTracers (Just Info)

defaultTracersNotice :: Tracers IO 'MinSeverities
defaultTracersNotice = defaultTracers (Just Notice)

defaultTracersWarning :: Tracers IO 'MinSeverities
defaultTracersWarning = defaultTracers (Just Warning)

defaultTracersError :: Tracers IO 'MinSeverities
defaultTracersError = defaultTracers (Just Error)

defaultArgs :: [String]
defaultArgs =
    [ "--node-socket", "./node.socket"
    , "--node-config", "./node.config"
    , "--in-memory"
    ]

defaultArgs' :: [String]
defaultArgs' =
    [ "--ogmios-host", "localhost"
    , "--ogmios-port", "1337"
    , "--in-memory"
    ]

defaultArgs'' :: [String]
defaultArgs'' =
    [ "--hydra-host", "localhost"
    , "--hydra-port", "4001"
    , "--in-memory"
    ]

shouldParseAppConfiguration
    :: Configuration
    -> (Either String Command-> Expectation)
shouldParseAppConfiguration cfg =
    flip shouldBe (Right (Run cfg defaultTracersInfo))

shouldParseTracersConfiguration
    :: Tracers IO 'MinSeverities
    -> (Either String Command -> Expectation)
shouldParseTracersConfiguration tracers =
    flip shouldBe (Right (Run defaultConfiguration tracers))

shouldFail :: (Either String Command) -> Expectation
shouldFail = flip shouldSatisfy isLeft

isLeftWith :: (err -> Bool) -> Either err result -> Bool
isLeftWith predicate = \case
    Left e -> predicate e
    Right{} -> False
