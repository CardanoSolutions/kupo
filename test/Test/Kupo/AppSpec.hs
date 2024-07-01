--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Kupo.AppSpec where

import Kupo.Prelude

import Control.Concurrent.STM
    ( STM
    , atomically
    )
import Control.Concurrent.STM.TChan
    ( TChan
    , cloneTChan
    , dupTChan
    , newTChanIO
    , readTChan
    , tryReadTChan
    , writeTChan
    )
import Control.Concurrent.STM.TVar
    ( newTVarIO
    , readTVar
    , writeTVar
    )
import Control.Monad.Class.MonadAsync
    ( link
    )
import GHC.Generics
    ( Generic1
    )
import Kupo
    ( kupoWith
    , newEnvironment
    , runWith
    )
import Kupo.App
    ( ChainSyncClient
    )
import Kupo.App.Mailbox
    ( Mailbox
    , newMailbox
    , putHighFrequencyMessage
    , putIntermittentMessage
    )
import Kupo.Control.MonadAsync
    ( MonadAsync (..)
    )
import Kupo.Control.MonadDelay
    ( threadDelay
    )
import Kupo.Control.MonadLog
    ( configureTracers
    , defaultTracers
    , nullTracer
    )
import Kupo.Control.MonadThrow
    ( throwIO
    )
import Kupo.Control.MonadTime
    ( DiffTime
    )
import Kupo.Data.Cardano
    ( BinaryData
    , BlockNo (..)
    , DatumHash
    , Input
    , Metadata
    , MetadataHash
    , Output
    , OutputReference
    , Point
    , Script
    , ScriptHash
    , SlotNo (..)
    , Tip
    , binaryDataToJson
    , datumHashToText
    , distanceToTip
    , getBinaryData
    , getDatum
    , getOutputIndex
    , getPoint
    , getPointSlotNo
    , getScript
    , getTipSlotNo
    , getTransactionId
    , hashBinaryData
    , hashDatum
    , hashScript
    , metadataHashToText
    , pattern BlockPoint
    , pattern GenesisPoint
    , pattern GenesisTip
    , pattern Tip
    , scriptHashToText
    , scriptToJson
    , slotNoToText
    , transactionIdToText
    , withReferences
    )
import Kupo.Data.ChainSync
    ( ForcedRollbackHandler
    )
import Kupo.Data.Configuration
    ( ChainProducer (..)
    , Configuration (..)
    , DatabaseLocation (..)
    , DeferIndexesInstallation (..)
    , InputManagement (..)
    , LongestRollback (..)
    , Since (..)
    , mailboxCapacity
    )
import Kupo.Data.FetchBlock
    ( FetchBlockClient
    )
import Kupo.Data.Http.GetCheckpointMode
    ( GetCheckpointMode (..)
    )
import Kupo.Data.Http.StatusFlag
    ( StatusFlag (..)
    )
import Kupo.Data.Ogmios
    ( NextBlockResponse (..)
    )
import Kupo.Data.PartialBlock
    ( PartialBlock (..)
    , PartialTransaction (..)
    )
import Kupo.Data.Pattern
    ( MatchBootstrap (..)
    , Pattern (..)
    , Result (..)
    )
import Network.HTTP.Client
    ( defaultManagerSettings
    , newManager
    )
import Network.WebSockets
    ( ConnectionException (..)
    )
import Test.Hspec
    ( Spec
    , runIO
    )
import Test.Hspec.QuickCheck
    ( prop
    )
import Test.Kupo.App.Http.Client
    ( HttpClient (..)
    , newHttpClientWith
    )
import Test.Kupo.Data.Generators
    ( genBinaryData
    , genDatumHash
    , genHeaderHash
    , genInputManagement
    , genMetadata
    , genNonGenesisPoint
    , genOutput
    , genOutputReference
    , genScript
    , genScriptHash
    , genTransactionId
    , generateWith
    )
import Test.QuickCheck
    ( Gen
    , arbitrary
    , choose
    , counterexample
    , elements
    , forAll
    , frequency
    , label
    , oneof
    , sized
    )
import Test.QuickCheck.Monadic
    ( assert
    , monadicIO
    , monitor
    , run
    )
import Test.QuickCheck.Property
    ( withMaxSuccess
    )
import Test.StateMachine
    ( CommandNames
    , Concrete
    , GenSym
    , Logic (..)
    , Reason (..)
    , StateMachine (..)
    , Symbolic
    , checkCommandNames
    , forAllCommands
    , runCommands
    , (.//)
    , (.==)
    )
import Test.StateMachine.Types
    ( Commands (..)
    , getCommand
    , newCounter
    , runGenSym
    )

import qualified Data.Aeson.Encoding as Json
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified GHC.Show as Show
import qualified Prelude
import qualified Test.StateMachine.Types.Rank2 as Rank2

varStateMachineIterations :: String
varStateMachineIterations = "KUPO_STATE_MACHINE_ITERATIONS"

spec :: Spec
spec = do
    manager <- runIO (newManager defaultManagerSettings)

    chan <- runIO newTChanIO

    maxSuccess <- maybe 30 Prelude.read
        <$> runIO (lookupEnv varStateMachineIterations)

    prop "State-Machine" $ withMaxSuccess maxSuccess $
      forAll genInputManagement $ \inputManagement -> do
        forAll genServerPort $ \serverPort -> do
           let httpClient = newHttpClientWith manager (serverHost, serverPort) (\_ -> pure ())
           let stateMachine = StateMachine
                initModel
                transition
                (precondition longestRollback)
                postcondition
                Nothing
                (generator inputManagement)
                shrinker
                (semantics garbageCollectionInterval httpClient chan)
                mock
                (cleanup chan)
           forAllCommands stateMachine Nothing $ \cmds -> monadicIO $ do
              let config = Configuration
                      { chainProducer = CardanoNode -- NOTE: unused, but must be different than ReadOnlyReplica
                          { nodeSocket = "/dev/null"
                          , nodeConfig = "/dev/null"
                          }
                      , databaseLocation = InMemory Nothing
                      , serverHost
                      , serverPort
                      , since = Just (SincePoint GenesisPoint)
                      , patterns = fromList [MatchAny IncludingBootstrap]
                      , inputManagement
                      , longestRollback
                      , garbageCollectionInterval
                      , deferIndexes
                      }
              env <- run (newEnvironment config)
              producer <- run (newMockProducer httpClient <$> atomically (dupTChan chan))
              fetchBlock <- run (newMockFetchBlock <$> atomically (dupTChan chan))
              let kupo = kupoWith tracers producer fetchBlock `runWith` env
              asyncId <- run (async kupo)
              run $ link asyncId
              (_hist, model, res) <- runCommands stateMachine cmds
              run $ cancel asyncId

              -- TODO: Check coverage using the history and label some interesting
              -- test scenarios that are relevant to cover.

              monitor (label (show inputManagement))
              monitor (checkCommandNames cmds)
              monitor $ counterexample $ toString $ unlines
                [ T.intercalate "\n -"
                   ("== Commands =="
                   : (show . getCommand <$> unCommands cmds)
                   )
                , ""
                , "== Model =="
                , show model
                , ""
                , "== Assertion =="
                , show res
                ]
              assert (res == Ok)
  where
    serverHost = "127.0.0.1"
    longestRollback = 10
    garbageCollectionInterval = 0.4
    deferIndexes = InstallIndexesIfNotExist
    tracers = configureTracers (defaultTracers Nothing) nullTracer
    genServerPort = sized $ \n -> do
        i <- arbitrary
        pure (1024 + n + i)

--------------------------------------------------------------------------------
---- Events / Respone
--
-- This is the interface used to simulate external events affecting the
-- application. Events are mostly of two kinds:
--
-- - Blockchain events (roll-forward / roll-back)
-- - HTTP Client requests
--
-- This allows to let the quickcheck machinery generate arbitrary on-chain
-- executions traces and interleave them with various HTTP client requests.
-- After each client requests, we check that some post-conditions do indeed
-- apply (e.g. after getting the tip, we should.. actually receive the tip!).

data Event (r :: Type -> Type)
    = DoRollForward !Tip !PartialBlock
    | DoRollBackward !Tip !Point
    | LoseConnection
    | GetMostRecentCheckpoint
    | GetPreviousCheckpoint !SlotNo
    | GetUtxo
    | GetDatumByHash !DatumHash
    | GetScriptByHash !ScriptHash
    | GetMetadataBySlotNo !SlotNo
    | Pause
        -- ^ Pauses makes the overall state-machine run a lot longer, but they
        -- allow to let the internal garbage-collector to run and possibly
        -- wreak havoc in the application. If something fails after a pause,
        -- then it certainly is due to the garbage collector kicking in.
    deriving stock (Eq, Generic1)
    deriving anyclass (Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

instance Show (Event r) where
    show = \case
        DoRollForward _ blk ->
            toString $ "(DoRollForward " <> showPartialBlock ", " blk <> ")"
        DoRollBackward _ pt ->
            toString $ "(DoRollBackward " <> showPoint pt <> ")"
        LoseConnection ->
            "LoseConnection"
        GetMostRecentCheckpoint ->
            "GetMostRecentCheckpoint"
        GetPreviousCheckpoint sl ->
            toString $ "(GetPreviousCheckpoint " <> showSlotNo sl <> ")"
        GetUtxo ->
            "GetUtxo"
        GetDatumByHash hash ->
            toString $ "(GetDatumByHash " <> showDatumHash hash <> ")"
        GetScriptByHash hash ->
            toString $ "(GetScriptByHash " <> showScriptHash hash <> ")"
        GetMetadataBySlotNo sl ->
            toString $ "(GetMetadata " <> showSlotNo sl <> ")"
        Pause ->
            "Pause"

data Response (r :: Type -> Type)
    = Unit !()
    | Checkpoint !(Maybe Point)
    | Utxo !(Set OutputReference)
    | DatumByHash !(Maybe BinaryData)
    | ScriptByHash !(Maybe Script)
    | MetadataBySlotNo ![(MetadataHash, Metadata)]
    deriving stock (Eq, Generic1)
    deriving anyclass (Rank2.Foldable)

instance Show (Response r) where
    show = \case
        Unit () ->
            "()"
        Checkpoint pt ->
            toString $ "(Checkpoint " <> maybe "NULL" showPoint pt <> ")"
        Utxo outRefs ->
            toString $ "(Utxo " <> T.intercalate "," (showOutputReference <$> Set.toList outRefs) <> ")"
        DatumByHash bin ->
            toString $ "(DatumByHash " <> showBinaryData bin <> ")"
        ScriptByHash script ->
            toString $ "(ScriptByHash " <> showScript script <> ")"
        MetadataBySlotNo metadata ->
            toString $ "(MetadataBySlotNo [" <> T.intercalate "," (showMetadataHash . fst <$> metadata) <> "])"

--------------------------------------------------------------------------------
---- Model
--
-- Here we define the application model against which we want to compare the
-- real application. The model is a simple, reasonable view of the system that a
-- client could have.
--
-- The major trick being that chain events are part of the possible commands
-- that our model can see. Thus, we can transition how we expect the model to
-- evolve irrespective of what really happens in the application. Ultimately,
-- they should both match.
--

data Model (r :: Type -> Type) = Model
    { networkTip :: !Tip
    , currentChain :: ![PartialBlock]
    , spentOutputReferences :: !(Set OutputReference)
    , unspentOutputReferences :: !(Set OutputReference)
    , mempool :: ![PartialTransaction]
    } deriving stock (Generic)

instance Show (Model r) where
    show model@Model{..} =
        toString $ T.unlines
            [ "network tip:    \n    "
                <> showTip networkTip
            , "Utxo set: \n    "
                <> T.intercalate "\n    "
                (showOutputReference <$> Set.toList unspentOutputReferences)
            , "Known datums: \n    "
                <> T.intercalate "\n    "
                (showDatumHash <$> Map.keys (knownBinaryData model))
            , "chain: \n    "
                <> T.intercalate "\n    "
                (showPartialBlock "\n        " <$> reverse currentChain)
            ]

initModel :: Model r
initModel = Model
    { networkTip =
        GenesisTip
    , currentChain =
        []
    , spentOutputReferences =
        Set.empty
    , unspentOutputReferences =
        genesisUtxo
    , mempool =
        []
    }

knownBinaryData :: Model r -> Map DatumHash BinaryData
knownBinaryData =
    foldMap collectDatumInBlock . currentChain

knownScripts :: Model r -> Map ScriptHash Script
knownScripts =
    foldMap collectScriptInBlock . currentChain

-- | A set of initial utxo to start from. This is pre-defined to allow filtering
-- it out. Kupo has no notion of genesis utxo anyway. This allows to create
-- valid sequences of blocks using utxos produced by previous transactions.
genesisUtxo :: Set OutputReference
genesisUtxo =
    let
        numberOfEntries = 5
        seed = 42
    in
        Set.fromList
            $ generateWith seed
            $ replicateM numberOfEntries genOutputReference

-- Note that while pre-conditions may seem redundant with the generators below,
-- they are crucial for shrinking a counterexample into a still valid sequence
-- of event. the shrinker being quite "dumb", it removes actions from the
-- sequence at random which may lead to an invalid state that generators would
-- have never generated in the first place (e.g. what if we remove a
-- 'RollForward' event during shrinking that makes a 'RollBackward' now
-- irrelevant?).
precondition
    :: LongestRollback
    -> Model Symbolic
    -> Event Symbolic
    -> Logic
precondition (LongestRollback k) model = \case
    DoRollForward _ block ->
        let currentChainTip = case currentChain model of
                [] -> GenesisPoint
                pt:_ -> blockPoint pt
         in
            getPointSlotNo (blockPoint block) .== next (getPointSlotNo currentChainTip)
    DoRollBackward _ point ->
        let
            d = distanceToTip (networkTip model) (getPointSlotNo point)
         in
            case blockPoint <$> currentChain model of
                [] -> Bot
                points@(tip:_) ->
                    if point `elem` points && point /= tip && d <= k
                    then Top
                    else Bot
    LoseConnection ->
        Top
    GetPreviousCheckpoint{} ->
        Top
    GetMostRecentCheckpoint ->
        Top
    GetUtxo ->
        Top
    GetDatumByHash{} ->
        Top
    GetScriptByHash{} ->
        Top
    GetMetadataBySlotNo{} ->
        Top
    Pause ->
        Top

-- | Apply a command to the current model to get the next one.
transition
    :: Model r
    -> Event r
    -> Response r
    -> Model r
transition model cmd _res =
    case cmd of
        DoRollForward nextTip block ->
            let
                ( nextSpentOutputReferences, nextUnspentOutputReferences ) =
                    applyBlock block
                        ( spentOutputReferences model
                        , unspentOutputReferences model
                        )
             in
                model
                    { networkTip =
                        nextTip
                    , currentChain =
                        block : currentChain model
                    , spentOutputReferences =
                        nextSpentOutputReferences
                    , unspentOutputReferences =
                        nextUnspentOutputReferences
                    , mempool =
                        filter
                            (not . hasConflictingInput nextSpentOutputReferences)
                            (mempool model)
                    }
        DoRollBackward nextTip point ->
            let
                isRolledBack =
                    (/= point) . blockPoint
                nextChain =
                    dropWhile isRolledBack (currentChain model)
                dropped =
                    takeWhile isRolledBack (currentChain model)
                (spentOutputReferences, unspentOutputReferences) =
                    foldr applyBlock (Set.empty, genesisUtxo) nextChain
             in
                model
                    { networkTip =
                        nextTip
                    , currentChain =
                        nextChain
                    , spentOutputReferences
                    , unspentOutputReferences
                    , mempool =
                        filter
                            (hasConflictingInput unspentOutputReferences)
                            (mempool model <> foldMap blockBody (reverse dropped))
                    }
        LoseConnection ->
            model
        GetMostRecentCheckpoint ->
            model
        GetPreviousCheckpoint{} ->
            model
        GetUtxo ->
            model
        GetDatumByHash{} ->
            model
        GetScriptByHash{} ->
            model
        GetMetadataBySlotNo{} ->
            model
        Pause ->
            model

applyBlock
    :: PartialBlock
    -> (Set OutputReference, Set OutputReference)
    -> (Set OutputReference, Set OutputReference)
applyBlock block (spentRefs, unspentRefs) =
    ( spentRefs <> spent
    , (unspentRefs <> unspent) Set.\\ spent
    )
  where
    spent =
        foldMap (Set.fromList . inputs) (blockBody block)
    unspent =
        foldMap (Set.fromList . fmap fst . outputs) (blockBody block)

hasConflictingInput :: Set Input -> PartialTransaction -> Bool
hasConflictingInput spentInputs tx =
    not (null (txInputs `Set.intersection` spentInputs))
  where
    txInputs = Set.fromList (inputs tx)

-- | This is where the "checks" happen. Indeed, we can compare the server's
-- outputs with what we would expect from our model. Because we don't make any
-- use of references in Kupo, this is equivalent to the `mock` implementation
-- though we could also add some extra post-conditions here.
postcondition
    :: Model r
    -> Event r
    -> Response r
    -> Logic
postcondition model cmd res =
    let
        (res', _) = runGenSym (mock model cmd) newCounter
     in
        Boolean (res == res') .// ("Real " <> show res <> " /= Model " <> show res')

--------------------------------------------------------------------------------
---- Generators
--
-- Generating arbitrary data to fuel the state-machine model. The strategy for
-- generating chain activity is as follows:
--
-- - For rolling back, we simply pick an arbitrary point in the past if any.
-- - For rolling forward, we generating a new continuing block using either
--   arbitrarily generated transactions or, transactions that were dropped from
--   a previous rollback.
--
-- This allows to simulate a somewhat realistic network activity where
-- transactions may in some cases be rolled back, and either fully dropped from
-- the chain or re-introduced later. We call these transactions in flux
-- "mempool".

generator
    :: InputManagement
    -> Model Symbolic
    -> Maybe (Gen (Event Symbolic))
generator inputManagement model =
    case currentChain model of
        [] -> Just $ frequency
            [ (1, pure GetMostRecentCheckpoint)
            , (1, pure GetUtxo)
            , (1, GetDatumByHash <$> genOrSelectDatum inputManagement model)
            , (1, GetScriptByHash <$> genOrSelectScript model)
            , (5, DoRollForward
                <$> genContinuingTip (networkTip model) GenesisPoint
                <*> genContinuingBlock
                        (unspentOutputReferences model)
                        (mempool model)
                        GenesisPoint
              )
            ]
        blocks@(tip:_) -> Just $ frequency
            [ (3, pure GetMostRecentCheckpoint)
            , (3, GetPreviousCheckpoint <$> oneof
                [ getPointSlotNo <$> genNonGenesisPoint
                , elements (getPointSlotNo . blockPoint <$> blocks)
                ]
              )
            , (5, pure GetUtxo)
            , (5, GetDatumByHash <$> genOrSelectDatum inputManagement model)
            , (3, GetScriptByHash <$> genOrSelectScript model)
            , (3, GetMetadataBySlotNo . SlotNo <$> (elements [0 .. unSlotNo (getPointSlotNo (blockPoint tip))]))
            , (15, DoRollForward
                <$> genContinuingTip (networkTip model) (blockPoint tip)
                <*> genContinuingBlock
                        (unspentOutputReferences model)
                        (mempool model)
                        (blockPoint tip)
              )
            , (3, DoRollBackward
                <$> genContinuingTip (networkTip model) (blockPoint tip)
                <*> selectPastPoint (blockPoint <$> blocks)
              )
            , (1, pure Pause)
            , (1, pure LoseConnection)
            ]

-- | Generate a new network tip based on the current network tip and a local
-- tip / point. When the local tip is far behind the network tip, we change the
-- network tip less often. This corresponds to a client catching up after a
-- rollback, while the network is moving forward -- though not fast.
genContinuingTip :: Tip -> Point -> Gen Tip
genContinuingTip networkTip currentTip
    | getTipSlotNo networkTip > getPointSlotNo currentTip =
        frequency
            [ (1, genTipAfter (getTipSlotNo networkTip))
            , (5, pure networkTip )
            ]
    | otherwise =
        genTipAfter (getPointSlotNo currentTip)

-- | Generate a tip right after the given slot.
genTipAfter :: SlotNo -> Gen Tip
genTipAfter slot = Tip (next slot)
    <$> genHeaderHash
    <*> pure (coerce $ next slot)

-- | Generate a valid sublist of transactions from a given sublist. It is not
-- okay to use Quickcheck's 'sublistOf' because some transactions depends on
-- outputs of previous transactions. Thus, discarding a previous transaction in
-- the list may invalid a transaction further up.
--
-- The final state represents the final utxo set obtained by applying those
-- transactions to an initial utxo state.
genTransactionSublist
    :: [PartialTransaction]
    -> StateT (Set OutputReference) Gen [PartialTransaction]
genTransactionSublist = \case
    [] -> pure []
    tx:rest -> do
        let inps = Set.fromList (inputs tx)
        let outs = Set.fromList (fst <$> outputs tx)
        utxo <- get
        if (inps `Set.isSubsetOf` utxo)
        then do
            put ((utxo <> outs) Set.\\ inps)
            xs <- genTransactionSublist rest
            pure (tx:xs)
        else do
            genTransactionSublist rest

-- | Generate a block right after the given point, picking from the given utxo
-- set.
genContinuingBlock
    :: Set OutputReference
    -> [PartialTransaction]
    -> Point
    -> Gen PartialBlock
genContinuingBlock utxo txs previousTip = do
    blockPoint <- BlockPoint (next (getPointSlotNo previousTip)) <$> genHeaderHash
    (txs', utxo') <- runStateT (genTransactionSublist txs) utxo
    blockBody <- (txs' <>) <$> evalStateT genPartialTransactions utxo'
    pure PartialBlock { blockPoint, blockBody }

-- | Generate a list of transactions using and modifying a given utxo. The
-- generation stops when there's no more utxos or, arbitrarily (1 chance out of
-- 2). This means that long list are exponentially less likely that "short
-- lists".
genPartialTransactions
    :: StateT (Set OutputReference) Gen [PartialTransaction]
genPartialTransactions = do
    tx <- genPartialTransaction
    n <- gets Set.size
    more <- if n > 0 then lift (elements [True, False]) else pure False
    txs <- if more then genPartialTransactions else pure []
    pure (tx : txs)

-- | Generate a completely arbitrary transaction, using utxos from an available
-- set and producing new ones. The utxo set is modified after each generation.
genPartialTransaction
    :: StateT (Set OutputReference) Gen PartialTransaction
genPartialTransaction = do
    id <- lift genTransactionId
    utxos <- get
    nInputs <- lift $ choose (1, min (Set.size utxos) maxNumberOfInputs)
    inputs <- replicateM nInputs (lift $ elements $ Set.toList utxos)
    nOutputs <- lift $ choose (1, maxNumberOfOutputs)
    outputs <- withReferences 0 id <$> replicateM nOutputs (lift genOutput)
    put ((utxos `Set.difference` Set.fromList inputs) <> Set.fromList (fmap fst outputs))
    nDatums <- lift $ oneof [pure 0, choose (1, nInputs)]
    datums <-
        replicateM nDatums (lift genBinaryData)
        & fmap (foldMap (\v -> Map.singleton (hashBinaryData v) v))
    nScripts <- lift $ elements [0, 1, 2]
    scripts <-
        replicateM nScripts (lift genScript)
        & fmap (foldMap (\v -> Map.singleton (hashScript v) v))
    metadata <- lift $ oneof [pure Nothing, Just <$> genMetadata]
    pure PartialTransaction { id, inputs, outputs, datums, scripts, metadata }
  where
    maxNumberOfInputs = 3
    maxNumberOfOutputs = 3

-- | Generate an arbitrary datum hash (for which we expect no associated binary
-- data to be known) or a known datum.
--
-- For known datums, we only consider datums hashes that are present in matched
-- inputs (although they may have been resolved or not).
--
-- Kupo does indeed keep track of all possible datums, even those for which
-- there's no (or no longer) a matching input. They are eventually
-- garbage-collected and thus it is hard to build any kind of deterministic
-- assumption on those. What we can and want to enforce however is that any
-- _known datum_ (i.e. seen in a transaction output, either inlined or hash
-- only) can be resolved if fully known.
genOrSelectDatum :: InputManagement -> Model r -> Gen DatumHash
genOrSelectDatum inputManagement model =
    let
        xs  = currentChain model
            & foldMap collectKnownDatumHashInBlock
            & case inputManagement of
                MarkSpentInputs ->
                    fmap snd
                RemoveSpentInputs ->
                    mapMaybe (\(outRef, dat) -> do
                        guard (outRef `Set.notMember` spentOutputReferences model)
                        pure dat
                    )
     in
        oneof $ genDatumHash : [ elements xs | not (null xs) ]

-- | Generate an arbitrary script hash (for which we expect no associated script
-- to be known or a known script)
genOrSelectScript :: Model r -> Gen ScriptHash
genOrSelectScript model =
    let
        xs = Map.keys (knownScripts model)
     in
        oneof $ genScriptHash : [ elements xs | not (null xs) ]

-- | Select a point from the list, after the first one, in order. The
-- probability of getting a point far in the fast is decreasing exponentially
-- (1/2).
selectPastPoint
    :: HasCallStack
    => [Point]
    -> Gen Point
selectPastPoint = \case
    [] ->
        error "selectPoint: empty list."
    _:rest ->
        loop rest
  where
    loop = \case
        [] ->
            pure GenesisPoint
        pt:rest ->
            oneof [ pure pt, loop rest ]

-- TODO: Better shrinker for commands. Note that this could be potentially quite
-- complicated as pruning one block for instance may invalidate all sub-sequent
-- commands after that block.
shrinker :: Model Symbolic -> Event Symbolic -> [Event Symbolic]
shrinker _ _ = []

--------------------------------------------------------------------------------
---- Semantics
--
-- This is actually where we get to interpret effects on the 'real' application.
--
-- With a subtle difference: we mock the network part.
--
-- Using a mock chain producer, we use generated commands to drive the
-- application. In fact, if we remove (or mock) the network, then Kupo becomes a
-- relatively closed system. While there are internal threads and workers, they
-- are mostly NoOp unless there's also a network activity.

cleanup
    :: TChan a
    -> Model Concrete
    -> IO ()
cleanup chan _ = do
    void (atomically $ flushTChan chan)

-- | Actual 'real world' interpretation of each commands. Note that, for
-- roll-forward / roll-backward, we want to make sure that the events have been
-- fully processed before we move to the next command otherwise everything
-- becomes unpredictable. We do so by simply comparing the most recent
-- checkpoint before and after those events.
--
-- For the rest, we use an HTTP client doing real requests to the server.
semantics
    :: DiffTime
    -> HttpClient IO
    -> TChan (Either ConnectionException NextBlockResponse)
    -> Event Concrete
    -> IO (Response Concrete)
semantics pause HttpClient{..} chan = \case
    DoRollForward tip block -> do
        cp <- getMostRecentCheckpoint
        atomically $ writeTChan chan (Right $ RollForward tip block)
        fmap Unit $ waitUntilM $ do
            cp' <- getMostRecentCheckpoint
            pure (cp' > cp)
    DoRollBackward tip point -> do
        cp <- getMostRecentCheckpoint
        atomically $ writeTChan chan (Right $ RollBackward tip point)
        fmap Unit $ waitUntilM $ do
            cp' <- getMostRecentCheckpoint
            pure (cp' < cp)
    LoseConnection ->
        Unit <$> atomically (writeTChan chan (Left ConnectionClosed))
    GetMostRecentCheckpoint -> do
        Checkpoint . Just <$> getMostRecentCheckpoint
    GetPreviousCheckpoint sl -> do
        Checkpoint <$> getCheckpointBySlot GetCheckpointClosestAncestor sl
    GetUtxo -> do
        Utxo . foldMap (Set.singleton . fst . outputReference) <$> getAllMatches OnlyUnspent
    GetDatumByHash hash ->
        DatumByHash <$> lookupDatumByHash hash
    GetScriptByHash hash ->
        ScriptByHash <$> lookupScriptByHash hash
    GetMetadataBySlotNo sl ->
        MetadataBySlotNo <$> lookupMetadataBySlotNo sl Nothing
    Pause -> do
        Unit <$> threadDelay pause
  where
    getMostRecentCheckpoint =
        listCheckpoints <&> \case
            [] -> GenesisPoint
            h:_ -> h

-- | Here we mock the server behavior using our model; this is used within the
-- generator itself, to advance the state-machine. Because the model is our
-- making, the mock should be trivial to implement.
mock
    :: Model r
    -> Event r
    -> GenSym (Response r)
mock model = \case
    DoRollForward{} ->
        pure (Unit ())
    DoRollBackward{} ->
        pure (Unit ())
    LoseConnection{} ->
        pure (Unit ())
    GetMostRecentCheckpoint ->
        pure $ Checkpoint $ case currentChain model of
            []  -> Just GenesisPoint
            h:_ -> Just (blockPoint h)
    GetPreviousCheckpoint sl ->
        let
            search = \case
                [] -> Nothing
                (blockPoint -> h):rest ->
                    if getPointSlotNo h <= sl
                    then Just h
                    else search rest
         in
            pure $ Checkpoint $ search (currentChain model)
    GetUtxo ->
        pure (Utxo (unspentOutputReferences model Set.\\ genesisUtxo))
    GetDatumByHash hash ->
        pure (DatumByHash (Map.lookup hash (knownBinaryData model)))
    GetScriptByHash hash ->
        pure (ScriptByHash (Map.lookup hash (knownScripts model)))
    GetMetadataBySlotNo sl ->
        case find ((== sl) . getPointSlotNo . getPoint) (currentChain model) of
            Nothing ->
                pure $ MetadataBySlotNo []
            Just blk ->
                pure $ MetadataBySlotNo (mapMaybe metadata (blockBody blk))

    Pause ->
        pure (Unit ())

-- | A mock chain producer that we plugged into Kupo. It is fully driven by a
-- bounded queue of events that are generated by the quickcheck machinery. It
-- does nothing more than passing information around in the mailbox.
newMockProducer
    :: HttpClient IO
    -> TChan (Either ConnectionException NextBlockResponse)
    -> (  (Point -> ForcedRollbackHandler IO -> IO ())
       -> Mailbox IO (Tip, PartialBlock) (Tip, Point)
       -> ChainSyncClient IO PartialBlock
       -> IO ()
       )
    -> IO ()
newMockProducer HttpClient{..} chan callback = do
    lastKnownTipVar <- newTVarIO GenesisPoint
    mailbox <- atomically (newMailbox mailboxCapacity)
    callback forcedRollbackCallback mailbox $ \_ -> \case
        requestedTip:_ -> \_ -> do
            lastKnownTip <- atomically (readTVar lastKnownTipVar)
            when (requestedTip /= lastKnownTip) $ do
                putStrLn $ "RequestedTip: " <> show requestedTip
                putStrLn $ "LastKnowntip: " <> show lastKnownTip
                fail "Tip out of sync"
            forever $ do
                result <- atomically $ readTChan chan >>= \case
                    Right (RollForward tip block) ->
                        Right <$> putHighFrequencyMessage mailbox (tip, block)
                    Right (RollBackward tip point) ->
                        Right <$> putIntermittentMessage mailbox (tip, point)
                    Left e -> do
                        pure (Left e)
                either
                    (\e -> do
                        atomically . writeTVar lastKnownTipVar =<< getMostRecentCheckpoint
                        throwIO e
                    )
                    return
                    result
        [] -> do
            const $ fail "Empty list requested for mock producer"
  where
    forcedRollbackCallback _point _handler =
        fail "Mock producer cannot force rollback."

    getMostRecentCheckpoint =
        listCheckpoints <&> \case
            [] -> GenesisPoint
            h:_ -> h

-- | Mock a request to the node which returns the block immediately following the given point.
newMockFetchBlock
    :: TChan (Either e NextBlockResponse)
    -> (FetchBlockClient IO PartialBlock -> IO ())
    -> IO ()
newMockFetchBlock chan callback =
    callback $ \point reply -> do
        let slotNo = next (getPointSlotNo point)
        blocks <- applyBlocks [] <$> atomically (cloneTChan chan >>= flushTChan)
        reply $ find ((== slotNo) . getPointSlotNo . blockPoint) blocks
  where
    applyBlocks :: [PartialBlock] -> [Either e NextBlockResponse] -> [PartialBlock]
    applyBlocks blocks = \case
        [] ->
            reverse blocks
        Left{}:rest ->
            applyBlocks blocks rest
        (Right (RollForward _ block)):rest ->
            applyBlocks (block:blocks) rest
        (Right (RollBackward _ point)):rest ->
            let blocks' = dropWhile ((/= point) . blockPoint) blocks
             in applyBlocks blocks' rest

--------------------------------------------------------------------------------
---- Pretty Printers
--
-- Some pretty printer to make counter-examples more bearable to read.

showPoint :: Point -> Text
showPoint =
    showSlotNo . getPointSlotNo

showTip :: Tip -> Text
showTip =
    showSlotNo . getTipSlotNo

showSlotNo :: SlotNo -> Text
showSlotNo =
    slotNoToText

showOutputReference :: OutputReference -> Text
showOutputReference o = mconcat
    [ show (getOutputIndex o)
    , "#"
    , T.take 8 (transactionIdToText (getTransactionId o))
    ]

showPartialBlock :: Text -> PartialBlock -> Text
showPartialBlock delimiter blk = mconcat
    [ "Block " <> showPoint (blockPoint blk)
    , delimiter
    , T.intercalate delimiter $ showPartialTransaction <$> (blockBody blk)
    , delimiter
    , "All datums: " <>
        if Map.null allDatums
        then "[]"
        else mconcat
            [ delimiter
            , T.intercalate delimiter
                [ showDatumHash k <> " ⇒ " <> showBinaryData (Just v)
                | (k, v) <- Map.toList allDatums
                ]
            ]
    ]
  where
    allDatums = collectDatumInBlock blk

showDatumHash :: DatumHash -> Text
showDatumHash =
    T.take 8 . datumHashToText

showPartialTransaction :: PartialTransaction -> Text
showPartialTransaction tx = mconcat
    [ T.intercalate ", " (showOutputReference <$> inputs tx)
    , " → "
    , T.take 8 $ transactionIdToText $ getTransactionId $ fst $ Prelude.head $ outputs tx
    , "{"
    , T.intercalate ", " (showOutput <$> outputs tx)
    , "}"
    , case metadata tx of
        Nothing ->
            " w/ no metadata"
        Just (hash, _) ->
            " w/ metadata = " <> showMetadataHash hash
    ]

showOutput :: (OutputReference, Output) -> Text
showOutput (outRef, out) =
    case hashDatum (getDatum out) of
        Nothing ->
            show (getOutputIndex outRef)
        Just dh ->
            show (getOutputIndex outRef) <> "+" <> showDatumHash dh

showBinaryData :: Maybe BinaryData -> Text
showBinaryData =
    showLongByteString (maybe Json.null_ binaryDataToJson)

showScriptHash :: ScriptHash -> Text
showScriptHash =
    T.take 8 . scriptHashToText

showScript :: Maybe Script -> Text
showScript =
    showLongByteString (maybe Json.null_ scriptToJson)

showMetadataHash :: MetadataHash -> Text
showMetadataHash =
    T.take 8 . metadataHashToText

showLongByteString :: (a -> Json.Encoding) -> a -> Text
showLongByteString toJsonEncoding =
    ellipse 20
    . T.drop 1
    . T.dropEnd 1
    . decodeUtf8
    . Json.encodingToLazyByteString
    . toJsonEncoding
  where
    ellipse n txt
        | T.length txt > n = T.take 20 txt <> "..."
        | otherwise = txt

--------------------------------------------------------------------------------
---- Helpers
--

-- | Flush the content of a channel, very much like
flushTChan :: TChan a -> STM [a]
flushTChan chan =
    tryReadTChan chan >>= \case
        Nothing ->
            pure []
        Just x -> do
            xs <- flushTChan chan
            pure (x : xs)

-- | Collect all resolved datums coming from either outputs (inline datums) or
-- transactions' witness set.
collectDatumInBlock :: PartialBlock -> Map DatumHash BinaryData
collectDatumInBlock =
    foldMap collectDatumInTransaction . blockBody
  where
    collectDatumInTransaction :: PartialTransaction -> Map DatumHash BinaryData
    collectDatumInTransaction tx =
        datums tx
        <>
        foldMap (collectDatumInOutput . snd) (outputs tx)

    collectDatumInOutput :: Output -> Map DatumHash BinaryData
    collectDatumInOutput o = do
        Map.fromList
            [ (k, v)
            | Just k <- [ hashDatum (getDatum o) ]
            , Just v <- [ getBinaryData  (getDatum o) ]
            ]

-- | Collect datum hashes that are seen in transaction outputs. This does not
-- collect datum hashes that are only seen in witness set.
collectKnownDatumHashInBlock :: PartialBlock -> [(OutputReference, DatumHash)]
collectKnownDatumHashInBlock =
    foldMap collectKnownDatumHashInTransaction . blockBody
  where
    collectKnownDatumHashInTransaction :: PartialTransaction -> [(OutputReference, DatumHash)]
    collectKnownDatumHashInTransaction tx =
        foldMap collectKnownDatumHashInOutput (outputs tx)

    collectKnownDatumHashInOutput :: (OutputReference, Output) -> [(OutputReference, DatumHash)]
    collectKnownDatumHashInOutput (ref, o) = do
        [ (ref, k) | Just k <- [ hashDatum (getDatum o) ] ]

-- | Collect all resolved scripts coming from outputs (reference scripts),
-- auxiliary data or transactions' witness set.
collectScriptInBlock :: PartialBlock -> Map ScriptHash Script
collectScriptInBlock =
    foldMap collectScriptInTransaction . blockBody
  where
    collectScriptInTransaction :: PartialTransaction -> Map ScriptHash Script
    collectScriptInTransaction tx =
        scripts tx
        <>
        foldMap (collectScriptInOutput . snd) (outputs tx)

    collectScriptInOutput :: Output -> Map ScriptHash Script
    collectScriptInOutput o = do
        Map.fromList
            [ (k, v)
            | Just k <- [ hashScript <$> getScript o ]
            , Just v <- [ getScript o ]
            ]
