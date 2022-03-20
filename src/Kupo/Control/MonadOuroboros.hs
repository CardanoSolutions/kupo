--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kupo.Control.MonadOuroboros
    ( -- * MonadOuroboros
      MonadOuroboros (..)

      -- * Health Reporting
    , ConnectionStatusToggle (..)

      -- * Types
    , NetworkMagic (..)
    , EpochSlots (..)
    , NodeToClientVersion (..)
    , IntersectionNotFoundException (..)

      -- * Tracer
    , TraceChainSync (..)
    ) where

import Kupo.Prelude

import Cardano.Chain.Slotting
    ( EpochSlots (..) )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Slotting.Slot
    ( SlotNo )
import Control.Exception.Safe
    ( IOException, isAsyncException )
import Control.Monad.Class.MonadThrow
    ( MonadCatch (..), MonadThrow (..) )
import Control.Monad.Class.MonadTimer
    ( threadDelay )
import Control.Tracer
    ( Tracer, traceWith )
import Control.Tracer
    ( nullTracer )
import Data.List
    ( isInfixOf )
import Data.Map.Strict
    ( (!) )
import Data.Severity
    ( HasSeverityAnnotation (..), Severity (..) )
import Data.Time.Clock
    ( DiffTime )
import Ouroboros.Consensus.Byron.Ledger.Config
    ( CodecConfig (..) )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock )
import Ouroboros.Consensus.Cardano.Block
    ( CodecConfig (..) )
import Ouroboros.Consensus.HardFork.Combinator.Block
    ()
import Ouroboros.Consensus.Network.NodeToClient
    ( ClientCodecs, Codecs' (..), clientCodecs )
import Ouroboros.Consensus.Node.NetworkProtocolVersion
    ( SupportedNetworkProtocolVersion (..) )
import Ouroboros.Consensus.Shelley.Ledger.Config
    ( CodecConfig (..) )
import Ouroboros.Network.Block
    ( Point (..), StandardHash, Tip (..) )
import Ouroboros.Network.Driver.Simple
    ( runPipelinedPeer )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolLimits (..)
    , MiniProtocolNum (..)
    , MuxPeer (..)
    , OuroborosApplication (..)
    , RunMiniProtocol (..)
    )
import Ouroboros.Network.NodeToClient
    ( NetworkConnectTracers (..)
    , NodeToClientVersion (..)
    , NodeToClientVersionData (..)
    , connectTo
    , localSnocket
    , withIOManager
    )
import Ouroboros.Network.Point
    ( WithOrigin (..) )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined (..), chainSyncClientPeerPipelined )
import Ouroboros.Network.Protocol.Handshake.Version
    ( combineVersions, simpleSingletonVersions )
import Ouroboros.Network.Snocket
    ( Snocket (..) )
import System.IO.Error
    ( isDoesNotExistError )

class MonadOuroboros (m :: Type -> Type) where
    type Block m :: Type
    withChainSyncServer
        :: IsBlock (Block m)
        => Tracer m TraceChainSync
        -> ConnectionStatusToggle m
        -> [NodeToClientVersion]
        -> NetworkMagic
        -> EpochSlots
        -> FilePath
        -> ChainSyncClientPipelined (Block m) (Point (Block m)) (Tip (Block m)) IO ()
        -> m ()

data ConnectionStatusToggle m = ConnectionStatusToggle
    { toggleConnected :: m ()
    , toggleDisconnected :: m ()
    }

type IsBlock block =
    ( StandardHash block
    , Typeable block
    )

-- | Exception thrown when creating a chain-sync client from an invalid list of
-- points.
data IntersectionNotFoundException = IntersectionNotFoundException
    { requestedPoints :: [WithOrigin SlotNo]
        -- ^ Provided points for intersection.
    , tip :: WithOrigin SlotNo
        -- ^ Current known tip of the chain.
    } deriving (Show)
instance Exception IntersectionNotFoundException

instance MonadOuroboros IO where
    type Block IO = CardanoBlock StandardCrypto
    withChainSyncServer tr ConnectionStatusToggle{..} wantedVersions networkMagic slotsPerEpoch socket client =
        withIOManager $ \iocp -> do
            connectTo (mkLocalSnocket iocp) tracers versions socket
                & onExceptions
                & foreverCalmly
      where
        tracers = NetworkConnectTracers
            { nctMuxTracer = nullTracer
            , nctHandshakeTracer = nullTracer
            }

        mkLocalSnocket iocp =
            let snocket = localSnocket iocp
             in snocket
                    { toBearer = \time trMux fd -> do
                        bearer <- toBearer snocket time trMux fd
                        toggleConnected $> bearer
                    }

        versions = combineVersions
            [ simpleSingletonVersions v vData (mkOuroborosApplication v)
            | v <- wantedVersions
            ]
          where
            vData  = NodeToClientVersionData networkMagic

        mkOuroborosApplication version =
            OuroborosApplication $ \_connectionId _controlMessageSTM ->
                [ MiniProtocol
                    { miniProtocolNum =
                        MiniProtocolNum 5
                    , miniProtocolLimits =
                        MiniProtocolLimits (fromIntegral $ maxBound @Word32)
                    , miniProtocolRun =
                        InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
                            let
                                peer = chainSyncClientPeerPipelined client
                                codec = cChainSyncCodec (codecs slotsPerEpoch version)
                             in
                                runPipelinedPeer nullTracer codec channel peer
                    }
                ]

        foreverCalmly a = do
            let a' = a *> threadDelay 5 *> a' in a'

        onExceptions
            = handle onUnknownException
            . handle onIOException
            . (`onException` toggleDisconnected)

        onIOException :: IOException -> IO ()
        onIOException e
            | isRetryable = do
                traceWith tr $ ChainSyncFailedToConnect socket 5
            | otherwise = do
                traceWith tr $ ChainSyncUnknownException $ show (toException e)
          where
            isRetryable :: Bool
            isRetryable = isResourceVanishedError e || isDoesNotExistError e || isTryAgainError e

            isTryAgainError :: IOException -> Bool
            isTryAgainError = isInfixOf "resource exhausted" . show

            isResourceVanishedError :: IOException -> Bool
            isResourceVanishedError = isInfixOf "resource vanished" . show

        onUnknownException :: SomeException -> IO ()
        onUnknownException e
            | isAsyncException e = do
                throwIO e
            | otherwise = case fromException e of
                Just (_ :: IntersectionNotFoundException) ->
                    throwIO e
                Nothing ->
                    traceWith tr $ ChainSyncUnknownException $ show e

codecs
    :: EpochSlots
    -> NodeToClientVersion
    -> ClientCodecs (Block IO) IO
codecs epochSlots nodeToClientV =
    clientCodecs cfg (supportedVersions ! nodeToClientV) nodeToClientV
  where
    supportedVersions =
        supportedNodeToClientVersions (Proxy @(Block IO))
    cfg =
        CardanoCodecConfig byron shelley allegra mary alonzo
      where
        byron   = ByronCodecConfig epochSlots
        shelley = ShelleyCodecConfig
        allegra = ShelleyCodecConfig
        mary    = ShelleyCodecConfig
        alonzo  = ShelleyCodecConfig

--
-- Tracer
--

data TraceChainSync where
    ChainSyncRollBackward
        :: { point :: SlotNo }
        -> TraceChainSync
    ChainSyncRollForward
        :: { slotNo :: SlotNo, matches :: Int }
        -> TraceChainSync
    ChainSyncIntersectionNotFound
        :: { points :: [WithOrigin SlotNo] }
        -> TraceChainSync
    ChainSyncFailedToConnect
        :: { socket :: FilePath, retryingIn :: DiffTime }
        -> TraceChainSync
    ChainSyncUnknownException
        :: { exception :: Text }
        -> TraceChainSync
    deriving stock (Generic, Show)

instance ToJSON TraceChainSync where
    toEncoding =
        defaultGenericToEncoding

instance ToJSON (WithOrigin SlotNo) where
    toEncoding = \case
        Origin -> toEncoding ("origin" :: Text)
        At sl -> toEncoding sl

instance HasSeverityAnnotation TraceChainSync where
    getSeverityAnnotation = \case
        ChainSyncRollForward{} ->
            Debug
        ChainSyncRollBackward{} ->
            Notice
        ChainSyncFailedToConnect{} ->
            Warning
        ChainSyncIntersectionNotFound{} ->
            Error
        ChainSyncUnknownException{} ->
            Error
