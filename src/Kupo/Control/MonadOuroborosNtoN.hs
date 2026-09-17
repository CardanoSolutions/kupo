--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module Kupo.Control.MonadOuroborosNtoN
    ( -- * MonadOuroborosNtoN
      MonadOuroborosNtoN (..)

      -- * Health Reporting
    , ConnectionStatusToggle (..)

      -- * Types
    , NetworkMagic (..)
    , EpochSlots (..)
    , NodeToNodeVersion (..)
    , WithOrigin (..)
    ) where

import Kupo.Prelude

import Cardano.Chain.Slotting
    ( EpochSlots (..)
    )
import Cardano.Network.NodeToNode
    ( DiffusionMode (InitiatorOnlyDiffusionMode)
    , NetworkConnectTracers (..)
    , NodeToNodeVersion (..)
    , NodeToNodeVersionData (..)
    , connectTo
    )
import Control.Tracer
    ( nullTracer
    )
import Data.Map.Strict
    ( (!)
    )
import Kupo.Control.MonadThrow
    ( MonadThrow (..)
    )
import Network.Mux
    ( StartOnDemandOrEagerly (..)
    )
import qualified Network.Mux
import Network.Socket
    ( AddrInfo (..)
    , SocketType (Stream)
    , defaultHints
    , getAddrInfo
    )
import Ouroboros.Consensus.Block.Abstract
    ( Header
    )
import Ouroboros.Consensus.Byron.Ledger.Config
    ( CodecConfig (..)
    )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock
    )
import Ouroboros.Consensus.Cardano.Block
    ( CodecConfig (..)
    )
import Ouroboros.Consensus.Network.NodeToNode
    ( Codecs (cChainSyncCodec)
    , defaultCodecs
    )
import Ouroboros.Consensus.Node.NetworkProtocolVersion
    ( SupportedNetworkProtocolVersion (..)
    )
import Ouroboros.Consensus.Shelley.Ledger.Config
    ( CodecConfig (..)
    )
import Ouroboros.Network.Block
    ( Point (..)
    , StandardHash
    , Tip (..)
    )
import Ouroboros.Network.Driver.Simple
    ( runPipelinedPeer
    )
import Ouroboros.Network.IOManager
    ( withIOManager
    )
import Ouroboros.Network.Magic
    ( NetworkMagic (..)
    )
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolCb (..)
    , MiniProtocolLimits (..)
    , MiniProtocolNum (..)
    , OuroborosApplication (..)
    , RunMiniProtocol (..)
    )
import Ouroboros.Network.PeerSelection.PeerSharing
    ( PeerSharing (..)
    )
import Ouroboros.Network.PeerSelection.PeerSharing.Codec
    (decodeRemoteAddress
    , encodeRemoteAddress
    )
import Ouroboros.Network.Point
    ( WithOrigin (..)
    )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined (..)
    , chainSyncClientPeerPipelined
    )
import Ouroboros.Network.Protocol.Handshake.Version
    ( combineVersions
    , simpleSingletonVersions
    )
import Ouroboros.Network.Snocket
    ( Snocket (..)
    , socketSnocket
    )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol
    ()

import qualified Data.List.NonEmpty as NE

class MonadThrow m => MonadOuroborosNtoN (m :: Type -> Type) where
    type BlockT m :: Type
    withChainSyncServerNtoN
        :: (StandardHash (BlockT m), Typeable (BlockT m))
        => ConnectionStatusToggle m
        -> [NodeToNodeVersion]
        -> NetworkMagic
        -> EpochSlots
        -> String
        -> Int
        -> ChainSyncClientPipelined
            (Header (BlockT m))
            (Point (BlockT m))
            (Tip (BlockT m))
            IO
            ()
        -> m ()

instance MonadOuroborosNtoN IO where
    type BlockT IO = CardanoBlock StandardCrypto
    withChainSyncServerNtoN
        ConnectionStatusToggle{..}
        wantedVersions
        networkMagic
        slotsPerEpoch
        host
        port
        client =
            withIOManager $ \iocp -> do
                addr <- resolve
                result <- connectTo
                    (mkSocketSnocket iocp)
                    tracers
                    versions
                    Nothing
                    (addrAddress addr)
                case result of
                    Left err ->
                        throwIO err
                    Right _ ->
                        pure ()

      where
        resolve = do
            let hints = defaultHints { addrSocketType = Stream }
            NE.head <$> getAddrInfo (Just hints) (Just host) (Just (show port))

        tracers = NetworkConnectTracers
            { nctMuxTracers = Network.Mux.nullTracers
            , nctHandshakeTracer = nullTracer
            }

        mkSocketSnocket iocp =
            let snocket = socketSnocket iocp
             in snocket
                    { connect = \fd addr -> do
                        connect snocket fd addr
                        toggleConnected
                    }

        versions = combineVersions
            [ simpleSingletonVersions v vData (mkOuroborosApplication v)
            | v <- wantedVersions
            ]
              where
                vData = NodeToNodeVersionData
                            networkMagic
                            InitiatorOnlyDiffusionMode
                            PeerSharingDisabled
                            False

        mkOuroborosApplication version _versionData =
            OuroborosApplication
                [ MiniProtocol
                    { miniProtocolNum =
                        MiniProtocolNum 5
                    , miniProtocolStart =
                        StartEagerly
                    , miniProtocolLimits =
                        MiniProtocolLimits (fromIntegral $ maxBound @Word32)
                    , miniProtocolRun =
                        InitiatorProtocolOnly $ MiniProtocolCb $ \_ channel ->
                            let
                                peer = chainSyncClientPeerPipelined client
                                codec =
                                    cChainSyncCodec
                                        (codecs slotsPerEpoch version)
                             in
                                runPipelinedPeer nullTracer codec channel peer
                    }
                ]

        codecs epochSlots nodeToNodeV =
            defaultCodecs
                (cfg epochSlots)
                (supportedVersions ! nodeToNodeV)
                encodeRemoteAddress
                decodeRemoteAddress
                nodeToNodeV

        supportedVersions = supportedNodeToNodeVersions (Proxy @(BlockT IO))

        cfg slts =
            CardanoCodecConfig
                (byron slts) shelley allegra mary alonzo babbage conway dijkstra

        byron slts = ByronCodecConfig slts
        shelley    = ShelleyCodecConfig
        allegra    = ShelleyCodecConfig
        mary       = ShelleyCodecConfig
        alonzo     = ShelleyCodecConfig
        babbage    = ShelleyCodecConfig
        conway     = ShelleyCodecConfig
        dijkstra   = ShelleyCodecConfig
