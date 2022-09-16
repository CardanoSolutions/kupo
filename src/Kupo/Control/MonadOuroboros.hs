--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module Kupo.Control.MonadOuroboros
    ( -- * MonadOuroboros
      MonadOuroboros (..)

      -- * Health Reporting
    , ConnectionStatusToggle (..)

      -- * Types
    , NetworkMagic (..)
    , EpochSlots (..)
    , NodeToClientVersion (..)
    , WithOrigin (..)
    ) where

import Kupo.Prelude

import Cardano.Chain.Slotting
    ( EpochSlots (..)
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Control.Tracer
    ( nullTracer
    )
import Data.Map.Strict
    ( (!)
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
import Ouroboros.Consensus.Network.NodeToClient
    ( ClientCodecs
    , Codecs' (..)
    , clientCodecs
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
import Ouroboros.Network.Magic
    ( NetworkMagic (..)
    )
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
    )

import Ouroboros.Consensus.Protocol.Praos.Translate
    ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol
    ()

class MonadOuroboros (m :: Type -> Type) where
    type BlockT m :: Type
    withChainSyncServer
        :: (StandardHash (BlockT m), Typeable (BlockT m))
        => ConnectionStatusToggle m
        -> [NodeToClientVersion]
        -> NetworkMagic
        -> EpochSlots
        -> FilePath
        -> ChainSyncClientPipelined (BlockT m) (Point (BlockT m)) (Tip (BlockT m)) IO ()
        -> m ()

instance MonadOuroboros IO where
    type BlockT IO = CardanoBlock StandardCrypto
    withChainSyncServer ConnectionStatusToggle{..} wantedVersions networkMagic slotsPerEpoch socket client =
        withIOManager $ \iocp -> do
            connectTo (mkLocalSnocket iocp) tracers versions socket
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

codecs
    :: EpochSlots
    -> NodeToClientVersion
    -> ClientCodecs (BlockT IO) IO
codecs epochSlots nodeToClientV =
    clientCodecs cfg (supportedVersions ! nodeToClientV) nodeToClientV
  where
    supportedVersions =
        supportedNodeToClientVersions (Proxy @(BlockT IO))
    cfg =
        CardanoCodecConfig byron shelley allegra mary alonzo babbage
      where
        byron    = ByronCodecConfig epochSlots
        shelley  = ShelleyCodecConfig
        allegra  = ShelleyCodecConfig
        mary     = ShelleyCodecConfig
        alonzo   = ShelleyCodecConfig
        babbage  = ShelleyCodecConfig
