module Kupo.Data.Cardano.Block where

import Kupo.Prelude

import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock
    )

type Block =
    CardanoBlock StandardCrypto
