module Kupo.Data.Cardano.HeaderHash
    ( HeaderHash
    , module Kupo.Data.Cardano.HeaderHash
    ) where

import Kupo.Prelude

import Cardano.Crypto.Hash
    ( hashFromTextAsHex
    , hashToBytesShort
    )
import Kupo.Data.Cardano.Block
    ( Block
    )
import Ouroboros.Consensus.Block
    ( ConvertRawHash (..)
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( OneEraHash (..)
    )
import Ouroboros.Network.Block
    ( HeaderHash
    )

import Ouroboros.Consensus.Cardano
    ()
import Ouroboros.Consensus.Protocol.Praos.Translate
    ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol
    ()

import qualified Data.Aeson.Encoding as Json

-- | Deserialise a 'HeaderHash' from a base16-encoded text string.
headerHashFromText
    :: Text
    -> Maybe (HeaderHash Block)
headerHashFromText =
    fmap (OneEraHash . hashToBytesShort) . hashFromTextAsHex @Blake2b_256
{-# INLINABLE headerHashFromText #-}

headerHashToText
    :: HeaderHash Block
    -> Text
headerHashToText =
    encodeBase16 . headerHashToBytes
{-# INLINABLE headerHashToText #-}

headerHashToBytes
    :: HeaderHash Block
    -> ByteString
headerHashToBytes =
    fromShort . toShortRawHash (Proxy @Block)
{-# INLINABLE headerHashToBytes #-}

headerHashToJson
    :: HeaderHash Block
    -> Json.Encoding
headerHashToJson =
    encodeBytes . headerHashToBytes
{-# INLINABLE headerHashToJson #-}

unsafeHeaderHashFromBytes
    :: ByteString
    -> HeaderHash Block
unsafeHeaderHashFromBytes =
    fromRawHash (Proxy @Block)
{-# INLINABLE unsafeHeaderHashFromBytes #-}
