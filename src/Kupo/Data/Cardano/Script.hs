module Kupo.Data.Cardano.Script where

import Kupo.Prelude

import Cardano.Ledger.Binary
    ( DecoderError (..)
    )
import Control.Arrow
    ( left
    )
import Kupo.Data.Cardano.NativeScript
    ( NativeScript
    )
import Kupo.Data.Cardano.ScriptHash
    ( ScriptHash
    )
import Ouroboros.Consensus.Util
    ( eitherToMaybe
    )

import qualified Cardano.Ledger.Allegra.Scripts as Ledger.Allegra
import qualified Cardano.Ledger.Allegra.TxAuxData as Ledger.Allegra
import qualified Cardano.Ledger.Alonzo as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.TxAuxData as Ledger.Alonzo
import qualified Cardano.Ledger.Core as Ledger.Core
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger

import qualified Codec.CBOR.Decoding as Cbor
import qualified Codec.CBOR.Read as Cbor
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.ByteString as BS
import qualified Data.Map as Map

type Script =
    Ledger.Script (BabbageEra StandardCrypto)

scriptFromAllegraAuxiliaryData
    :: forall era.
        ( Ledger.Era era
        , Ledger.Core.Script era ~ Ledger.Allegra.Timelock era
        )
    => (Ledger.Core.Script era -> Script)
    -> Ledger.Allegra.AllegraTxAuxData era
    -> Map ScriptHash Script
    -> Map ScriptHash Script
scriptFromAllegraAuxiliaryData liftScript (Ledger.Allegra.AllegraTxAuxData _ scripts) m0 =
    foldr
        (\(liftScript -> s) -> Map.insert (hashScript s) s)
        m0
        scripts
{-# INLINABLE scriptFromAllegraAuxiliaryData #-}

scriptFromAlonzoAuxiliaryData
    :: forall era.
        ( Ledger.Era era
        , Ledger.Core.Script era ~ Ledger.Alonzo.AlonzoScript era
        )
    => (Ledger.Core.Script era -> Script)
    -> Ledger.Alonzo.AlonzoTxAuxData era
    -> Map ScriptHash Script
    -> Map ScriptHash Script
scriptFromAlonzoAuxiliaryData liftScript (Ledger.Alonzo.AlonzoTxAuxData _ scripts _) m0 =
    foldr
        (\((liftScript . Ledger.TimelockScript) -> s) -> Map.insert (hashScript s) s)
        m0
        scripts
{-# INLINABLE scriptFromAlonzoAuxiliaryData #-}

fromAllegraScript
    :: Ledger.Allegra.Timelock (AllegraEra StandardCrypto)
    -> Script
fromAllegraScript =
    Ledger.TimelockScript . Ledger.Allegra.translateTimelock
{-# INLINABLE fromAllegraScript #-}

fromMaryScript
    :: Ledger.Allegra.Timelock (MaryEra StandardCrypto)
    -> Script
fromMaryScript =
    Ledger.TimelockScript . Ledger.Allegra.translateTimelock
{-# INLINABLE fromMaryScript  #-}

fromAlonzoScript
    :: Ledger.Script (AlonzoEra StandardCrypto)
    -> Script
fromAlonzoScript = \case
    Ledger.TimelockScript script ->
        Ledger.TimelockScript (Ledger.Allegra.translateTimelock script)
    Ledger.PlutusScript lang bytes ->
        Ledger.PlutusScript lang bytes

fromBabbageScript
    :: Ledger.Script (BabbageEra StandardCrypto)
    -> Script
fromBabbageScript =
    identity
{-# INLINABLE fromBabbageScript #-}

scriptToJson
    :: Script
    -> Json.Encoding
scriptToJson script = encodeObject
    [ ("script", encodeBytes (Ledger.originalBytes script))
    , ("language", case script of
        Ledger.TimelockScript{} ->
          Json.text "native"
        Ledger.PlutusScript Ledger.PlutusV1 _ ->
          Json.text "plutus:v1"
        Ledger.PlutusScript Ledger.PlutusV2 _ ->
          Json.text "plutus:v2"
        Ledger.PlutusScript Ledger.PlutusV3 _ ->
          Json.text "plutus:v3"
      )
    ]

scriptToBytes
    :: Script
    -> ByteString
scriptToBytes = \case
    script@Ledger.TimelockScript{} ->
        BS.singleton 0 <> Ledger.originalBytes script
    script@(Ledger.PlutusScript Ledger.PlutusV1 _) ->
        BS.singleton 1 <> Ledger.originalBytes script
    script@(Ledger.PlutusScript Ledger.PlutusV2 _) ->
        BS.singleton 2 <> Ledger.originalBytes script
    script@(Ledger.PlutusScript Ledger.PlutusV3 _) ->
        BS.singleton 3 <> Ledger.originalBytes script

unsafeScriptFromBytes
    :: HasCallStack
    => ByteString
    -> Script
unsafeScriptFromBytes =
    fromMaybe (error "unsafeScriptFromBytes") . scriptFromBytes
{-# INLINABLE unsafeScriptFromBytes #-}

scriptFromBytes
    :: ByteString
    -> Maybe Script
scriptFromBytes (toLazy -> bytes) =
    eitherToMaybe $ do
        (script, tag) <- left (DecoderErrorDeserialiseFailure "Script") $
            Cbor.deserialiseFromBytes Cbor.decodeWord8 bytes
        case tag of
            0 -> Ledger.TimelockScript <$> decodeCborAnn @BabbageEra "Timelock" decCBOR script
            1 -> pure $ Ledger.PlutusScript Ledger.PlutusV1 (toShort $ toStrict script)
            2 -> pure $ Ledger.PlutusScript Ledger.PlutusV2 (toShort $ toStrict script)
            3 -> pure $ Ledger.PlutusScript Ledger.PlutusV3 (toShort $ toStrict script)
            t -> Left (DecoderErrorUnknownTag "Script" t)

fromNativeScript
    :: NativeScript
    -> Script
fromNativeScript =
    Ledger.TimelockScript
{-# INLINABLE fromNativeScript #-}

hashScript
    :: Script
    -> ScriptHash
hashScript =
    Ledger.Core.hashScript @(BabbageEra StandardCrypto)
{-# INLINABLE hashScript #-}

newtype ComparableScript = ComparableScript { unComparableScript :: Script }
    deriving (Generic, Show, Eq)

instance Ord ComparableScript where
    compare (ComparableScript a) (ComparableScript b) =
        compare (Ledger.originalBytes a) (Ledger.originalBytes b)

toComparableScript :: Script -> ComparableScript
toComparableScript = ComparableScript
{-# INLINEABLE toComparableScript #-}

fromComparableScript :: ComparableScript -> Script
fromComparableScript = unComparableScript
{-# INLINEABLE fromComparableScript #-}
