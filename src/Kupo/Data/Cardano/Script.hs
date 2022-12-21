module Kupo.Data.Cardano.Script where

import Kupo.Prelude

import Cardano.Binary
    ( DecoderError (..)
    , FromCBOR (..)
    , decodeAnnotator
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

import qualified Cardano.Binary as Cbor
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.Language as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Core as Ledger.Core
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as Ledger.MaryAllegra
import qualified Cardano.Ledger.ShelleyMA.Timelocks as Ledger
import qualified Cardano.Ledger.ShelleyMA.Timelocks as Ledger.MaryAllegra
import qualified Codec.CBOR.Read as Cbor
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.ByteString as BS
import qualified Data.Map as Map

type Script =
    Ledger.Script (BabbageEra StandardCrypto)

scriptFromAllegraAuxiliaryData
    :: forall era. (Ledger.Core.Script era ~ Ledger.Timelock StandardCrypto)
    => (Ledger.Core.Script era -> Script)
    -> Ledger.MaryAllegra.AuxiliaryData era
    -> Map ScriptHash Script
    -> Map ScriptHash Script
scriptFromAllegraAuxiliaryData liftScript (Ledger.MaryAllegra.AuxiliaryData _ scripts) m0 =
    foldr
        (\(liftScript -> s) -> Map.insert (hashScript s) s)
        m0
        scripts
{-# INLINABLE scriptFromAllegraAuxiliaryData #-}

scriptFromAlonzoAuxiliaryData
    :: forall era.
        ( Ledger.Era era
        , Ledger.Core.Script era ~ Ledger.Script era
        )
    => (Ledger.Script era -> Script)
    -> Ledger.AuxiliaryData era
    -> Map ScriptHash Script
    -> Map ScriptHash Script
scriptFromAlonzoAuxiliaryData liftScript Ledger.AuxiliaryData{Ledger.scripts} m0 =
    foldr
        (\(liftScript -> s) -> Map.insert (hashScript s) s)
        m0
        scripts
{-# INLINABLE scriptFromAlonzoAuxiliaryData #-}

fromAllegraScript
    :: Ledger.MaryAllegra.Timelock StandardCrypto
    -> Script
fromAllegraScript =
    Ledger.TimelockScript
{-# INLINABLE fromAllegraScript #-}

fromMaryScript
    :: Ledger.MaryAllegra.Timelock StandardCrypto
    -> Script
fromMaryScript =
    Ledger.TimelockScript
{-# INLINABLE fromMaryScript  #-}

fromAlonzoScript
    :: Ledger.Script (AlonzoEra StandardCrypto)
    -> Script
fromAlonzoScript = \case
    Ledger.TimelockScript script ->
        Ledger.TimelockScript script
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
            0 -> Ledger.TimelockScript <$> decodeAnnotator "Timelock" fromCBOR script
            1 -> pure $ Ledger.PlutusScript Ledger.PlutusV1 (toShort $ toStrict script)
            2 -> pure $ Ledger.PlutusScript Ledger.PlutusV2 (toShort $ toStrict script)
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
    Ledger.hashScript @(BabbageEra StandardCrypto)
{-# INLINABLE hashScript #-}
