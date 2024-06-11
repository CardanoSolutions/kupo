{-# LANGUAGE TypeOperators #-}

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

import qualified Cardano.Ledger.Plutus.Language as Ledger

import qualified Cardano.Ledger.Allegra.Scripts as Ledger.Allegra
import qualified Cardano.Ledger.Allegra.TxAuxData as Ledger.Allegra
import qualified Cardano.Ledger.Alonzo as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxAuxData as Ledger.Alonzo
import qualified Cardano.Ledger.Binary.Plain as Ledger
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
    Ledger.Alonzo.Script (ConwayEra StandardCrypto)

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
        ( Ledger.Core.Script era ~ Ledger.Alonzo.AlonzoScript era
        , Ledger.Alonzo.AlonzoEraScript era
        )
    => (Ledger.Core.Script era -> Script)
    -> Ledger.Alonzo.AlonzoTxAuxData era
    -> Map ScriptHash Script
    -> Map ScriptHash Script
scriptFromAlonzoAuxiliaryData liftScript (Ledger.Alonzo.AlonzoTxAuxData _ scripts _) m0 =
    foldr
        (\((liftScript . Ledger.Alonzo.TimelockScript) -> s) -> Map.insert (hashScript s) s)
        m0
        scripts
{-# INLINABLE scriptFromAlonzoAuxiliaryData #-}

fromAllegraScript
    :: Ledger.Allegra.Timelock (AllegraEra StandardCrypto)
    -> Script
fromAllegraScript =
    Ledger.Alonzo.TimelockScript . Ledger.Allegra.translateTimelock
{-# INLINABLE fromAllegraScript #-}

fromMaryScript
    :: Ledger.Allegra.Timelock (MaryEra StandardCrypto)
    -> Script
fromMaryScript =
    Ledger.Alonzo.TimelockScript . Ledger.Allegra.translateTimelock
{-# INLINABLE fromMaryScript  #-}

fromAlonzoScript
    :: Ledger.Alonzo.Script (AlonzoEra StandardCrypto)
    -> Script
fromAlonzoScript =
    fromBabbageScript . Ledger.Core.upgradeScript
{-# INLINABLE fromAlonzoScript #-}

fromBabbageScript
    :: Ledger.Alonzo.Script (BabbageEra StandardCrypto)
    -> Script
fromBabbageScript =
    Ledger.Core.upgradeScript
{-# INLINABLE fromBabbageScript #-}

fromConwayScript
    :: Ledger.Alonzo.Script (ConwayEra StandardCrypto)
    -> Script
fromConwayScript =
    identity
{-# INLINABLE fromConwayScript #-}

scriptToJson
    :: Script
    -> Json.Encoding
scriptToJson script = encodeObject
    [ ("script", encodeBytes (Ledger.originalBytes script))
    , ("language", case script of
        Ledger.Alonzo.TimelockScript _ ->
            Json.text "native"
        Ledger.Alonzo.PlutusScript ps ->
            case Ledger.Alonzo.plutusScriptLanguage ps of
                Ledger.PlutusV1 -> Json.text "plutus:v1"
                Ledger.PlutusV2 -> Json.text "plutus:v2"
                Ledger.PlutusV3 -> Json.text "plutus:v3"
      )
    ]

scriptToBytes
    :: Script
    -> ByteString
scriptToBytes =
    let withTag n s = BS.singleton n <> Ledger.originalBytes s
     in \case
        Ledger.Alonzo.TimelockScript script ->
            withTag 0 script
        Ledger.Alonzo.PlutusScript script ->
            case Ledger.Alonzo.plutusScriptLanguage script of
                Ledger.PlutusV1 -> withTag 1 script
                Ledger.PlutusV2 -> withTag 2 script
                Ledger.PlutusV3 -> withTag 3 script

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
            0 -> Ledger.Alonzo.TimelockScript <$> decodeCborAnn @BabbageEra "Timelock" decCBOR script
            1 -> plutusScript Ledger.PlutusV1 script
            2 -> plutusScript Ledger.PlutusV2 script
            3 -> plutusScript Ledger.PlutusV3 script
            t -> Left (DecoderErrorUnknownTag "Script" t)
  where
    plutusScript lang s =
        let
            uplc =
                Ledger.PlutusBinary $ toShort $ toStrict s

            script = maybeToRight
                (Ledger.DecoderErrorCustom "Incompatible language and era" $ show (lang, uplc))
                (Ledger.Alonzo.mkBinaryPlutusScript @(ConwayEra StandardCrypto) lang uplc)
         in
            Ledger.Alonzo.PlutusScript <$> script

fromNativeScript
    :: NativeScript
    -> Script
fromNativeScript =
    Ledger.Alonzo.TimelockScript
{-# INLINABLE fromNativeScript #-}

hashScript
    :: Script
    -> ScriptHash
hashScript =
    Ledger.Core.hashScript @(ConwayEra StandardCrypto)
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
