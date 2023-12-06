{-# LANGUAGE TypeOperators #-}

module Kupo.Data.Cardano.Output where

import Kupo.Prelude

import Cardano.Ledger.Val
    ( Val (..)
    )
import Data.Maybe.Strict
    ( StrictMaybe (..)
    , maybeToStrictMaybe
    , strictMaybeToMaybe
    )
import Kupo.Data.Cardano.Address
    ( Address
    )
import Kupo.Data.Cardano.Datum
    ( Datum
    , fromBabbageDatum
    , toBabbageDatum
    )
import Kupo.Data.Cardano.Script
    ( ComparableScript
    , Script
    , fromComparableScript
    , hashScript
    , toComparableScript
    )
import Kupo.Data.Cardano.ScriptHash
    ( ScriptHash
    )
import Kupo.Data.Cardano.Value
    ( ComparableValue
    , Value
    , fromComparableValue
    , toComparableValue
    )

import qualified Cardano.Chain.Common as Ledger.Byron
import qualified Cardano.Chain.UTxO as Ledger.Byron
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger.Alonzo
import qualified Cardano.Ledger.Babbage.TxBody as Ledger.Babbage
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Ledger.Core
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Cardano.Ledger.Shelley.Tx as Ledger.Shelley

import qualified Data.Map as Map

-- Output

type Output =
    Output' StandardCrypto

type Output' crypto =
    Ledger.Babbage.BabbageTxOut (BabbageEra crypto)

mkOutput
    :: Address
    -> Value
    -> Datum
    -> Maybe Script
    -> Output
mkOutput address value datum script =
    Ledger.Babbage.BabbageTxOut
        address
        value
        (toBabbageDatum datum)
        (maybeToStrictMaybe script)
{-# INLINABLE mkOutput #-}

fromByronOutput
    :: forall crypto.
        ( Crypto crypto
        )
    => Ledger.Byron.TxOut
    -> Ledger.Core.TxOut (BabbageEra crypto)
fromByronOutput (Ledger.Byron.TxOut address value) =
    Ledger.Babbage.BabbageTxOut
        (Ledger.AddrBootstrap (Ledger.BootstrapAddress address))
        (inject $ Ledger.Coin $ toInteger $ Ledger.Byron.unsafeGetLovelace value)
        Ledger.Babbage.NoDatum
        SNothing
{-# INLINABLE fromByronOutput #-}

fromShelleyOutput
    :: forall (era :: Type -> Type) crypto.
        ( Ledger.Core.Era (era crypto)
        , Ledger.Core.EraCrypto (era crypto) ~ crypto
        , Ledger.Core.TxOut (era crypto) ~ Ledger.Shelley.ShelleyTxOut (era crypto)
        , Val (Ledger.Core.Value (era crypto))
        )
    => (Ledger.Core.Value (era crypto) -> Ledger.MaryValue crypto)
    -> Ledger.Core.TxOut (era crypto)
    -> Ledger.Core.TxOut (BabbageEra crypto)
fromShelleyOutput liftValue (Ledger.Shelley.ShelleyTxOut addr value) =
    Ledger.Babbage.BabbageTxOut addr (liftValue value) Ledger.Babbage.NoDatum SNothing
{-# INLINABLE fromShelleyOutput #-}

fromAlonzoOutput
    :: forall crypto.
        ( Crypto crypto
        )
    => Ledger.Core.TxOut (AlonzoEra crypto)
    -> Ledger.Core.TxOut (BabbageEra crypto)
fromAlonzoOutput (Ledger.Alonzo.AlonzoTxOut addr value datum) =
    case datum of
        SNothing ->
            Ledger.Babbage.BabbageTxOut
                addr
                value
                Ledger.Babbage.NoDatum
                SNothing

        SJust datumHash ->
            Ledger.Babbage.BabbageTxOut
                addr
                value
                (Ledger.Babbage.DatumHash datumHash)
                SNothing

getAddress
    :: Output
    -> Address
getAddress (Ledger.Babbage.BabbageTxOut address _value _datum _refScript) =
    address
{-# INLINABLE getAddress #-}

getValue
    :: Output
    -> Value
getValue (Ledger.Babbage.BabbageTxOut _address value _datum _refScript) =
    value
{-# INLINABLE getValue #-}

getDatum
    :: Output
    -> Datum
getDatum (Ledger.Babbage.BabbageTxOut _address _value datum _refScript) =
    fromBabbageDatum datum
{-# INLINABLE getDatum #-}

getScript
    :: Output
    -> Maybe Script
getScript (Ledger.Babbage.BabbageTxOut _address _value _datum refScript) =
    strictMaybeToMaybe refScript
{-# INLINABLE getScript #-}

scriptsFromOutputs
    :: forall f.
        ( Foldable f
        )
    => f Output
    -> Map ScriptHash Script
    -> Map ScriptHash Script
scriptsFromOutputs =
    flip $ foldr
        (\out -> case getScript out of
            Nothing -> identity
            Just s -> Map.insert (hashScript s) s
        )
{-# INLINABLE scriptsFromOutputs #-}

-- ComparableOutput

data ComparableOutput = ComparableOutput
    { comparableOutputAddress :: !Address
    , comparableOutputValue :: !ComparableValue
    , comparableOutputDatum :: !Datum
    , comparableOutputScript :: !(Maybe ComparableScript)
    } deriving (Generic, Show, Eq, Ord)

toComparableOutput
    :: Output
    -> ComparableOutput
toComparableOutput out = ComparableOutput
    { comparableOutputAddress = getAddress out
    , comparableOutputValue = toComparableValue (getValue out)
    , comparableOutputDatum = getDatum out
    , comparableOutputScript = toComparableScript <$> getScript out
    }

fromComparableOutput
    :: ComparableOutput
    -> Output
fromComparableOutput (ComparableOutput addr val datum script) =
    mkOutput addr (fromComparableValue val) datum (fromComparableScript <$> script)
