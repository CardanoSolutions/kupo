module Kupo.Data.Cardano.Output where

import Kupo.Prelude

import Cardano.Ledger.Val
    ( Val (inject)
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
    ( Script
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
import qualified Cardano.Ledger.Era as Ledger.Era
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Cardano.Ledger.Shelley.Tx as Ledger.Shelley

-- Output

type Output =
    Output' StandardCrypto

type Output' crypto =
    Ledger.Babbage.TxOut (BabbageEra crypto)

mkOutput
    :: Address
    -> Value
    -> Datum
    -> Maybe Script
    -> Output
mkOutput address value datum script =
    Ledger.Babbage.TxOut
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
    Ledger.Babbage.TxOut
        (Ledger.AddrBootstrap (Ledger.BootstrapAddress address))
        (inject $ Ledger.Coin $ toInteger $ Ledger.Byron.unsafeGetLovelace value)
        Ledger.Babbage.NoDatum
        SNothing
{-# INLINABLE fromByronOutput #-}

fromShelleyOutput
    :: forall (era :: Type -> Type) crypto.
        ( Ledger.Era.Era (era crypto)
        , Ledger.Era.Crypto (era crypto) ~ crypto
        , Ledger.Core.TxOut (era crypto) ~ Ledger.Shelley.TxOut (era crypto)
        , Show (Ledger.Core.Value (era crypto))
        )
    => (Ledger.Core.Value (era crypto) -> Ledger.Value crypto)
    -> Ledger.Core.TxOut (era crypto)
    -> Ledger.Core.TxOut (BabbageEra crypto)
fromShelleyOutput liftValue (Ledger.Shelley.TxOut addr value) =
    Ledger.Babbage.TxOut addr (liftValue value) Ledger.Babbage.NoDatum SNothing
{-# INLINABLE fromShelleyOutput #-}

fromAlonzoOutput
    :: forall crypto.
        ( Crypto crypto
        )
    => Ledger.Core.TxOut (AlonzoEra crypto)
    -> Ledger.Core.TxOut (BabbageEra crypto)
fromAlonzoOutput (Ledger.Alonzo.TxOut addr value datum) =
    case datum of
        SNothing ->
            Ledger.Babbage.TxOut
                addr
                value
                Ledger.Babbage.NoDatum
                SNothing

        SJust datumHash ->
            Ledger.Babbage.TxOut
                addr
                value
                (Ledger.Babbage.DatumHash datumHash)
                SNothing

getAddress
    :: Output
    -> Address
getAddress (Ledger.Babbage.TxOut address _value _datum _refScript) =
    address
{-# INLINABLE getAddress #-}

getValue
    :: Output
    -> Value
getValue (Ledger.Babbage.TxOut _address value _datum _refScript) =
    value
{-# INLINABLE getValue #-}

getDatum
    :: Output
    -> Datum
getDatum (Ledger.Babbage.TxOut _address _value datum _refScript) =
    fromBabbageDatum datum
{-# INLINABLE getDatum #-}

getScript
    :: Output
    -> Maybe Script
getScript (Ledger.Babbage.TxOut _address _value _datum refScript) =
    strictMaybeToMaybe refScript
{-# INLINABLE getScript #-}

-- ComparableOutput

data ComparableOutput = ComparableOutput
    { comparableOutputAddress :: !Address
    , comparableOutputValue :: !ComparableValue
    , comparableOutputDatum :: !Datum
    , comparableOutputScript :: !(Maybe Script)
    } deriving (Generic, Show, Eq, Ord)

toComparableOutput
    :: Output
    -> ComparableOutput
toComparableOutput out = ComparableOutput
    { comparableOutputAddress = getAddress out
    , comparableOutputValue = toComparableValue (getValue out)
    , comparableOutputDatum = getDatum out
    , comparableOutputScript = getScript out
    }

fromComparableOutput
    :: ComparableOutput
    -> Output
fromComparableOutput (ComparableOutput addr val datum script) =
    mkOutput addr (fromComparableValue val) datum script
