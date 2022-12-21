module Kupo.Data.Cardano.Datum where
import Kupo.Prelude

import Kupo.Data.Cardano.BinaryData
    ( BinaryData
    , hashBinaryData
    )
import Kupo.Data.Cardano.DatumHash
    ( DatumHash
    )

import qualified Cardano.Ledger.Alonzo.Data as Ledger

data Datum
    = NoDatum
    | Reference !(Either DatumHash BinaryData)
    | Inline !(Either DatumHash BinaryData)
    deriving (Generic, Show, Eq, Ord)

toBabbageDatum
    :: Datum
    -> Ledger.Datum (BabbageEra StandardCrypto)
toBabbageDatum = \case
    NoDatum -> Ledger.NoDatum
    Reference (Left ref) -> Ledger.DatumHash ref
    Reference (Right bin) -> Ledger.Datum bin
    Inline (Left ref) -> Ledger.DatumHash ref
    Inline (Right bin) -> Ledger.Datum bin

fromBabbageDatum
    :: Ledger.Datum (BabbageEra StandardCrypto)
    -> Datum
fromBabbageDatum = \case
    Ledger.NoDatum -> NoDatum
    Ledger.DatumHash ref -> Reference (Left ref)
    Ledger.Datum bin -> Inline (Right bin)

getBinaryData
    :: Datum
    -> Maybe BinaryData
getBinaryData = \case
    NoDatum -> Nothing
    Reference (Right bin) -> Just bin
    Reference{} -> Nothing
    Inline (Right bin) -> Just bin
    Inline Left{} -> Nothing

hashDatum
    :: Datum
    -> Maybe DatumHash
hashDatum = \case
    NoDatum -> Nothing
    Reference (Left ref) -> Just ref
    Reference (Right bin) -> Just (hashBinaryData bin)
    Inline (Left ref) -> Just ref
    Inline (Right bin) -> Just (hashBinaryData bin)
