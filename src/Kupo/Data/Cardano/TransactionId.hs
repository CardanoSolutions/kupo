module Kupo.Data.Cardano.TransactionId where

import Kupo.Prelude

import Cardano.Crypto.Hash
    ( hashFromTextAsHex
    )

import qualified Cardano.Chain.UTxO as Ledger.Byron
import qualified Cardano.Crypto as Ledger.Byron
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Data.Aeson as Json

-- TransactionId

class HasTransactionId (a :: Type) where
    getTransactionId :: a -> TransactionId

instance HasTransactionId Void where
    getTransactionId = absurd

instance HasTransactionId Ledger.TxIn where
    getTransactionId (Ledger.TxIn i _) = i

instance (HasTransactionId a) => HasTransactionId (a, b) where
    getTransactionId = getTransactionId . fst

type TransactionId =
    Ledger.TxId

transactionIdFromByron
    :: Ledger.Byron.TxId
    -> TransactionId
transactionIdFromByron (Ledger.Byron.hashToBytes -> bytes) =
    Ledger.TxId (Ledger.unsafeMakeSafeHash (UnsafeHash (toShort bytes)))

transactionIdFromHash
    :: Hash Blake2b_256 Ledger.EraIndependentTxBody
    -> TransactionId
transactionIdFromHash =
    Ledger.TxId . Ledger.unsafeMakeSafeHash
{-# INLINABLE transactionIdFromHash #-}

transactionIdToBytes :: TransactionId -> ByteString
transactionIdToBytes =
    (\(UnsafeHash h) -> fromShort h) . Ledger.extractHash . Ledger.unTxId
{-# INLINABLE transactionIdToBytes #-}

unsafeTransactionIdFromBytes
    :: HasCallStack
    => ByteString
    -> TransactionId
unsafeTransactionIdFromBytes =
    transactionIdFromHash . unsafeHashFromBytes @Blake2b_256
{-# INLINABLE unsafeTransactionIdFromBytes #-}

transactionIdToText :: TransactionId -> Text
transactionIdToText =
    encodeBase16 . (\(UnsafeHash h) -> fromShort h) . Ledger.extractHash . Ledger.unTxId
{-# INLINABLE transactionIdToText #-}

transactionIdFromText
    :: Text
    -> Maybe TransactionId
transactionIdFromText =
    fmap transactionIdFromHash . hashFromTextAsHex @Blake2b_256
{-# INLINABLE transactionIdFromText #-}

transactionIdToJson :: TransactionId -> Json.Encoding
transactionIdToJson =
    hashToJson . Ledger.extractHash . Ledger.unTxId
{-# INLINABLE transactionIdToJson #-}
