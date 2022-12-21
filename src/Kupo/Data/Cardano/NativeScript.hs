{-# LANGUAGE PatternSynonyms #-}

module Kupo.Data.Cardano.NativeScript
    ( NativeScript
    , pattern Ledger.RequireSignature
    , pattern Ledger.RequireAllOf
    , pattern Ledger.RequireAnyOf
    , pattern Ledger.RequireMOf
    , pattern Ledger.RequireTimeExpire
    , pattern Ledger.RequireTimeStart
    , KeyHash(..)
    ) where

import Kupo.Prelude

import Cardano.Ledger.Keys
    ( KeyHash (..)
    )

import qualified Cardano.Ledger.ShelleyMA.Timelocks as Ledger

type NativeScript = Ledger.Timelock StandardCrypto
