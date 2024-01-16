{-# LANGUAGE PatternSynonyms #-}

module Kupo.Data.Cardano.NativeScript
    ( NativeScript
    , pattern Ledger.Allegra.RequireSignature
    , pattern Ledger.Allegra.RequireAllOf
    , pattern Ledger.Allegra.RequireAnyOf
    , pattern Ledger.Allegra.RequireMOf
    , pattern Ledger.Allegra.RequireTimeExpire
    , pattern Ledger.Allegra.RequireTimeStart
    , KeyHash(..)
    ) where

import Kupo.Prelude

import Cardano.Ledger.Keys
    ( KeyHash (..)
    )

import qualified Cardano.Ledger.Allegra.Scripts as Ledger.Allegra

type NativeScript = Ledger.Allegra.Timelock (ConwayEra StandardCrypto)
