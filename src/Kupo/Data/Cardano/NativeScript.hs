{-# LANGUAGE PatternSynonyms #-}

module Kupo.Data.Cardano.NativeScript
    ( NativeScript
    , pattern Ledger.Shelley.RequireSignature
    , pattern Ledger.Shelley.RequireAllOf
    , pattern Ledger.Shelley.RequireAnyOf
    , pattern Ledger.Shelley.RequireMOf
    , pattern Ledger.Allegra.RequireTimeExpire
    , pattern Ledger.Allegra.RequireTimeStart
    , KeyHash(..)
    ) where

import Kupo.Prelude

import Cardano.Ledger.Keys
    ( KeyHash (..)
    )

import qualified Cardano.Ledger.Shelley.Scripts as Ledger.Shelley
import qualified Cardano.Ledger.Allegra.Scripts as Ledger.Allegra

type NativeScript = Ledger.Allegra.Timelock (ConwayEra StandardCrypto)
