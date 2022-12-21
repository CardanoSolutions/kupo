module Kupo.Data.Cardano.ScriptReference where

import Kupo.Prelude

import Kupo.Data.Cardano.Script
    ( Script
    , hashScript
    )
import Kupo.Data.Cardano.ScriptHash
    ( ScriptHash
    )

data ScriptReference
    = NoScript
    | ReferencedScript !ScriptHash
    | InlineScript !Script
    deriving (Eq, Show)

mkScriptReference
    :: Maybe Script
    -> ScriptReference
mkScriptReference =
    maybe NoScript InlineScript

hashScriptReference
    :: ScriptReference
    -> Maybe ScriptHash
hashScriptReference = \case
    NoScript ->
        Nothing
    ReferencedScript hash ->
        Just hash
    InlineScript script ->
        Just (hashScript script)
