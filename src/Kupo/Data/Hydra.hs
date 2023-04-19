module Kupo.Data.Hydra where

import Kupo.Prelude

import Data.Aeson
    ( (.:)
    )
import qualified Data.Aeson.Types as Json

data HydraMessage
  = SnapshotConfirmed
  | SomethingElse


-- Decoders

decodeHydraMessage :: Json.Value -> Json.Parser HydraMessage
decodeHydraMessage =
  Json.withObject "HydraMessage" $ \o -> do
    tag <- o .: "tag"
    case tag of
      ("SnapshotConfirmed" :: Text) -> pure SnapshotConfirmed
      _ -> pure SomethingElse
