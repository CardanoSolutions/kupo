--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.App.ChainSync.Hydra
    ( connect
    , runChainSyncClient
    ) where

import Kupo.Prelude

import Control.Exception.Safe
    ( MonadThrow
    )
import Kupo.App.Mailbox
    ( Mailbox
    , putHighFrequencyMessage
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Data.Cardano
    ( Point
    , Tip
    )
import Kupo.Data.ChainSync
    ( IntersectionNotFoundException (..)
    )
import Kupo.Data.Ogmios
    ( PartialBlock
    )

import Kupo.Data.Hydra
    ( HydraMessage (..)
    , decodeHydraMessage
    , fromSnapshot
    )
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Json as WS

runChainSyncClient
    :: forall m.
        ( MonadIO m
        , MonadSTM m
        , MonadThrow m
        )
    => Mailbox m (Tip, PartialBlock) (Tip, Point)
    -> m () -- An action to run before the main loop starts.
    -> [Point]
    -> WS.Connection
    -> m IntersectionNotFoundException
runChainSyncClient mailbox beforeMainLoop _pts ws = do
    beforeMainLoop
    forever $ do
        WS.receiveJson ws decodeHydraMessage >>= \case
            SnapshotConfirmed{ snapshot } -> do
                atomically (putHighFrequencyMessage mailbox (fromSnapshot snapshot))
            SomethingElse -> pure ()

connect
    :: ConnectionStatusToggle IO
    -> String
    -> Int
    -> (WS.Connection -> IO a)
    -> IO a
connect ConnectionStatusToggle{toggleConnected} host port action =
    WS.runClientWith host port "/"
        WS.defaultConnectionOptions [] (\ws -> toggleConnected >> action ws)
