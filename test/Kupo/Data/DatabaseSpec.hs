-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.DatabaseSpec
    ( spec
    ) where

import Kupo.Prelude

import Data.Maybe
    ( fromJust )
import Database.SQLite.Simple
    ( Connection
    , Only (..)
    , Query (..)
    , SQLData (..)
    , executeMany
    , execute_
    , query_
    , withConnection
    , withTransaction
    )
import Kupo.Configuration
    ( StandardCrypto )
import Kupo.Data.Cardano
    ( Address, addressFromBytes, addressToBytes )
import Kupo.Data.Database
    ( patternToQueryLike
    , pointFromRow
    , pointToRow
    , resultFromRow
    , resultToRow
    )
import Kupo.Data.Generators
    ( genNonGenesisPoint, genResult )
import Kupo.Data.Pattern.Fixture
    ( addresses, patterns )
import Test.Hspec
    ( Spec, around, context, parallel, shouldBe, specify )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Gen, Property, counterexample, forAllBlind )

spec :: Spec
spec = parallel $ do
    context "fromRow ↔ toRow" $ do
        prop "Result" $
            roundtripFromToRow genResult resultToRow resultFromRow
        prop "Checkpoint" $
            roundtripFromToRow genNonGenesisPoint pointToRow pointFromRow

    context "patternToQueryLike" $ around withFixtureDatabase $ do
        forM_ patterns $ \(_, p, results) -> do
            let like = patternToQueryLike p
            specify (toString like) $ \conn -> do
                rows <- query_ conn $ "SELECT address, LENGTH(address) as len \
                                      \FROM addresses \
                                      \WHERE address " <> Query like
                sort (rowToAddress <$> rows) `shouldBe` sort results

--
-- Properties
--

roundtripFromToRow
    :: forall a row.
        ( Show a
        , Show row
        , Eq a
        )
    => Gen a
    -> (a -> row)
    -> (row -> a)
    -> Property
roundtripFromToRow genA toRow fromRow =
    forAllBlind genA $ \a ->
        let row = toRow a in fromRow row == a
        & counterexample ("Row: "  <> show row)
        & counterexample ("Got:  " <> show (fromRow row))
        & counterexample ("Want: " <> show a)

--
-- Helpers
--

withFixtureDatabase :: (Connection -> IO ()) -> IO ()
withFixtureDatabase action = withConnection ":memory:" $ \conn -> do
    withTransaction conn $ do
        execute_ conn
            "CREATE TABLE IF NOT EXISTS addresses (\
            \  address TEXT NOT NULL\
            \)"
        executeMany conn
            "INSERT INTO addresses VALUES (?)"
            (Only . SQLText . encodeBase16 . addressToBytes <$> addresses)
    action conn

rowToAddress :: HasCallStack => [SQLData] -> Address StandardCrypto
rowToAddress = \case
    [SQLText txt, _] ->
        fromJust (addressFromBytes (unsafeDecodeBase16 txt))
    _ ->
        error "rowToAddress: not SQLText"
