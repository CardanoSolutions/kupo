--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.Http.Response
    ( responseJson
    , responseJsonEncoding
    , responseStreamJson
    ) where

import Kupo.Prelude

import Data.IORef
    ( mkWeakIORef )
import GHC.Weak
    ( finalize )
import Network.HTTP.Types.Header
    ( Header, hContentLength )
import Network.HTTP.Types.Status
    ( status200 )
import Network.Wai
    ( Response, responseLBS, responseStream )

import qualified Data.Aeson as Json
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Types.Status as Http

responseJsonEncoding
    :: Http.Status
    -> [Header]
    -> Json.Encoding
    -> Response
responseJsonEncoding status headers a =
    let
        bytes = B.toLazyByteString $ Json.fromEncoding a
        len = BL.length bytes
        contentLength = ( hContentLength, encodeUtf8 (show @Text len) )
     in
        responseLBS status (contentLength : headers) bytes
{-# INLINEABLE responseJsonEncoding #-}

responseJson
    :: ToJSON a
    => Http.Status
    -> [Header]
    -> a
    -> Response
responseJson status headers a =
    responseJsonEncoding status headers (Json.toEncoding a)
{-# INLINEABLE responseJson #-}

responseStreamJson
    :: [Header]
    -> (a -> Json.Encoding)
    -> ((a -> IO ()) -> IO () -> IO ())
    -> Response
responseStreamJson headers encode callback = do
    responseStream status200 headers $ \write flush -> do
        ref <- newIORef True
        weak <- mkWeakIORef ref flush
        write openBracket
        callback
            (\a -> do
                isFirstResult <- readIORef ref
                write (separator isFirstResult <> Json.fromEncoding (encode a))
                writeIORef ref False
            )
            (write closeBracket >> finalize weak)
  where
    openBracket = B.putCharUtf8 '['
    closeBracket = B.putCharUtf8 ']'
    separator isFirstResult
        | isFirstResult = mempty
        | otherwise     = B.putCharUtf8 ','
