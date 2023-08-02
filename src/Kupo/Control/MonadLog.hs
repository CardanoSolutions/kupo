--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Control.MonadLog
    ( -- * Class
      MonadLog (..)
    , Logger

      -- * Severity
    , Severity (..)
    , HasSeverityAnnotation (..)

      -- * Tracer
    , Tracer
    , natTracer
    , nullTracer
    , contramap
    , traceWith

      -- * Tracers
    , TracerDefinition (..)
    , TracerHKD
    , defaultTracers
    , configureTracers
    , withTracers

      -- * Progress
    , TraceProgress (..)
    ) where

import Kupo.Prelude

import Control.Concurrent
    ( ThreadId
    , myThreadId
    )
import Control.Monad.Class.MonadTime
    ( UTCTime
    , getCurrentTime
    )
import Control.Tracer
    ( Tracer (..)
    , natTracer
    , nullTracer
    , traceWith
    )
import Data.Aeson.Encoding
    ( fromEncoding
    , pair
    , pairs
    )
import Data.Char
    ( isLower
    )
import Data.Generics.Tracers
    ( IsRecordOfTracers
    , SomeMsg (..)
    , TracerDefinition (..)
    , TracerHKD
    , configureTracers
    , defaultTracers
    )
import Data.Severity
    ( HasSeverityAnnotation (..)
    , Severity (..)
    )
import Data.Time
    ( defaultTimeLocale
    , formatTime
    , getCurrentTimeZone
    , utcToZonedTime
    )
import Kupo.Control.MonadSTM
    ( TMVar
    , newTMVarIO
    , withTMVar
    )
import System.IO
    ( BufferMode (..)
    , hFlush
    , hSetBuffering
    , hSetEncoding
    , utf8
    )

import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Key as Json.Key
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import qualified Data.Text.Internal.Builder as T
import qualified Data.Text.Lazy.IO as T
import qualified Text.Builder.ANSI as Ansi

class Monad m => MonadLog (m :: Type -> Type) where
    logWith :: Logger msg -> msg -> m ()

type Logger = Tracer IO

instance MonadLog IO where
    logWith = traceWith

instance MonadLog m => MonadLog (ReaderT env m) where
    logWith tr = lift . logWith tr

type AppVersion = Text

type Writer =
    Handle -> AppVersion -> ThreadId -> UTCTime -> SomeMsg -> IO ()

-- | Acquire and configure multiple tracers which outputs structured JSON on the
-- standard output. The tracer is concurrent-safe but none buffered, while it is
-- okay for a vast majority of applications, it also relies on a simple
-- implementation and does not perform any caching or hardcore optimizations;
-- For example timestamps are computed on-the-fly for every log messages.
--
withTracers
    :: forall tracers. (IsRecordOfTracers tracers IO)
    => Handle
        -- ^ Handle to which forward logs
    -> AppVersion
        -- ^ Extra information to embed in the logging envelope.
    -> tracers IO 'MinSeverities
        -- ^ A configuration of tracers.
    -> (tracers IO 'Concrete -> IO ())
        -- ^ Callback with acquired and configured tracers.
    -> IO ()
withTracers h version tracers action = do
    hSetBuffering h LineBuffering
    hSetEncoding h utf8
    lock <- newTMVarIO ()
    mkEnvelop <- hSupportsANSI h <&> \case
        True -> mkAnsiEnvelop
        False -> mkJsonEnvelop
    action (configureTracers tracers (tracer mkEnvelop lock))
  where
    tracer
        :: Writer
        -> TMVar IO ()
        -> Tracer IO SomeMsg
    tracer say lock = Tracer $ \someMsg@(SomeMsg minSeverity _ msg) -> do
        let severity = getSeverityAnnotation msg
        when (severity >= minSeverity) $ liftIO $ withTMVar lock $ \() -> do
            threadId <- myThreadId
            timestamp <- getCurrentTime
            say h version threadId timestamp someMsg

-- | Some structured JSON logs when redirecting to files
mkJsonEnvelop :: Writer
mkJsonEnvelop h version extThreadId timestamp (SomeMsg _ tracerName msg) =
    B.hPutBuilder h $ (<> B.charUtf8 '\n') $ fromEncoding $ pairs $ mempty
        <> pair "severity"  (toEncoding severity)
        <> pair "timestamp" (toEncoding timestamp)
        <> pair "thread"    (toEncoding threadId)
        <> pair "message"   (pairs $ pair context (toEncoding msg))
        <> pair "version"   (toEncoding version)
  where
    context = Json.Key.fromString (dropWhile isLower tracerName)
    threadId = T.drop 9 (show extThreadId)
    severity = getSeverityAnnotation msg

-- | Pretty colored logs to show in the terminal
mkAnsiEnvelop :: Writer
mkAnsiEnvelop h _version threadId utcTimestamp (SomeMsg _ tracerName msg) = do
    timestamp <- utcToZonedTime <$> getCurrentTimeZone <*> pure utcTimestamp
    if "tracerProgress" == tracerName && subject /= "Done" then do
        T.hPutStr h $ T.toLazyText $ mconcat
            [ mkTime timestamp
            , severity
            , context
            , accent details
            ]
        hFlush h
        hSetCursorColumn h 0
        hClearLine h
    else
        T.hPutStrLn h $ T.toLazyText $ mconcat
            [ mkTime timestamp
            , severity
            , context
            , accent details
            ]
  where
    mkTime timestamp =
        mconcat
        [ Ansi.whiteBg $ Ansi.black $
            T.fromString (formatTime defaultTimeLocale "%X%3Q" timestamp) <> " "
        , Ansi.white "\57520"
        ]

    context =
        let name = T.fromString $ dropWhile isLower tracerName
            id = T.fromText $ T.drop 9 $ show threadId
         in accent $ " " <> id <> " ❭ " <> (Ansi.bold (name <> accent "/" <> Ansi.bold (accent subject) <> " "))

    (subject, details) =
        let
            str = msg
                & toEncoding
                & Json.encodingToLazyByteString
                & decodeUtf8
            details_ =
                str
                & T.dropWhile (/= ',')
                & T.drop 1
                & (\txt -> if T.null txt then "" else "{" <> txt)
                & T.fromText
            subject_ = str
                & T.takeWhile (/= ',')
                & (\txt -> if str == txt then T.dropEnd 1 txt else txt)
                & T.dropWhile (/= ':')
                & T.drop 3
                & T.dropWhile isLower
                & T.dropEnd 1
                & T.fromText
         in
            (subject_, details_)

    (severity, accent) = case getSeverityAnnotation msg of
        Debug ->
            ( Ansi.whiteBg (Ansi.black "\57520" <> Ansi.bold (Ansi.white " + "))
                <> Ansi.white "\57520"
            , Ansi.white
            )
        Info ->
            ( Ansi.blueBg (Ansi.black "\57520" <> Ansi.bold (Ansi.white " ℹ "))
                <> Ansi.blue "\57520"
            , Ansi.blue
            )
        Notice ->
            ( Ansi.magentaBg (Ansi.black "\57520" <> Ansi.bold (Ansi.white " ★ "))
                <> Ansi.magenta "\57520"
            , Ansi.magenta
            )
        Warning ->
            ( Ansi.yellowBg (Ansi.black "\57520" <> Ansi.bold (Ansi.white " ! "))
                <> Ansi.yellow "\57520"
            , Ansi.yellow
            )
        Error ->
            ( Ansi.redBg (Ansi.black "\57520" <> Ansi.bold (Ansi.white " ✖ "))
                <> Ansi.red "\57520"
            , Ansi.red
            )

--
-- Progress
--

data TraceProgress where
    ProgressStep
        :: { progress :: Text }
        -> TraceProgress
    ProgressDone
        :: TraceProgress
    deriving stock (Generic, Show)

instance ToJSON TraceProgress where
    toEncoding =
        defaultGenericToEncoding

instance HasSeverityAnnotation TraceProgress where
    getSeverityAnnotation = \case
        ProgressStep{} -> Info
        ProgressDone{} -> Info
