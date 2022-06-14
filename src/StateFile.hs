{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module StateFile where

import Core.Data
import Core.Encoding.Json
import Core.Program hiding (Datum (..))
import Core.System
import Core.Text
import Data.ByteString qualified as B
import Data.Int (Int64)
import Data.Scientific
import System.IO (withBinaryFile)

data TraceState = TraceState
    { stateStartTime :: Time
    , stateTraceIdentifier :: Trace
    , stateParentIdentifier :: Span
    }

convertStateToJson :: TraceState -> Rope
convertStateToJson (TraceState start trace parent) =
    let object =
            JsonObject
                ( intoMap
                    [
                        ( JsonKey "start"
                        , JsonNumber (fromIntegral (fromTime start :: Int64))
                        )
                    ,
                        ( JsonKey "trace"
                        , JsonString (unTrace trace)
                        )
                    ,
                        ( JsonKey "parent"
                        , JsonString (unSpan parent)
                        )
                    ]
                )
     in encodeToRope object

convertJsonToState :: Bytes -> Either Rope TraceState
convertJsonToState bytes =
    let result = decodeFromUTF8 bytes
     in case result of
            Nothing -> Left "Decode failed"
            Just (JsonObject kvs) -> do
                start <- case lookupKeyValue "start" kvs of
                    Just (JsonNumber value) -> case toBoundedInteger value :: Maybe Int64 of
                        Nothing -> Left "Unable to parse number"
                        Just ticks -> Right (intoTime ticks)
                    _ -> Left "Missing or invalid 'start' key"

                trace <- case lookupKeyValue "trace" kvs of
                    Just (JsonString value) -> Right (Trace value)
                    _ -> Left "Missing or invalid 'trace' key"

                parent <- case lookupKeyValue "parent" kvs of
                    Just (JsonString value) -> Right (Span value)
                    _ -> Left "Missing or invalid 'parent' key"

                pure
                    ( TraceState
                        { stateStartTime = start
                        , stateTraceIdentifier = trace
                        , stateParentIdentifier = parent
                        }
                    )
            Just _ -> Left "Invalid JSON"

data TraceStateProblem
    = InvalidStateFile Rope
    deriving (Show)

instance Exception TraceStateProblem

writeTraceFile :: FilePath -> TraceState -> Program None ()
writeTraceFile path state = do
    let blob = convertStateToJson state
    liftIO $ do
        withBinaryFile
            path
            WriteMode
            ( \handle -> do
                hWrite handle blob
            )

readTraceFile :: FilePath -> Program None TraceState
readTraceFile path = do
    contents <- liftIO $ do
        withBinaryFile
            path
            ReadMode
            ( \handle -> do
                B.hGetContents handle
            )
    state <- case convertJsonToState (intoBytes contents) of
        Left problem -> throw (InvalidStateFile problem)
        Right value -> pure value

    pure state
