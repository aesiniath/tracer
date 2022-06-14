{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TracerSpans where

import Core.Data
import Core.Program hiding (Datum (..))
import Core.System (liftIO)
import Core.Telemetry
import Core.Text
import Data.Word (Word16)
import System.Random (randomIO)

import StateFile

program :: Program None ()
program = do
    path <-
        queryOptionValue "trace-file" >>= \case
            Just value -> pure (fromRope value)
            Nothing -> pure ".trace"

    command <- queryCommandName
    case command of
        "init" -> do
            prepareTraceFile path
        "exec" -> do
            executeChildProcess path "list-files" "ls" ["-l"]
        "send" -> do
            -- finalizeRootSpan
            undefined
        _ -> invalid

    pure ()

prepareTraceFile :: FilePath -> Program None ()
prepareTraceFile path = do
    beginTrace $ do
        trace <-
            getIdentifierTrace >>= \case
                Nothing -> invalid
                Just value -> pure value

        now <-
            liftIO $ do
                getCurrentTimeNanoseconds

        (rand :: Word16) <-
            liftIO $ do
                randomIO

        let unique = createIdentifierSpan now rand

        writeTraceFile path (TraceState now trace unique)

executeChildProcess :: FilePath -> Label -> Rope -> [Rope] -> Program None ()
executeChildProcess path label command args = do
    state <- readTraceFile path
    let trace = stateTraceIdentifier state
        parent = stateParentIdentifier state

    usingTrace trace parent $ do
        encloseSpan label $ do
            (exit, out, err) <- execProcess (command : args)
            debugS "exit" exit
            debug "out" out
            debug "err" err

finalizeRootSpan :: Label -> Program None ()
finalizeRootSpan label = do
    let state = undefined :: TraceState

    let start = stateStartTime state
        trace = stateTraceIdentifier state
        parent = stateParentIdentifier state

    usingTrace' trace $ do
        encloseSpan label $ do
            setIdentifierSpan parent
            setStartTime start
