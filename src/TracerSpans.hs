{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TracerSpans where

import Core.Data
import Core.Program
import Core.System
import Core.Telemetry
import Core.Text
import Data.Word (Word16)
import System.Exit (ExitCode (..))
import System.Process.Typed (nullStream, proc, runProcess, setStdin)
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
            executeChildProcess path
        "send" -> do
            finalizeRootSpan path
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

data TraceExecutionProblem
    = ChildProcessFailed Int
    deriving (Show)

instance Exception TraceExecutionProblem

executeChildProcess :: FilePath -> Program None ()
executeChildProcess path = do
    state <- readTraceFile path
    let trace = stateTraceIdentifier state
        parent = stateParentIdentifier state

    label <- queryArgument "label"
    command <- queryArgument "command"
    args <- queryRemaining

    debug "command" command
    debugS "args" args

    usingTrace trace parent $ do
        encloseSpan label $ do
            let task = proc (fromRope command) (fmap fromRope args)
                task' = setStdin nullStream task

            exit <- liftIO $ do
                runProcess task'

            debugS "exit" exit
            case exit of
                ExitSuccess -> pure ()
                ExitFailure n -> throw (ChildProcessFailed n)

finalizeRootSpan :: FilePath -> Program None ()
finalizeRootSpan path = do
    state <- readTraceFile path
    let start = stateStartTime state
        trace = stateTraceIdentifier state
        parent = stateParentIdentifier state

    label <- queryArgument "label"

    usingTrace' trace $ do
        encloseSpan label $ do
            setIdentifierSpan parent
            setStartTime start
