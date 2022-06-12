{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import Core.Program
import Core.Telemetry
import Core.Text

version :: Version
version = $(fromPackage)

config :: Config
config =
    complexConfig
        [ Global
            [ Option
                "trace-file"
                Nothing
                (Value "FILENAME")
                [quote|
File to record the trace and root span identifier in. The default filename is
.trace in the current working directory. If you override it when initializing
you will need to use the same file for all subsequent invocations making up
this procedure.
                |]
            ]
        , Command
            "init"
            [quote|
Create a trace and root span identifier and mark the start time of your trace.
            |]
            []
        , Command
            "exec"
            [quote|
Wrap the execution of a command in a span, recording its start time and duration.
            |]
            [ Argument
                "label"
                [quote|
Name for the span that will enclose the command you are running. By convention
the label used for the step is the name of the program, script, or function
your are executing, rather than a descriptive string.
                |]
            , Argument
                "command"
                [quote|
The program or script to run.
                |]
            , Remaining
                [quote|
Command-line arguments to be passed to the program being executed, if any.
                |]
            ]
        , Command
            "send"
            [quote|
Send the root span, finishing the corresponding trace.
                    |]
            [ Argument
                "label"
                [quote|
Name for the root span. By convention this is the identifier or codename for
the process being undertaken.
                |]
            ]
        ]

main :: IO ()
main = do
    context <-
        configure
            version
            None
            config

    context' <-
        initializeTelemetry
            [consoleExporter, honeycombExporter]
            context

    executeWith context' program

program :: Program None ()
program = do
    write "Hello World"
