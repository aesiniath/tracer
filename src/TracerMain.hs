{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import Core.Program
import Core.Telemetry

version :: Version
version = $(fromPackage)

main :: IO ()
main = do
    context <-
        configure
            version
            None
            ( complexConfig
                [ Global
                    [ Option
                        "trace-file"
                        Nothing
                        (Value "FILENAME")
                        "File to record the trace and root span identifier in. The default filename is .trace in the current working directory."
                    ]
                , Command
                    "init"
                    "Create a trace and root span identifier and mark the start time of your trace."
                    [ Argument
                        "label"
                        "Name for the root span."
                    ]
                , Command
                    "exec"
                    "Wrap the execution of a command in a span, recording its start time and duration."
                    [ Argument
                        "label"
                        "Name for the span that will enclose the command you are running."
                    , Remaining "The command and arguments to run"
                    ]
                , Command
                    "send"
                    "Send the root span, finishing the corresponding trace."
                    [ Argument
                        "label"
                        "Name for the root span."
                    ]
                ]
            )
    context' <- initializeTelemetry [consoleExporter, honeycombExporter] context
    executeWith context' program

program :: Program None ()
program = do
    write "Hello World"
