{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Core.Program

main :: IO ()
main = execute $ do
    write "Hello World"
