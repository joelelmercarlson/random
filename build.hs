#!/usr/bin/env stack
-- stack script --system-ghc --resolver lts-11.10 --package "process"
module Main where

  import System.Environment
  import System.Exit
  import System.IO
  import System.Process

  main :: IO ()
  main = do
    run <- callCommand "stack build"
    run <- callCommand "stack exec Main"
    print run
