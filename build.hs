#!/usr/bin/env stack
-- stack script --system-ghc --resolver lts-9.18 --package "process"
  import System.Environment
  import System.Exit
  import System.IO
  import System.Process 

  main :: IO ()
  main = do
    r <- callCommand "stack build"
    run <- callCommand "stack exec rpg dwarf"
    run <- callCommand "stack exec rpg hit"
    run <- callCommand "stack exec rpg wound"
    putStrLn $ "build: " ++ show r
