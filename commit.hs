#!/usr/bin/env stack
-- stack script --system-ghc --resolver lts-9.18 --package "process" --package "time"
module Main where

  import System.Environment
  import System.Exit
  import System.IO
  import System.Process 

  import Data.Maybe
  import Data.Time.Clock.POSIX

  main :: IO ()
  main = do
    xs <- getArgs
    tm <- round `fmap` getPOSIXTime

    let cmd = "git commit -m \"" ++ fromMaybe (tt tm) (nth 1 xs) ++ "\""

    run <- callCommand "git status"
    run <- callCommand "git add ."
    run <- callCommand cmd
    run <- callCommand "git push"
    print run

  nth :: Int -> [a] -> Maybe a
  nth _ []     = Nothing
  nth 1 (x:_)  = Just x
  nth n (x:xs) = nth (n - 1) xs

  tt :: Integer -> String
  tt n = "+ " ++ show n 
