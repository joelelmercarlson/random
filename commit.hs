#!/usr/bin/env stack
{- stack --system-ghc
--resolver lts-20.20 script
--package process
--package time
-}
module Main (main) where

import System.Environment
import System.Process

import Data.Maybe
import Data.Time.Clock.POSIX

main :: IO ()
main = do
  xs <- getArgs
  tm <- round `fmap` getPOSIXTime

  let cmd = "git commit -m \"" ++ fromMaybe (tt tm) (nth 1 xs) ++ "\""

  a <- callCommand "git pull"
  _ <- callCommand "git add ."
  _ <- callCommand cmd
  b <- callCommand "git push"
  print $ a
  print $ b

-- | nth safe chooser
nth :: Int -> [a] -> Maybe a
nth _ []     = Nothing
nth 1 (x:_)  = Just x
nth n (_:xs) = nth (n - 1) xs

tt :: Integer -> String
tt n = "+ " ++ show n
