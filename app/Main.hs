{-# LANGUAGE OverloadedStrings #-}
{-

Main.hs

Main.hs creates Character '@' for Arrow. Allows easy Experiment
with different Race, Class, Stats, et cetra.

<https://github.com/joelelmercarlson/arrow>

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Main (main) where

import Control.Monad.Random (getStdGen)
import Data.Maybe
import qualified Data.Text as T
import System.Environment

import Display
import Game.Factory.Character
import Game.Data.Dwarf
import Game.Data.Elf
import Game.Data.Halfling
import Game.Data.Human
import Game.DiceSet (d1000)
import Util (nth)

main :: IO ()
main = do
  gen <- getStdGen
  xs <- getArgs
  let (s, _) = d1000 gen
      action = fromMaybe "None" $ nth 1 xs
      job    = T.pack $ fromMaybe "None" $ nth 2 xs
  case action of
    "dwarf"    -> toEntityKind job $ genDwarf s
    "elf"      -> toEntityKind job $ genElf s
    "halfling" -> toEntityKind job $ genHalfling s
    "human"    -> toEntityKind job $ genHuman s
    _          -> toEntityKind job $ genHuman s
