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
import Game.Data.Dwarf
import Game.Data.Elf
import Game.Data.Halfling
import Game.Data.Human
import qualified Game.DiceSet as DS
import Game.Factory.Character
import Game.Factory.Player
import Util (nth)

main :: IO ()
main = do
  gen <- getStdGen
  xs <- getArgs
  let (s, _) = DS.d1000 gen
      action = speciesFmt $ T.pack $ fromMaybe "None" $ nth 1 xs
      job    = classFmt   $ T.pack $ fromMaybe "None" $ nth 2 xs

  case action of
    "Dwarf"    -> toEntityKind job $ genDwarf s
    "Elf"      -> toEntityKind job $ genElf s
    "Halfling" -> toEntityKind job $ genHalfling s
    "Human"    -> toEntityKind job $ genHuman s
    _          -> toEntityKind job $ genHuman s
