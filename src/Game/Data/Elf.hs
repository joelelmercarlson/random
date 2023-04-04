{-# LANGUAGE OverloadedStrings #-}
{-

Game.Data.Elf.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Data.Elf (genElf) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Game.DiceSet as DS
import Game.Factory.Character
import Util

-- | elf know thy self
genElf :: Int -> Character
genElf s = let
  gen = genders (DS.d10 s)
  in Character {
  seed = s
  , gender = gen
  , rStr = DS.d3 (s+1) + DS.d4 (s+2) + DS.d5 (s+3) + DS.d6 (s+4)
  , rDex = DS.d3 (s+5) + DS.d4 (s+6) + DS.d5 (s+7) + DS.d6 (s+8)
  , rCon = DS.d3 (s+9) + DS.d4 (s+10) + DS.d5 (s+11) + DS.d6 (s+12)
  , rInt = DS.d3 (s+13) + DS.d4 (s+14) + DS.d5 (s+15) + DS.d6 (s+16)
  , rWis = DS.d3 (s+17) + DS.d4 (s+18) + DS.d5 (s+19) + DS.d6 (s+20)
  , rCha = DS.d3 (s+21) + DS.d4 (s+22) + DS.d5 (s+23) + DS.d6 (s+24)
  , tStr = 0
  , tDex = 2
  , tCon = 0
  , tInt = 1
  , tWis = 0
  , tCha = 0
  , eAC    = 10
  , race   = "Elf"
  , age    = 30 + DS.d100 (s+25)
  , eye    = pickEyes (DS.d20 (s+26))
  , hair   = pickHairs (DS.d20 (s+27))
  , height = 60 + DS.d12 (s+28)
  , mark   = "nil"
  , name   = names gen (DS.d20 (s+29)) female male
  }

pickEyes :: Int -> Text
pickEyes x
  | x == 2 = "Jet"
  | x == 3 = "Amethyst"
  | x == 4 = "Aquamarine"
  | x >= 5 && x <= 7   = "Sapphire"
  | x >= 8 && x <= 11  = "Turquoise"
  | x >= 12 && x <= 14 = "Emerald"
  | x >= 15 && x <= 17 = "Amber"
  | x == 18 = "Copper"
  | x == 19 = "Citrine"
  | x == 20 = "Gold"
  | otherwise = "nil"

female :: [Text]
female = [ "Alane"
         , "Altronia"
         , "Davandrel"
         , "Eldril"
         , "Eponia"
         , "Fanriel"
         , "Filamir"
         , "Gallina"
         , "Halion"
         , "Illudil"
         , "Ionor"
         , "Lindara"
         , "Lorandara"
         , "Maruviel"
         , "Pelgrana"
         , "Siluvaine"
         , "Tallana"
         , "Ulliana"
         , "Vivandrel"
         , "Yuviel"
         ]

pickHairs :: Int -> Text
pickHairs x
  | x == 2 = "Silver"
  | x == 3 = "White"
  | x == 4 = "Pale Blond"
  | x >= 5 && x <= 7   = "Blond"
  | x >= 8 && x <= 11  = "Yellow Blond"
  | x >= 12 && x <= 14 = "Copper Blond"
  | x >= 15 && x <= 17 = "Red Blond"
  | x == 18 = "Auburn"
  | x == 19 = "Red"
  | x == 20 = "Black"
  | otherwise = "nil"

male :: [Text]
male = [ "Aluthol"
       , "Aamendil"
       , "Angran"
       , "Cavindel"
       , "Dolwen"
       , "Eldillor"
       , "Falandar"
       , "Farnoth"
       , "Gildiril"
       , "Harrond"
       , "Imhol"
       , "Larandar"
       , "Laurenor"
       , "Mellion"
       , "Mormacar"
       , "Ravandil"
       , "Torendil"
       , "Urdithane"
       , "Valahuir"
       , "Yavandir"
       ]
