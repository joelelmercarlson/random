{-# LANGUAGE OverloadedStrings #-}
{-

Game.Data.Halfling.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Data.Halfling (genHalfling) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Game.DiceSet as DS
import Game.Factory.Character
import Util

-- | hobbit know thy self
genHalfling :: Int -> Character
genHalfling s = let
  gen = genders (DS.d10 s)
  in Character {
  seed = s
  , gender = gen
  , rStr = DS.d3 (s+1) + DS.d4 (s+2) + DS.d5 (s+3) + DS.d6 (s+4)
  , rDex = DS.d3 (s+5) + DS.d4 (s+6) + DS.d5 (s+7) + DS.d6 (s+8)
  , rCon = DS.d3 (s+9) + DS.d4 (s+10) + DS.d5 (s+11) + DS.d6 (s+12)
  , rWis = DS.d3 (s+17) + DS.d4 (s+18) + DS.d5 (s+19) + DS.d6 (s+20)
  , rCha = DS.d3 (s+21) + DS.d4 (s+22) + DS.d5 (s+23) + DS.d6 (s+24)
  , tStr = 0
  , tDex = 2
  , tCon = 0
  , tWis = 0
  , tCha = 0
  , eAC    = 10
  , race   = "Halfling"
  , age    = 15 + DS.d50 (s+25)
  , eye    = pickEyes (DS.d10 (s+26))
  , hair   = pickHairs (DS.d10 (s+27))
  , height = 35 + DS.d10 (s+28)
  , mark   = pick (DS.d20 (s+29)) marks
  , name   = names gen (DS.d20 (s+30)) female male
  }

pickEyes :: Int -> Text
pickEyes x
  | x == 2 = "Light Grey"
  | x == 3 = "Grey"
  | x == 4 = "Pale Blue"
  | x >= 5 && x <= 7   = "Blue"
  | x >= 8 && x <= 11  = "Green"
  | x >= 12 && x <= 14 = "Hazel"
  | x >= 15 && x <= 17 = "Brown"
  | x == 18 = "Copper"
  | x == 19 = "Dark Brown"
  | x == 20 = "Dark Brown"
  | otherwise = "nil"

female :: [Text]
female = [ "Cerasta Twofoot"
        , "Mahonia Fallohide"
        , "Amaryllis Townsend"
        , "Calaminth Rumble"
        , "Salva Gamwich"
        , "Sapphire Boffin"
        , "Malva Goodchild"
        , "Kalmia Longhole"
        , "Clematis Brown"
        , "Forsythia Proudfoot"
        , "Belba Greenholm"
        , "Poinsetta Gammidge"
        , "Dianthus Brownlock"
        , "Peona Labingi"
        , "Opal Boffin"
        , "Salva Meadows"
        , "Hanna Harfoot"
        , "Salvia Marsh"
        , "Peony Bunce"
        , "Cleoma Hornblower"
        ]

pickHairs :: Int -> Text
pickHairs x
  | x == 2 = "Grey"
  | x == 3 = "Flazen"
  | x == 4 = "Russet"
  | x >= 5 && x <= 7   = "Honey"
  | x >= 8 && x <= 11  = "Chestnut"
  | x >= 12 && x <= 14 = "Ginger"
  | x >= 15 && x <= 17 = "Mustard"
  | x == 18 = "Almond"
  | x == 19 = "Chocolate"
  | x == 20 = "Liquirice"
  | otherwise = "nil"

male :: [Text]
male = [ "Wilcome Gardner"
       , "Brocard Diggle"
       , "Haiduc Brown"
       , "Britius Burrow"
       , "Reginar Burrows"
       , "Hartgard Sandheaver"
       , "Reolus Harfoot"
       , "Porro Twofoot"
       , "Brice Gamgee"
       , "Pepin Goldworthy"
       , "Medard Burrow"
       , "Anno Banks"
       , "Turpin Burrows"
       , "Munderic Brockhouse"
       , "Dreux Stoor"
       , "Folmar Chubb"
       , "Orderic Stoor"
       , "Fortinbras Smallburrow"
       , "Jago Sandheaver"
       , "Isengar Brownlock"
       ]

marks :: [Text]
marks = [ "Pox Marks"
        , "Ruddy Faced"
        , "Scar"
        , "Tattoo"
        , "Earring"
        , "Ragged Ear"
        , "Nose Ring"
        , "Wart"
        , "Broken Nose"
        , "Missing Tooth"
        , "Snaggle Teeth"
        , "Lazy Eye"
        , "Missing Eyebrow(s)"
        , "Missing Digit"
        , "Distinctive Gait"
        , "Curious Smell"
        , "Huge Nose"
        , "Large Mole"
        , "Small Bald Patch"
        , "Strange Coloured Eye(s)"
        ]
