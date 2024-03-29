{-# LANGUAGE OverloadedStrings #-}
{-

Game.Data.Halfling.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Data.Dwarf (genDwarf) where

import Data.Text (Text)
import qualified Game.DiceSet as DS
import Game.Factory.Character
import Util

-- | dwarf know thy self
genDwarf :: Int ->  Character
genDwarf s = let
  gen = genders (DS.d10 s)
  in Character {
  seed = s
  , gender = gen
  , rStr = DS.d6 (s+1) + DS.d6 (s+2)
  , rDex = DS.d6 (s+3) + DS.d6 (s+4)
  , rInt = DS.d6 (s+5) + DS.d6 (s+6)
  , tStr = 2
  , tDex = 0
  , tInt = 0
  , race   = "Dwarf"
  , age    = 20 + DS.d100 (s+25)
  , eye    = pickEyes (DS.d20 (s+26))
  , hair   = pickHairs (DS.d20 (s+27))
  , height = 50 + DS.d10 (s+28)
  , mark   = pick (DS.d20 (s+29)) marks
  , name   = names gen (DS.d20 (s+30)) female male
  }

pickEyes :: Int -> Text
pickEyes x
  | x == 2 = "Coal"
  | x == 3 = "Lead"
  | x == 4 = "Steel"
  | x >= 5 && x <= 7   = "Blue"
  | x >= 8 && x <= 11  = "Earth Brown"
  | x >= 12 && x <= 14 = "Dark Brown"
  | x >= 15 && x <= 17 = "Hazel"
  | x == 18 = "Green"
  | x == 19 = "Coper"
  | x == 20 = "Gold"
  | otherwise = "nil"

female :: [Text]
female = [ "Anika"
        , "Asta"
        , "Astrid"
        , "Berta"
        , "Brigit"
        , "Dagmar"
        , "Elsa"
        , "Erika"
        , "Fanziska"
        , "Greta"
        , "Hunni"
        , "Ingrid"
        , "Janna"
        , "Karin"
        , "Petra"
        , "Sigrid"
        , "Sigrun"
        , "Silma"
        , "Thylda"
        , "Ulla"
        ]

pickHairs :: Int -> Text
pickHairs x
  | x == 2 = "White"
  | x == 3 = "Grey"
  | x == 4 = "Pale Blond"
  | x >= 5 && x <= 7   = "Golden"
  | x >= 8 && x <= 11  = "Copper"
  | x >= 12 && x <= 14 = "Bronze"
  | x >= 15 && x <= 17 = "Brown"
  | x == 18 = "Dark Brown"
  | x == 19 = "Reddish Brown"
  | x == 20 = "Black"
  | otherwise = "nil"

male :: [Text]
male = [ "Bardin"
      , "Brokk"
      , "Dimzad"
      , "Durak"
      , "Garil"
      , "Gottri"
      , "Grundi"
      , "Hargin"
      , "Imrak"
      , "Kargun"
      , "Jorunn"
      , "Magnar"
      , "Modrin"
      , "Nargond"
      , "Ozad"
      , "Ragnar"
      , "Snorri"
      , "Storri"
      , "Thingrim"
      , "Urgrim"
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
