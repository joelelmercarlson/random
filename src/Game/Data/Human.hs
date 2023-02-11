{-# LANGUAGE OverloadedStrings #-}
{-

Game.Data.Human.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Data.Human (genHuman) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Game.DiceSet as DS
import Game.Factory.Character
import Util

-- | human know thy self
genHuman :: Int -> Character
genHuman s = let
  gen = genders (DS.d10 s)
  in Character {
  seed = s
  , gender = gen
  , rStr = DS.d3 (s+1) + DS.d4 (s+2) + DS.d5 (s+3) + DS.d6 (s+4)
  , rDex = DS.d3 (s+5) + DS.d4 (s+6) + DS.d5 (s+7) + DS.d6 (s+8)
  , rCon = DS.d3 (s+9) + DS.d4 (s+10) + DS.d5 (s+11) + DS.d6 (s+12)
  , rWis = DS.d3 (s+17) + DS.d4 (s+18) + DS.d5 (s+19) + DS.d6 (s+20)
  , rCha = DS.d3 (s+21) + DS.d4 (s+22) + DS.d5 (s+23) + DS.d6 (s+24)
  , tStr = 1
  , tDex = 1
  , tCon = 1
  , tWis = 1
  , tCha = 1
  , eAC    = 10
  , race   = "Human"
  , age    = 15 + DS.d20 (s+25)
  , eye    = pickEyes (DS.d20 (s+28))
  , hair   = pickHairs (DS.d20 (s+29))
  , height = 55 + DS.d20 (s+30)
  , mark   = pick (DS.d20 (s+31)) marks
  , name   = names gen (DS.d20 (s+32)) female male
  }

pickEyes :: Int -> Text
pickEyes x
  | x == 2 = "Purple"
  | x == 3 = "Green"
  | x == 4 = "Pale Blue"
  | x >= 5 && x <= 7   = "Blue"
  | x >= 8 && x <= 11  = "Pale Grey"
  | x >= 12 && x <= 14 = "Grey"
  | x >= 15 && x <= 17 = "Brown"
  | x == 18 = "Hazel"
  | x == 19 = "Dark Brown"
  | x == 20 = "Black"
  | otherwise = "nil"

female :: [Text]
female = [ "Alexa"
         , "Alfrida"
         , "Betrix"
         , "Bianka"
         , "Carlott"
         , "Elfrida"
         , "Elise"
         , "Gabrielle"
         , "Gretchen"
         , "Hanna"
         , "Ilsa"
         , "Klara"
         , "Jarla"
         , "Ludmilla"
         , "Mathilde"
         , "Regina"
         , "Solveig"
         , "Theodora"
         , "Ulrike"
         , "Wertha"
         ]

pickHairs :: Int -> Text
pickHairs x
  | x == 2 = "White Blond"
  | x == 3 = "Golden Brown"
  | x == 4 = "Red Blond"
  | x >= 5 && x <= 7   = "Golden Brown"
  | x >= 8 && x <= 11  = "Light Brown"
  | x >= 12 && x <= 14 = "Dark Brown"
  | x >= 15 && x <= 17 = "Black"
  | x == 18 = "Auburn"
  | x == 19 = "Red"
  | x == 20 = "Grey"
  | otherwise = "nil"

male :: [Text]
male = [ "Adelbert"
       , "Albrecht"
       , "Berthold"
       , "Dieter"
       , "Eckhardt"
       , "Felix"
       , "Gottfried"
       , "Gustav"
       , "Heinz"
       , "Johann"
       , "Konrad"
       , "Leopold"
       , "Magnus"
       , "Otto"
       , "Pieter"
       , "Rudiger"
       , "Seigfried"
       , "Ulrich"
       , "Waldemar"
       , "Wolfgang"
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
