{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-

Game.Factory.Character.hs Characters for Arrow.

<https://github.com/joelelmercarlson/arrow>

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Factory.Character (
  Character(..)
  , genClasses
  , genRaces
  , hitPoint
  , manaPoint
  ) where

import Data.Text (Text)
import Util

data Character = Character {
  seed :: Int
  , rStr :: Int
  , rDex :: Int
  , rCon :: Int
  , rInt :: Int
  , rWis :: Int
  , rCha :: Int
  , tStr :: Int
  , tDex :: Int
  , tCon :: Int
  , tInt :: Int
  , tWis :: Int
  , tCha :: Int
  , race :: Text
  , gender :: Text
  , age :: Int
  , eye :: Text
  , hair :: Text
  , height :: Int
  , mark :: Text
  , name :: Text
} deriving (Show)

hitPoint :: Text -> Int
hitPoint n
  | n == "Barbarian" = 12
  | n == "Bard"      = 8
  | n == "Cleric"    = 8
  | n == "Druid"     = 8
  | n == "Fighter"   = 10
  | n == "Monk"      = 8
  | n == "Paladin"   = 10
  | n == "Ranger"    = 10
  | n == "Rogue"     = 8
  | n == "Sorcerer"  = 6
  | n == "Warlock"   = 8
  | n == "Wizard"    = 6
  | otherwise = 8

manaPoint :: Text -> Int
manaPoint n
  | n == "Barbarian" = 0
  | n == "Bard"      = 6
  | n == "Cleric"    = 6
  | n == "Druid"     = 6
  | n == "Fighter"   = 0
  | n == "Monk"      = 0
  | n == "Paladin"   = 3
  | n == "Ranger"    = 3
  | n == "Rogue"     = 0
  | n == "Sorcerer"  = 6
  | n == "Warlock"   = 6
  | n == "Wizard"    = 6
  | otherwise = 0

-- | genClasses
genClasses :: [Text]
genClasses =
  [ "Barbarian"
  , "Bard"
  , "Cleric"
  , "Druid"
  , "Fighter"
  , "Monk"
  , "Paladin"
  , "Ranger"
  , "Rogue"
  , "Sorcerer"
  , "Warlock"
  , "Wizard"
  ]

genRaces :: [Text]
genRaces =
  [ "Dwarf"
  , "Elf"
  , "Halfling"
  , "Human"
  ]
