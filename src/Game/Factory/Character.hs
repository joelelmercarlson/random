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
  ) where

import Data.Text (Text)
import Util

data Character = Character {
  seed :: Int
  , rStr :: Int
  , rDex :: Int
  , rInt :: Int
  , tStr :: Int
  , tDex :: Int
  , tInt :: Int
  , race :: Text
  , gender :: Text
  , age :: Int
  , eye :: Text
  , hair :: Text
  , height :: Int
  , mark :: Text
  , name :: Text
} deriving (Show)

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
