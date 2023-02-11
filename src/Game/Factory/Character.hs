{-# LANGUAGE OverloadedStrings #-}
{-

Game.Factory.Character.hs Characters for Arrow.

<https://github.com/joelelmercarlson/arrow>

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Factory.Character (
  manaPoint, hitPoint, genClasses, genRaces
  , Character(..), mkCharacter
  ) where

import Data.Text (Text)
import Util

data Character = Character {
  seed :: Int
  , rStr :: Int
  , rDex :: Int
  , rCon :: Int
  , rWis :: Int
  , rCha :: Int
  , tStr :: Int
  , tDex :: Int
  , tCon :: Int
  , tWis :: Int
  , tCha :: Int
  , eAC :: Int
  , race :: Text
  , gender :: Text
  , age :: Int
  , eye :: Text
  , hair :: Text
  , height :: Int
  , mark :: Text
  , name :: Text
} deriving (Show)

-- | mkCharacter - make '@'
mkCharacter :: Character -> Character
mkCharacter pEntity = let
  dex = rDex pEntity + tDex pEntity
  ac = 11 + abilityMod dex
  in pEntity { eAC = ac }

hitPoint :: Text -> Int
hitPoint n
  | n == "Cleric"    = 8
  | n == "Druid"     = 8
  | n == "Fighter"   = 10
  | n == "Monk"      = 8
  | n == "Paladin"   = 10
  | n == "Ranger"    = 10
  | n == "Rogue"     = 8
  | n == "Warlock"   = 8
  | n == "Wizard"    = 6
  | otherwise = 8

manaPoint :: Text -> Int
manaPoint n
  | n == "Cleric"    = 6
  | n == "Druid"     = 6
  | n == "Fighter"   = 0
  | n == "Monk"      = 0
  | n == "Paladin"   = 3
  | n == "Ranger"    = 3
  | n == "Rogue"     = 3
  | n == "Warlock"   = 6
  | n == "Wizard"    = 6
  | otherwise = 0


-- | genClasses
genClasses :: [Text]
genClasses =
  [ "Cleric"
  , "Druid"
  , "Fighter"
  , "Monk"
  , "Paladin"
  , "Ranger"
  , "Rogue"
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
