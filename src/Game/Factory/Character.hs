{-# LANGUAGE OverloadedStrings #-}
{-

Game.Factory.Character.hs Characters for Arrow.

<https://github.com/joelelmercarlson/arrow>

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Factory.Character (
  archType, manaPoint, hitPoint
  , genClasses, genRaces
  , Character(..), mkCharacter
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

-- | archType
archType :: Character -> Text
archType n
  | warrior > academic  && str > dex = "Fighter"
  | warrior > academic  && dex > str = "Rogue"
  | academic > warrior  && int > wis = "Wizard"
  | academic > warrior  && wis > int = "Cleric"
  | warrior == academic && str > int || str > wis = "Fighter"
  | warrior == academic && dex > int || dex > wis = "Rogue"
  | warrior == academic && int > str || int > dex = "Wizard"
  | warrior == academic && wis > str || wis > dex = "Cleric"
  | otherwise = "Fighter"
  where
    str = rStr n + tStr n
    dex = rDex n + tDex n
    int = rInt n + tInt n
    wis = rWis n + tWis n
    warrior  = str + dex
    academic = int + wis

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
