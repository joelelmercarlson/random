{-# LANGUAGE OverloadedStrings #-}
{-

Game.Factory.Equipment.hs

Game.Factory.Equipment.hs creates '@' gear

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Factory.Equipment where

import Data.Text (Text)

-- | '@' Cast
weaponCast :: Text -> (Text, Text)
weaponCast n
  | n == "Cleric"    = ("CAST", "1d8")
  | n == "Druid"     = ("CAST", "1d8")
  | n == "Fighter"   = ("CAST", "0")
  | n == "Monk"      = ("CAST", "0")
  | n == "Paladin"   = ("CAST", "1d8")
  | n == "Ranger"    = ("CAST", "1d8")
  | n == "Rogue"     = ("CAST", "1d4")
  | n == "Warlock"   = ("CAST", "1d10")
  | n == "Wizard"    = ("CAST", "1d10")
  | otherwise        = ("CAST", "0")

-- | '@' melee
weaponClass :: Text -> (Text, Text)
weaponClass n
  | n == "Cleric"    = ("melee", "melee/Mace")
  | n == "Druid"     = ("melee", "melee/Sickle")
  | n == "Fighter"   = ("melee", "melee/Longsword")
  | n == "Monk"      = ("melee", "melee/Shortsword")
  | n == "Paladin"   = ("melee", "melee/Longsword")
  | n == "Ranger"    = ("melee", "melee/Shortsword")
  | n == "Rogue"     = ("melee", "melee/Shortsword")
  | n == "Warlock"   = ("melee", "melee/Club")
  | n == "Wizard"    = ("melee", "melee/Quarterstaff")
  | otherwise        = ("melee", "melee/Dagger")

-- | '@' shoot
weaponShoot :: Text -> (Text, Text)
weaponShoot n
  | n == "Cleric"    = ("shoot", "shoot/Sling")
  | n == "Druid"     = ("shoot", "shoot/Sling")
  | n == "Fighter"   = ("shoot", "melee/Spear")
  | n == "Monk"      = ("shoot", "shoot/Dart")
  | n == "Paladin"   = ("shoot", "melee/Javelin")
  | n == "Ranger"    = ("shoot", "shoot/Longbow")
  | n == "Rogue"     = ("shoot", "melee/Dagger")
  | n == "Warlock"   = ("shoot", "shoot/Dart")
  | n == "Wizard"    = ("shoot", "shoot/Dart")
  | otherwise        = ("shoot", "None")
