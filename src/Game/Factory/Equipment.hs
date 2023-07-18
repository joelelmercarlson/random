{-# LANGUAGE OverloadedStrings #-}
{-

Game.Factory.Equipment.hs

Game.Factory.Equipment.hs creates '@' gear

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Factory.Equipment where

import Data.Text (Text)

-- | '@' zap
weaponCast :: Text -> (Text, Text)
weaponCast n
  | n == "Barbarian" = ("Zap", "0")
  | n == "Bard"      = ("Zap", "1d8")
  | n == "Cleric"    = ("Zap", "1d8")
  | n == "Druid"     = ("Zap", "1d8")
  | n == "Fighter"   = ("Zap", "0")
  | n == "Monk"      = ("Zap", "0")
  | n == "Paladin"   = ("Zap", "0")
  | n == "Ranger"    = ("Zap", "0")
  | n == "Rogue"     = ("Zap", "0")
  | n == "Sorcerer"  = ("Zap", "1d10")
  | n == "Warlock"   = ("Zap", "1d10")
  | n == "Wizard"    = ("Zap", "1d10")
  | otherwise        = ("Zap", "0")

-- | '@' melee
weaponClass :: (Text, Text)
weaponClass = ("melee", "melee/Dagger")

-- | '@' shoot
weaponShoot :: (Text, Text)
weaponShoot = ("shoot", "shoot/Sling")
