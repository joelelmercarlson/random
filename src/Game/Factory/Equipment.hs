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
  | n == "Barbarian" = ("CAST", "0")
  | n == "Bard"      = ("CAST", "1d8")
  | n == "Cleric"    = ("CAST", "1d8")
  | n == "Druid"     = ("CAST", "1d8")
  | n == "Fighter"   = ("CAST", "0")
  | n == "Monk"      = ("CAST", "0")
  | n == "Paladin"   = ("CAST", "0")
  | n == "Ranger"    = ("CAST", "0")
  | n == "Rogue"     = ("CAST", "0")
  | n == "Sorcerer"  = ("CAST", "1d10")
  | n == "Warlock"   = ("CAST", "1d10")
  | n == "Wizard"    = ("CAST", "1d10")
  | otherwise        = ("CAST", "0")

-- | '@' melee
weaponClass :: (Text, Text)
weaponClass = ("melee", "melee/Dagger")

-- | '@' shoot
weaponShoot :: (Text, Text)
weaponShoot = ("shoot", "shoot/Sling")
