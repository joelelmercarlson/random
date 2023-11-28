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
  | n == "Bard"      = ("Zap", "spell/Radiant10")
  | n == "Cleric"    = ("Zap", "spell/Radiant10")
  | n == "Druid"     = ("Zap", "spell/Radiant10")
  | n == "Fighter"   = ("Zap", "0")
  | n == "Monk"      = ("Zap", "0")
  | n == "Paladin"   = ("Zap", "0")
  | n == "Ranger"    = ("Zap", "0")
  | n == "Rogue"     = ("Zap", "0")
  | n == "Sorcerer"  = ("Zap", "spell/Force10")
  | n == "Warlock"   = ("Zap", "spell/Force10")
  | n == "Wizard"    = ("Zap", "spell/Force10")
  | otherwise        = ("Zap", "0")

-- | '@' melee
weaponClass :: (Text, Text)
weaponClass = ("melee", "melee/Dagger")

-- | '@' shoot
weaponShoot :: (Text, Text)
weaponShoot = ("shoot", "shoot/Sling")
