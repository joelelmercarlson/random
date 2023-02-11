{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-

Display.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Display (rpg, toEntityKind) where

import Data.Text (Text)
import Data.Yaml as Y
import Text.Printf
import Game.Factory.Character
import Game.Factory.EntityKind
import Util

toEntityKind :: Text -> Character -> IO ()
toEntityKind c n = do
  let r = mkEntityKind c n
  rpg r
  Y.encodeFile "player.yaml" r

-- | profile
rpg :: EntityKind -> IO ()
rpg n = do
  let str = propertyNLookup "str" n
      dex = propertyNLookup "dex" n
      con = propertyNLookup "con" n
      wis = propertyNLookup "wis" n
      cha = propertyNLookup "cha" n
      ac  = propertyNLookup "AC" n
      hp  = propertyNLookup "HP" n
      mp  = propertyNLookup "MP" n
      armor = propertyLookup "armor" n
      melee = propertyLookup "melee" n
      shoot = propertyLookup "shoot" n
      cast  = propertyLookup "CAST" n
      pCls  = propertyLookup "Class" n
      race  = propertyLookup "Race" n
  story n
  printf "\n"
  printf "Armor Class %d (%s)\n" ac armor
  printf "Hit Points %d\n" hp
  printf "Mana Points %d\n" mp
  printf "\n"
  printf "%s %s, " race pCls
  printf "Str=%s, " (abilityFmt str)
  printf "Dex=%s, " (abilityFmt dex)
  printf "Con=%s, " (abilityFmt con)
  printf "Wis=%s, " (abilityFmt wis)
  printf "Cha=%s\n" (abilityFmt cha)
  printf "\n"
  printf "M:%s\nS:%s\nZap:%s\n" (melee)(shoot)(cast)

-- | every character has a story...
story :: EntityKind -> IO ()
story n@EntityKind{..} = do
  let name   = propertyLookup "Name" n
      gender = propertyLookup "Character/Gender" n
      age    = propertyNLookup "Character/Age" n
      height = propertyNLookup "Character/Height" n
      hair   = propertyLookup "Character/Hair" n
      eye    = propertyLookup "Character/Eye" n
      mark   = propertyLookup "Character/Mark" n
  printf "Name:  %s | Gender: %s\n" name gender
  printf "Age: %-2d " age
  printf "| Height: %-5s " $ heightF height
  printf "| Hair: %s " hair
  printf "| Eyes: %s\n" eye
  printf "Mark: %s " mark
  printf "\n"
