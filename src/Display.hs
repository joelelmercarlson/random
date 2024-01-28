{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-

Display.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Display (rpg, toEntityKind) where

import Data.Aeson
import Data.Text (Text)
import Text.Printf
import Game.Factory.Character
import Game.Factory.Player
import Game.Library.Kind.Entity
import Util


-- | profile
rpg :: EntityKind -> IO ()
rpg n = do
  let str = propertyLookup "str" n
      dex = propertyLookup "dex" n
      int = propertyLookup "int" n
      melee = propertyLookup "melee" n
      shoot = propertyLookup "shoot" n
      zap   = propertyLookup "Zap" n
      pCls  = propertyLookup "Class" n
      race  = propertyLookup "Race" n
  story n
  printf "\n"
  printf "%s %s, " race pCls
  printf "Str %s, " str
  printf "Dex %s, " dex
  printf "Int %s"   int
  printf "\n\n"
  printf "M:%s\nS:%s\nZ:%s\n" (melee)(shoot)(zap)

-- | every character has a story...
story :: EntityKind -> IO ()
story n@EntityKind{..} = do
  let name   = propertyLookup "Name" n
      gender = propertyLookup "Character/Gender" n
      age    = propertyZLookup "Character/Age" n
      height = propertyZLookup "Character/Height" n
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

toEntityKind :: Text -> Character -> IO ()
toEntityKind c n = do
  let r = mkPlayer c n
  rpg r
  encodeFile "player.json" r
