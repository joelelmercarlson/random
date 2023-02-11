{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-

Game.Factory.EntityKind.hs for Arrow. Converts Characters into
EntityKind.

<https://github.com/joelelmercarlson/arrow>

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Factory.EntityKind (
  EntityKind(..), mkEntityKind, propertyLookup, propertyNLookup
  ) where

import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Game.Compass
import Game.Factory.Character
import Game.Factory.Equipment
import Game.Kind.Entity
import Game.Kind.Visual
import Util

abilitySort :: Text -> Character -> Character
abilitySort x n@Character{..} = let
  s = sort [rStr, rDex, rCon, rWis, rCha]
  ability | x == "Fighter" = strengthSort s n
          | x `elem` [ "Monk", "Ranger", "Rogue" ] = dexteritySort s n
          | x `elem` [ "Druid", "Cleric", "Wizard" ] = wisdomSort s n
          | x `elem` [ "Paladin", "Warlock" ] = charismaSort s n
          | otherwise = n
  in mkCharacter $ ability

classFmt :: Text -> Text
classFmt n = let
  Just (x, xs) = T.uncons n
  k = T.cons (toUpper x) (T.toLower xs)
  pCls = if k `elem` genClasses then k else "Fighter"
  in pCls

mkEntityKind :: Text -> Character -> EntityKind
mkEntityKind c n = let
  pCls = classFmt c
  character = abilitySort pCls n
  con       = rCon character + tCon character
  hp        = hitPoint pCls + abilityMod con
  mp        = manaPoint pCls
  in EntityKind {
  coord = originPoint
  , block = True
  , kind = Actor
  , glyph = VActor
  , moveT = []
  , spawn = originPoint
  , inventory = Map.fromList mkInventory
  , property = Map.fromList $ mkProperty pCls hp mp character
  , eLvl = 1
  , eHP = hp
  , eMaxHP = hp
  , eMP = mp
  , eMaxMP = mp
  , eXP = 0
  }

mkInventory :: [(Text, Int)]
mkInventory = [("Arrow", 1), ("Coin", 1), ("Mushroom", 1), ("Potion", 1)]

mkProperty :: Text -> Int -> Int -> Character -> [(Text, Text)]
mkProperty pClass hp mp Character{..} = let
  str  = T.pack $ show $ rStr + tStr
  dex  = T.pack $ show $ rDex + tDex
  con  = T.pack $ show $ rCon + tCon
  wis  = T.pack $ show $ rWis + tWis
  cha  = T.pack $ show $ rCha + tCha
  str' = T.pack $ show $ tStr
  dex' = T.pack $ show $ tDex
  con' = T.pack $ show $ tCon
  wis' = T.pack $ show $ tWis
  cha' = T.pack $ show $ tCha
  newPlayer = [ ("Name", name)
              , ("AWAKE", "awake")
              , ("Race", race)
              , ("str", str)
              , ("dex", dex)
              , ("con", con)
              , ("wis", wis)
              , ("cha", cha)
              , ("AC", T.pack $ show eAC)
              , ("ATTACKS", "1")
              , ("Character/Age", T.pack $ show age)
              , ("Character/Birth/Trait/str", str')
              , ("Character/Birth/Trait/dex", dex')
              , ("Character/Birth/Trait/con", con')
              , ("Character/Birth/Trait/wis", wis')
              , ("Character/Birth/Trait/cha", cha')
              , ("Character/Birth/str", str)
              , ("Character/Birth/dex", dex)
              , ("Character/Birth/con", con)
              , ("Character/Birth/wis", wis)
              , ("Character/Birth/cha", cha)
              , ("Character/Eye", eye)
              , ("Character/Gender", gender)
              , ("Character/Hair", hair)
              , ("Character/Height", T.pack $ show height)
              , ("Character/Mark", mark)
              , ("Character/Store",
                 T.concat [
                    "Arrow:Mushroom:Potion"
                    , ":melee/Longsword:melee/Shortsword:melee/Quarterstaff"
                    , ":melee/Mace:melee/Dagger:armor/Leather:shoot/Sling"
                    ])
              , ("Class", pClass)
              , ("HP", T.pack $ show $ hp)
              , ("MP", T.pack $ show $ mp)
              , ("seed", T.pack $ show seed)
              , ("Dungeon/Zone", "Town")
              , ("Dungeon/Level", "0")
              , ("Dungeon/Depth", "0")
              , ("Dungeon/Prev", "0")
              , weaponCast pClass
              , weaponClass pClass
              , weaponShoot pClass
              , ("rightHand", "None")
              , ("leftHand", "None")
              , ("neck", "None")
              , ("armor", "armor/Leather")
              , ("cloak", "None")
              , ("shield", "None")
              , ("head", "None")
              , ("hands", "None")
              , ("feet", "None")
              , ("rune", "None")
              , ("rune1", "None")
              ]
  in newPlayer

propertyLookup :: Text -> EntityKind -> Text
propertyLookup x EntityKind{..} = Map.findWithDefault "None" x property

propertyNLookup :: Text -> EntityKind -> Int
propertyNLookup x EntityKind{..} =
  read $ T.unpack $ Map.findWithDefault "0" x property :: Int

strengthSort :: [Int] -> Character -> Character
strengthSort s n =
  n { rStr=s!!4, rDex=s!!2, rCon=s!!3, rWis=s!!1, rCha=s!!0 }

dexteritySort :: [Int] -> Character -> Character
dexteritySort s n =
  n { rStr=s!!1, rDex=s!!4, rCon=s!!2, rWis=s!!3, rCha=s!!0 }

charismaSort :: [Int] -> Character -> Character
charismaSort s n =
  n { rStr=s!!4, rDex=s!!1, rCon=s!!2, rWis=s!!0, rCha=s!!3 }

wisdomSort :: [Int] -> Character -> Character
wisdomSort s n =
  n { rStr=s!!1, rDex=s!!3, rCon=s!!2, rWis=s!!4, rCha=s!!0 }
