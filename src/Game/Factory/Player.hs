{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-

Game.Factory.Player.hs for Arrow. Converts Characters into
EntityKind.

<https://github.com/joelelmercarlson/bifrost>

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Factory.Player (
  mkPlayer, propertyLookup, propertyZLookup, classFmt, speciesFmt
  ) where

import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Game.Compass
import Game.Factory.Character
import Game.Factory.Equipment
import Game.Library.Kind.Entity
import Game.Library.Kind.RGB
import Game.Library.Kind.Visual
import Util


abilitySort :: Text -> Character -> Character
abilitySort x n@Character{..}
  | x `elem` [ "Barbarian", "Fighter" ] = strengthSort xs n
  | x `elem` [ "Monk", "Ranger", "Rogue" ] = dexteritySort xs n
  | x `elem` [ "Druid", "Cleric", "Wizard" ] = wisdomSort xs n
  | x `elem` [ "Bard", "Paladin", "Sorcerer", "Warlock" ] = charismaSort xs n
  | otherwise = n
  where
    xs = sort $ map (min 18) [ rStr + tStr, rDex + tDex, rCon + tCon, rInt + tInt, rWis + tWis, rCha + tCha ]

mkPlayer :: Text -> Character -> EntityKind
mkPlayer x n = let
  pCls  = classFmt x
  actor = abilitySort pCls n
  hp    = hitPoint pCls
  mp    = manaPoint pCls
  in EntityKind {
  coord = originPoint
  , block = True
  , move = True
  , kind = Actor
  , glyph = VActor
  , energy = Map.fromList [("seed", 0), ("Hunger", 500), ("speed", 100), ("energy", 100), ("Coin", 13)]
  , equipment = Map.empty
  , inventory = Map.empty
  , property = Map.fromList $ mkProperty pCls hp mp actor
  , status = Map.empty
  , eLvl = 1
  , eHP = hp
  , eMaxHP = hp
  , eMP = mp
  , eMaxMP = mp
  , eXP = 0
  , ecolor = RGB 0 255 0 }

mkProperty :: Text -> Int -> Int -> Character -> [(Text, Text)]
mkProperty pCls hp mp Character{..} = let
  str  = T.pack $ show $ rStr
  dex  = T.pack $ show $ rDex
  con  = T.pack $ show $ rCon
  int  = T.pack $ show $ rInt
  wis  = T.pack $ show $ rWis
  cha  = T.pack $ show $ rCha
  str' = T.pack $ show $ tStr
  dex' = T.pack $ show $ tDex
  con' = T.pack $ show $ tCon
  int' = T.pack $ show $ tInt
  wis' = T.pack $ show $ tWis
  cha' = T.pack $ show $ tCha
  in [ ("Name", name)
     , ("Race", race)
     , ("str", str)
     , ("dex", dex)
     , ("con", con)
     , ("int", int)
     , ("wis", wis)
     , ("cha", cha)
     , ("Character/Age", T.pack $ show age)
     , ("Character/Birth/Trait/str", str')
     , ("Character/Birth/Trait/dex", dex')
     , ("Character/Birth/Trait/con", con')
     , ("Character/Birth/Trait/int", int')
     , ("Character/Birth/Trait/wis", wis')
     , ("Character/Birth/Trait/cha", cha')
     , ("Character/Birth/str", str)
     , ("Character/Birth/dex", dex)
     , ("Character/Birth/con", con)
     , ("Character/Birth/int", int)
     , ("Character/Birth/wis", wis)
     , ("Character/Birth/cha", cha)
     , ("Character/Eye", eye)
     , ("Character/Gender", gender)
     , ("Character/Hair", hair)
     , ("Character/Height", T.pack $ show height)
     , ("Character/Mark", mark)
     , ("Character/Spells", "Zap:Light:Recall:Minor Heal")
     , ("Character/Store", "ammo/arrow:food/mushroom:melee/Dagger:armor/Leather:shoot/Sling")
     , ("Class", pCls)
     , ("HP", T.pack $ show $ hp)
     , ("MP", T.pack $ show $ mp)
     , ("Dungeon/Zone", "Town")
     , ("Extra/AC", "10")
     , ("Extra/ATTACKS", "1")
     , weaponCast pCls
     , weaponClass
     , weaponShoot
     , ("armor", "armor/Leather")
     ]

propertyLookup :: Text -> EntityKind -> Text
propertyLookup n x = Map.findWithDefault "None" n (property x)

propertyZLookup :: Text -> EntityKind -> Int
propertyZLookup n x = read $ T.unpack $ Map.findWithDefault "0" n (property x) :: Int

strengthSort :: [Int] -> Character -> Character
strengthSort s x = x { rStr=s!!5, rDex=s!!3, rCon=s!!4, rInt=s!!1, rWis=s!!2, rCha=s!!0 }

dexteritySort :: [Int] -> Character -> Character
dexteritySort s x = x { rStr=s!!2, rDex=s!!5, rCon=s!!4, rInt=s!!1, rWis=s!!3, rCha=s!!0 }

charismaSort :: [Int] -> Character -> Character
charismaSort s x = x { rStr=s!!4, rDex=s!!1, rCon=s!!2, rInt=s!!0, rWis=s!!3, rCha=s!!5 }

wisdomSort :: [Int] -> Character -> Character
wisdomSort s x = x { rStr=s!!2, rDex=s!!1, rCon=s!!3, rInt=s!!4, rWis=s!!5, rCha=s!!0 }

classFmt :: Text -> Text
classFmt n = if k `elem` genClasses then k else "Fighter"
  where
    Just (x, xs) = T.uncons n
    k = T.cons (toUpper x) (T.toLower xs)

speciesFmt :: Text -> Text
speciesFmt n = if k `elem` genRaces then k else "Human"
  where
    Just (x, xs) = T.uncons n
    k = T.cons (toUpper x) (T.toLower xs)
