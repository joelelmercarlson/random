{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-

Game.Factory.EntityKind.hs for Arrow. Converts Characters into
EntityKind.

<https://github.com/joelelmercarlson/arrow>

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Factory.EntityKind (
  EntityKind(..), mkEntityKind
  , propertyLookup, propertyZLookup
  , classFmt, speciesFmt
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
abilitySort x n@Character{..}
  | x `elem` [ "Barbarian", "Fighter" ] = strengthSort s n
  | x `elem` [ "Monk", "Ranger", "Rogue" ] = dexteritySort s n
  | x `elem` [ "Druid", "Cleric", "Wizard" ] = wisdomSort s n
  | x `elem` [ "Bard", "Paladin", "Sorcerer", "Warlock" ] = charismaSort s n
  | otherwise = n
  where
    s = sort $ map (min 18) [ rStr + tStr
                            , rDex + tDex
                            , rCon + tCon
                            , rWis + tWis
                            , rCha + tCha ]

mkEntityKind :: Text -> Character -> EntityKind
mkEntityKind x n = let
  pCls  = classFmt x
  actor = abilitySort pCls n
  hp    = hitPoint pCls
  mp    = manaPoint pCls
  in EntityKind {
  coord = originPoint
  , block = True
  , kind = Actor
  , glyph = VActor
  , moveT = []
  , spawn = originPoint
  , inventory = Map.fromList mkInventory
  , property = Map.fromList $ mkProperty pCls hp mp actor
  , eLvl = 1
  , eHP = hp
  , eMaxHP = hp
  , eMP = mp
  , eMaxMP = mp
  , eXP = 0 }

mkInventory :: [(Text, Int)]
mkInventory = [("Arrow", 1), ("Coin", 1), ("Mushroom", 1), ("Potion", 1)]

mkProperty :: Text -> Int -> Int -> Character -> [(Text, Text)]
mkProperty pCls hp mp Character{..} = let
  str  = T.pack $ show $ rStr
  dex  = T.pack $ show $ rDex
  con  = T.pack $ show $ rCon
  int  = T.pack $ show $ rInt + tInt
  wis  = T.pack $ show $ rWis
  cha  = T.pack $ show $ rCha
  str' = T.pack $ show $ tStr
  dex' = T.pack $ show $ tDex
  con' = T.pack $ show $ tCon
  int' = T.pack $ show $ tInt
  wis' = T.pack $ show $ tWis
  cha' = T.pack $ show $ tCha
  xs = [ ("Name", name)
       , ("Race", race)
       , ("str", str)
       , ("dex", dex)
       , ("con", con)
       , ("int", int)
       , ("wis", wis)
       , ("cha", cha)
       , ("AC", T.pack $ show eAC)
       , ("ATTACKS", "1")
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
       , ("Character/Store", T.concat [
             "Arrow:Mushroom:Potion"
             , ":melee/Dagger:armor/Leather:shoot/Sling"])
       , ("Class", pCls)
       , ("HP", T.pack $ show $ hp)
       , ("MP", T.pack $ show $ mp)
       , ("seed", T.pack $ show seed)
       , ("Condition/awake", "None")
       , ("Dungeon/Zone", "Town")
       , ("Dungeon/Level", "0")
       , ("Dungeon/Depth", "0")
       , ("Dungeon/Prev", "0")
       , weaponCast pCls
       , weaponClass
       , weaponShoot
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
       ]
  in xs

propertyLookup :: Text -> EntityKind -> Text
propertyLookup x EntityKind{..} = Map.findWithDefault "None" x property

propertyZLookup :: Text -> EntityKind -> Int
propertyZLookup x EntityKind{..} =
  read $ T.unpack $ Map.findWithDefault "0" x property :: Int

strengthSort :: [Int] -> Character -> Character
strengthSort s x = mkCharacter
  x { rStr=s!!4, rDex=s!!2, rCon=s!!3, rWis=s!!1, rCha=s!!0 }

dexteritySort :: [Int] -> Character -> Character
dexteritySort s x = mkCharacter
  x { rStr=s!!1, rDex=s!!4, rCon=s!!2, rWis=s!!3, rCha=s!!0 }

charismaSort :: [Int] -> Character -> Character
charismaSort s x = mkCharacter
  x { rStr=s!!1, rDex=s!!0, rCon=s!!2, rWis=s!!3, rCha=s!!4 }

wisdomSort :: [Int] -> Character -> Character
wisdomSort s x = mkCharacter
  x { rStr=s!!1, rDex=s!!3, rCon=s!!2, rWis=s!!4, rCha=s!!0 }

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
