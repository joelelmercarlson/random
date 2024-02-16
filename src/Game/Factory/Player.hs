{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-

Game.Factory.Player.hs for Arrow. Converts Characters into
EntityKind.

<https://github.com/joelelmercarlson/arrow24>

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
    xs = sort $ map (min 12) [ rStr + tStr, rDex + tDex, rInt + tInt ]

mkPlayer :: Text -> Character -> EntityKind
mkPlayer x n = let
  cls = classFmt x
  actor = abilitySort cls n
  x0 = mkEntityKind "Player" originPoint
  in x0 { kind = Actor, glyph = VActor
  , eBlock = Just True
  , eMove = Just True
  , property = Map.fromList $ mkProperty cls actor
  , status = Map.insert COIN 30 (status x0)
  , tid = Just (23,60) }

mkProperty :: Text -> Character -> [(Text, Text)]
mkProperty pCls Character{..} = let
  str  = T.pack $ show $ rStr
  dex  = T.pack $ show $ rDex
  int  = T.pack $ show $ rInt
  str' = T.pack $ show $ tStr
  dex' = T.pack $ show $ tDex
  int' = T.pack $ show $ tInt
  in [ ("Name", name)
     , ("Race", race)
     , ("str", str)
     , ("dex", dex)
     , ("int", int)
     , ("Character/Age", T.pack $ show age)
     , ("Character/Birth/Trait/str", str')
     , ("Character/Birth/Trait/dex", dex')
     , ("Character/Birth/Trait/int", int')
     , ("Character/Birth/str", str)
     , ("Character/Birth/dex", dex)
     , ("Character/Birth/int", int)
     , ("Character/Eye", eye)
     , ("Character/Gender", gender)
     , ("Character/Hair", hair)
     , ("Character/Height", T.pack $ show height)
     , ("Character/Mark", mark)
     , ("Class", pCls)
     ]

propertyLookup :: Text -> EntityKind -> Text
propertyLookup n x = Map.findWithDefault "None" n (property x)

propertyZLookup :: Text -> EntityKind -> Int
propertyZLookup n x = read $ T.unpack $ Map.findWithDefault "0" n (property x) :: Int

strengthSort :: [Int] -> Character -> Character
strengthSort s x = x { rStr=s!!2, rDex=s!!1, rInt=s!!0 }

dexteritySort :: [Int] -> Character -> Character
dexteritySort s x = x { rStr=s!!1, rDex=s!!2, rInt=s!!0 }

charismaSort :: [Int] -> Character -> Character
charismaSort s x = x { rStr=s!!1, rDex=s!!0, rInt=s!!2 }

wisdomSort :: [Int] -> Character -> Character
wisdomSort s x = x { rStr=s!!0, rDex=s!!1, rInt=s!!2 }

classFmt :: Text -> Text
classFmt n
  | k `elem` genClasses = k
  | otherwise = "Fighter"
  where
    Just (x, xs) = T.uncons n
    k = T.cons (toUpper x) (T.toLower xs)

speciesFmt :: Text -> Text
speciesFmt n
  | k `elem` genRaces = k
  | otherwise = "Human"
  where
    Just (x, xs) = T.uncons n
    k = T.cons (toUpper x) (T.toLower xs)
