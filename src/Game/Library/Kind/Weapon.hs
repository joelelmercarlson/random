{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-

Game.Library.Kind.Weapon.hs

WeaponKind

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Library.Kind.Weapon (
   Weapon(..)
   , check2h
   , checkBow
   , checkFinesse
   , checkLight
   , checkRange
   , checkStone
   , mkWeapon
   , rollWeapon
  ) where

import Prelude hiding (lookup)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Game.DiceSet as DS
import Game.Library.Kind.Entity

data Weapon = Weapon
  { wName  :: !Text
  , wDie   :: !Int
  , wToHit :: !Int
  , wSkill :: !Int
  , wStat  :: !Int
  , wSlay  :: !Int
  , wType  :: !EntityDmg
  } deriving (Eq)

-- | pretty print Weapon
instance Show Weapon where
  show Weapon{..} =
    concat [ T.unpack wName, ". +", show wToHit, " to Hit, d", show wDie, " ", show wType ]

check2h :: EntityKind -> Bool
check2h x = TwoHand `elem` xs
  where
    xs = fromMaybe [] $ iFeature x

checkBow :: EntityKind -> Bool
checkBow x = xs == Shoot
  where
    xs = fromMaybe MiscItem $ iType x

checkFinesse :: EntityKind -> Bool
checkFinesse x = Finesse `elem` xs
  where
    xs = fromMaybe [] $ iFeature x

checkLight :: EntityKind -> Bool
checkLight x = Light `elem` xs
  where
    xs = fromMaybe [] $ iFeature x

checkRange :: EntityKind -> Bool
checkRange x = Range `elem` xs
  where
    xs = fromMaybe [] $ iFeature x

checkStone :: EntityKind -> Bool
checkStone x = xs == Throw
  where
    xs = fromMaybe MiscItem $ iType x

-- | mkWeapon
mkWeapon :: Text -> Int -> Int -> (Int, Int, Int, Int) -> EntityDmg -> Weapon
mkWeapon n0 die0 hit (sk, st, acc, slay) t0 = let
  rawHT = fromIntegral hit
  rawSK = fromIntegral sk
  rawST = fromIntegral st
  rawAC = fromIntegral acc
  rawEN = fromIntegral slay
  finalSK = rawSK-1
  finalST = rawST/2
  finalHT = rawHT + finalSK + finalST + rawAC + rawEN :: Double
  final = Weapon {
    wName = n0
    , wDie   = die0
    , wToHit = floor finalHT
    , wSkill = sk
    , wStat  = st
    , wSlay  = slay
    , wType  = t0
    }
  in final

-- | rollWeapon
-- | random 1d(x+1)-1
-- | uniform (base * skill * stat) + uniform(slay)
rollWeapon :: Weapon -> Int -> Int
rollWeapon Weapon{..} s = let
  base  = fromIntegral wDie
  rawSK = fromIntegral wSkill
  rawST = fromIntegral wStat
  scale = 100
  finalSK = 1 + rawSK/30
  finalST = 75 + 2.5*rawST
  rawUniform = (base+1) * finalST/scale * finalSK :: Double
  finalR0 = floor rawUniform
  r0 = DS.uniformRoll finalR0 (s+1)
  r1 = DS.uniformRoll wSlay (s+2)
  final | wSlay > 0 = (r0+r1)-1
        | otherwise = r0-1
  in max 0 final
