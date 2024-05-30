{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-

Game.Library.Kind.Weapon.hs

WeaponKind

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Library.Kind.Weapon (
   Weapon(..)
   , mkWeapon
   , rollWeapon
  ) where

import Prelude hiding (lookup)
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

-- | mkWeapon
-- | TODO brand
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
-- | uniform (x) = uniform (base * skill * stat) + slay
rollWeapon :: Weapon -> Int -> Int
rollWeapon Weapon{..} s = let
  base  = fromIntegral $ wDie+1
  rawSK = fromIntegral wSkill
  rawST = fromIntegral wStat
  rawEN = fromIntegral wSlay
  finalST = max 1.0 $ 75 + 2.5*rawST
  finalSK = 1 + rawSK/30
  rawUniform   = base * finalST/100 * finalSK + rawEN :: Double
  finalUniform = max 0 $ floor rawUniform
  r0    = DS.uniformRoll finalUniform s
  final = max 0 $ r0-1
  in final
