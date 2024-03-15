{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-

Game.Library.Kind.Dungeon

The Dungeon.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Library.Kind.Dungeon (
  Dungeon(..)
  , boxDungeon
  , rogueDungeon
  , findPoints
  , Terrain(..)
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.Random (getRandomR, RandomGen, runRandT)
import Control.Monad.ST (runST)
import Data.Aeson (FromJSON, ToJSON)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import GHC.Generics (Generic)
import Game.Compass

data Dungeon = Dungeon
  { dungeonWidth :: Int
  , dungeonHeight :: Int
  , dungeonTiles :: Vector Terrain
  }

data Terrain
  = Wall
  | Light
  | Magma
  | Rubble
  | Door
  | Open
  | Rally
  | Stair
  | Unique
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | Forest
  | Grass
  | Ice
  | Lava
  | Swamp
  | WaterDeep
  | WaterShallow
  deriving (Show, Eq, Generic)

instance FromJSON Terrain
instance ToJSON Terrain

-- | ##########
-- | #........#
-- | #........#
-- | #........#
-- | #........#
-- | #........#
-- | #........#
-- | #........#
-- | #........#
-- | ##########
boxDungeon :: X -> Y -> Dungeon
boxDungeon xMax yMax = let
  t0 = V.generate (xMax*yMax) (\i -> let
                                  (y, x) = i `divMod` xMax
                                  in if x==0 || x==xMax-1 ||
                                        y==0 || y==yMax-1
                                     then Wall
                                     else Open)
  in Dungeon xMax yMax t0

-- | roll die
roll :: (MonadRandom m) => X -> Y -> m (Int)
roll x0 y0 = do
  x <- getRandomR (x0, y0)
  pure $ x

-- | rogueDungeon - 9 rooms w/o halls
rogueDungeon :: RandomGen g => X -> Y -> g -> (Dungeon, g)
rogueDungeon w h g = let
  (tileVector, g0) = runST $ flip runRandT g $ do
    tr      <- roll 1 8
    vec0    <- VM.replicate (w*h) Wall
    grass   <- mkTerrain w h [Grass,Grass,Forest,Wall]
    ice     <- mkTerrain w h [Ice,Ice,Rubble,Wall]
    forest  <- mkTerrain w h [Grass,Forest,Forest,Wall]
    lava    <- mkTerrain w h [Lava,Lava,Magma,Wall]
    pasture <- mkTerrain w h [Grass,Forest,WaterShallow,Wall]
    rocks   <- mkTerrain w h [Magma,Rubble,Rubble,Wall]
    swamp   <- mkTerrain w h [Swamp,Swamp,WaterShallow,Wall]
    walls   <- mkTerrain w h [Open,Wall,Open,Wall]
    zones   <- mkZones w h
    let terrain = case tr of
          1 -> grass
          2 -> ice
          3 -> forest
          4 -> lava
          5 -> pasture
          6 -> rocks
          7 -> swamp
          _ -> walls
    mapM_ (\(k, v) -> setBox w vec0 k v) terrain
    mapM_ (\(k, v) -> setBox w vec0 k v) zones
    V.unsafeFreeze vec0
  in (Dungeon w h tileVector, g0)

mkZones :: (MonadRandom m) => X -> Y -> m([((X,Y,X,Y),Terrain)])
mkZones x0 y0 = sequence [
      randRoom w h (p!!0) R1
      , randRoom w h (p!!1) R2
      , randRoom w h (p!!2) R3
      , randRoom w h (p!!3) R4
      , randRoom w h (p!!4) R5
      , randRoom w h (p!!5) R6
      , randRoom w h (p!!6) R7
      , randRoom w h (p!!7) R8
      , randRoom w h (p!!8) R9
      ]
  where
    w = x0 - 1
    h = y0 - 1
    p = findPoints w h

-- | Dungeon room helpers 3x3 or bigger
mkRoom :: X -> Y -> (X, Y, X, Y) -> (X, Y, X, Y)
mkRoom w h (x0, y0, x1, y1) = let
  xMin = max 3 $ min x0 x1
  xMax = max 3 $ min (w-1) $ max x0 x1
  yMin = max 3 $ min y0 y1
  yMax = max 3 $ min (h-1) $ max y0 y1
  in (xMin, yMin, xMax, yMax)

randRoom :: (MonadRandom m) => X -> Y -> (X, Y, X, Y) -> Terrain -> m ((X, Y, X, Y), Terrain)
randRoom w h (x0, y0, x1, y1) t = do
  x2 <- getRandomR (x0, x1)
  y2 <- getRandomR (y0, y1)
  x3 <- getRandomR (x0, x1)
  y3 <- getRandomR (y0, y1)
  pure $ (mkRoom w h (x2, y2, x3, y3), t)

setBox :: PrimMonad m => Int -> VM.MVector (PrimState m) a -> (X, Y, X, Y) -> a -> m ()
setBox width vec (x0, y0, x1, y1) t = do
  let xMin = min x0 x1
      xMax = max x0 x1
      yMin = min y0 y1
      yMax = max y0 y1
      targets = do
        y <- [yMin .. yMax]
        x <- [xMin .. xMax]
        pure $ width * y + x
  mapM_ (\i -> VM.write vec i t) targets

-- | findPoints - Dungeon sectors
findPoints :: X -> Y -> [(X, Y, X, Y)]
findPoints width height = let
  secW = width `div` 3
  secH = height `div` 3
  s1 = (1,          1,          (secW-1), (secH-1))
  s2 = ((secW+1),   1,          (2*secW-1), (secH-1))
  s3 = ((2*secW+1), 1,          (width-2), (secH-1))
  s4 = (1,          (secH+1),   (secW-1), (2*secH-1))
  s5 = ((secW+1),   (secH+1),   (2*secW-1), (2*secH-1))
  s6 = ((2*secW+1), (secH+1),   (width-2), (2*secH-1))
  s7 = (1,          (2*secH+1), (secW-1),   (height-2))
  s8 = ((secW+1),   (2*secH+1), (2*secW-1), (height-2))
  s9 = ((2*secW+1), (2*secH+1), (width-2),  (height-2))
  in  [s1,s2,s3,s4,s5,s6,s7,s8,s9]

mkTerrain :: (MonadRandom m) => X -> Y -> [Terrain] -> m([((X,Y,X,Y),Terrain)])
mkTerrain x0 y0 t = sequence [
      randRoom   w h (p!!0) (t!!0)
      , randRoom w h (p!!0) (t!!1)
      , randRoom w h (p!!0) (t!!2)
      , randRoom w h (p!!0) (t!!3)
      , randRoom w h (p!!1) (t!!0)
      , randRoom w h (p!!1) (t!!1)
      , randRoom w h (p!!1) (t!!2)
      , randRoom w h (p!!1) (t!!3)
      , randRoom w h (p!!2) (t!!0)
      , randRoom w h (p!!2) (t!!1)
      , randRoom w h (p!!2) (t!!2)
      , randRoom w h (p!!2) (t!!3)
      , randRoom w h (p!!3) (t!!0)
      , randRoom w h (p!!3) (t!!1)
      , randRoom w h (p!!3) (t!!2)
      , randRoom w h (p!!3) (t!!3)
      , randRoom w h (p!!4) (t!!0)
      , randRoom w h (p!!4) (t!!1)
      , randRoom w h (p!!4) (t!!2)
      , randRoom w h (p!!4) (t!!3)
      , randRoom w h (p!!5) (t!!0)
      , randRoom w h (p!!5) (t!!1)
      , randRoom w h (p!!5) (t!!2)
      , randRoom w h (p!!5) (t!!3)
      , randRoom w h (p!!6) (t!!0)
      , randRoom w h (p!!6) (t!!1)
      , randRoom w h (p!!6) (t!!2)
      , randRoom w h (p!!6) (t!!3)
      , randRoom w h (p!!7) (t!!0)
      , randRoom w h (p!!7) (t!!1)
      , randRoom w h (p!!7) (t!!2)
      , randRoom w h (p!!7) (t!!3)
      , randRoom w h (p!!8) (t!!0)
      , randRoom w h (p!!8) (t!!1)
      , randRoom w h (p!!8) (t!!2)
      , randRoom w h (p!!8) (t!!3)
      ]
  where
    w = x0 - 2
    h = y0 - 2
    p = findPoints w h
