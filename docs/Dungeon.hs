{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-

Engine.Arrow.Dungeon.hs

Engine.Arrow.Dungeon is a Dungeon generator which uses Vector

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.Dungeon (boxDungeon
                            , Dungeon(..)
                            , getTerrainAt
                            , rogueDungeon
                            , Terrain(..)) where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Random.Class (MonadRandom, uniformMay)
import Control.Monad.Random (getRandomR, RandomGen, runRandT)
import Control.Monad.ST (runST)
import Data.Aeson
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import GHC.Generics

data Dungeon = Dungeon
  { dungeonWidth :: Int
  , dungeonHeight :: Int
  , dungeonTiles :: Vector Terrain
  } deriving (Show)

type Hall = Room
data Orientation = Vertical | Horizontal deriving (Show)
data Room = Room !Int !Int !Int !Int deriving (Show)

data Terrain
  = Door
  | Magma
  | Open
  | Rock
  | Rubble
  | Wall
  | Rally
  | Stair
  deriving (Show, Eq, Generic)

instance FromJSON Terrain
instance ToJSON Terrain

-- | boxDungeon builds the dungeon
-- Output:
-- ######
-- #....#
-- #....#
-- ######
boxDungeon :: Int -> Int -> Dungeon
boxDungeon xMax yMax = Dungeon xMax yMax tiles
  where
    tiles =  V.generate (xMax*yMax)
      (\i -> let (y, x) = i `divMod` xMax
        in if x == 0 || x == xMax-1 || y == 0 || y == yMax-1
        then Wall
        else Open)

-- | getTerrainAt
getTerrainAt :: (Int, Int) -> Dungeon -> Terrain
getTerrainAt (x, y) d = let
  width = dungeonWidth d
  height = dungeonHeight d
  index = y * width + x
  in if y < 0 || x < 0 || y >= height || x >= width
    then Wall
    else V.unsafeIndex (dungeonTiles d) index

-- | mkRoom
mkRoom :: (Int,Int) -> (Int,Int) -> Room
mkRoom (x1,y1) (x2,y2) = let
  xlow = min x1 x2
  ylow = min y1 y2
  xhigh = max x1 x2
  yhigh = max y1 y2
  in Room xlow ylow xhigh yhigh

-- | overlapX
overlapX :: Room -> Room -> Set Int
overlapX (Room r1xl _ r1xh _) (Room r2xl _ r2xh _) =
  S.intersection
    (S.fromAscList [r1xl..r1xh])
    (S.fromAscList [r2xl..r2xh])

-- | overlapY
overlapY :: Room -> Room -> Set Int
overlapY (Room _ r1yl _ r1yh) (Room _ r2yl _ r2yh) =
  S.intersection
    (S.fromAscList [r1yl..r1yh])
    (S.fromAscList [r2yl..r2yh])

-- | pickHallways
-- Horizontal
-- Vertical
pickHallways :: MonadRandom m
  => Room
  -> Room
  -> Orientation
  -> m [Hall]
pickHallways r1@(Room r1xl _ r1xh _) r2@(Room r2xl _ r2xh _)
  Horizontal = do
  mayY <- uniformMay (overlapY r1 r2)
  case mayY of
    Just y -> pure [mkRoom (min r1xl r2xl,y) (max r1xh r2xh,y)]
    Nothing -> do
      pure []
pickHallways r1@(Room _ r1yl _ r1yh) r2@(Room _ r2yl _ r2yh)
  Vertical = do
  mayX <- uniformMay (overlapX r1 r2)
  case mayX of
    Just x -> pure [mkRoom (x,min r1yl r2yl) (x,max r1yh r2yh)]
    Nothing -> do
      pure []

-- | randDebris
randDebris :: (MonadRandom m) => Int -> Int -> m (Room, Orientation, Terrain)
randDebris width height = do
  t <- getRandomR (0, 4)
  r <- randRoom 1 (width-1) 1 (height-1)
  pure $ case (t :: Int) of
    0 -> (r, Horizontal, Rubble)
    1 -> (r, Vertical,   Rubble)
    2 -> (r, Horizontal, Magma)
    3 -> (r, Vertical,   Magma)
    _ -> (r, Horizontal, Rubble)

-- | randRoom
randRoom :: (MonadRandom m) => Int -> Int -> Int -> Int -> m Room
randRoom xlow xhigh ylow yhigh = do
  x1 <- getRandomR (xlow,xhigh)
  x2 <- getRandomR (xlow,xhigh)
  y1 <- getRandomR (ylow,yhigh)
  y2 <- getRandomR (ylow,yhigh)
  pure $ mkRoom (x1,y1) (x2,y2)

-- | rogueDungeon build the Dungeon
--
-- 0. Fill w/ Wall
-- 1. Add 5% Rubble
-- 2. Divide Dungeon into 9 sections
--    1 - 2 - 3
--    4 - 5 - 6
--    7 - 8 - 9
-- 3. Create Room in each section
-- 4. Create Hallway between each section
-- 5. Connect Rooms and Hallways w/ Open
-- 6. Explore the Dungeon
rogueDungeon :: RandomGen g => Int -> Int -> g -> (Dungeon, g)
rogueDungeon width height g = let
  tileCount   = width*height
  debrisCount = tileCount `div` 20
  secWidth    = width `div` 3
  secHeight   = height `div` 3
  (tileVector, gFinal) = runST $ flip runRandT g $ do
    vec <- VM.replicate tileCount Wall
    -- add Debris
    forM_ [1 :: Int .. debrisCount] $ \_ -> do
      (r, o, t) <- randDebris (width-1) (height-1)
      setPoint width vec r o t
    -- pick the rooms
    rooms <- sequence [
            randRoom   1 (secWidth-1) 1 (secHeight-1)
            , randRoom (secWidth+1) (2*secWidth-1) 1 (secHeight-1)
            , randRoom (2*secWidth+1) (width-2) 1 (secHeight-1)
            , randRoom 1 (secWidth-1) (secHeight+1) (2*secHeight-1)
            , randRoom (secWidth+1) (2*secWidth-1) (secHeight+1)  (2*secHeight-1)
            , randRoom (2*secWidth+1) (width-2) (secHeight+1) (2*secHeight-1)
            , randRoom 1 (secWidth-1) (2*secHeight+1) (height-2)
            , randRoom (secWidth+1) (2*secWidth-1) (2*secHeight+1) (height-2)
            , randRoom (2*secWidth+1) (width-2) (2*secHeight+1) (height-2)
            ]
    -- draw the rooms
    forM_ rooms $ \r -> setBox width vec r Open
    -- pick the halls
    forM_ [1 :: Int .. 12] $ \borderIndex -> do
      let (sec1targ, sec2targ, isVert) = case borderIndex of
            1 -> (1,2,Horizontal)
            2 -> (2,3,Horizontal)
            3 -> (4,5,Horizontal)
            4 -> (5,6,Horizontal)
            5 -> (7,8,Horizontal)
            6 -> (8,9,Horizontal)
            7 -> (1,4,Vertical)
            8 -> (4,7,Vertical)
            9 -> (2,5,Vertical)
            10 -> (5,8,Vertical)
            11 -> (3,6,Vertical)
            12 -> (6,9,Vertical)
            _ -> (1,2,Horizontal)
          sec1 = rooms !! (sec1targ-1)
          sec2 = rooms !! (sec2targ-1)
    -- line up rooms with halls
      halls <- pickHallways sec1 sec2 isVert
      forM_ halls $ \h -> setBox width vec h Open
    -- connect the sectors
    V.unsafeFreeze vec
  in (Dungeon width height tileVector, gFinal)

-- | setPoint
-- Modify the vector w/ Point and Terrain
-- Output:
-- %%
-- %%
setPoint :: PrimMonad m
  => Int
  -> VM.MVector (PrimState m) a
  -> Room
  -> Orientation
  -> a
  -> m ()
setPoint width vec (Room x1 y1 _ _) Horizontal
  = setBox width vec (Room x1 y1 (x1+1) y1)
setPoint width vec (Room x1 y1 _ _) Vertical
  = setBox width vec (Room x1 y1 x1 (y1+1))

-- | setBox
-- Modify the vector w/ Room and Terrain
setBox :: PrimMonad m
  => Int
  -> VM.MVector (PrimState m) a
  -> Room
  -> a
  -> m ()
setBox width vec (Room x1 y1 x2 y2) tile = do
  let xmin = min x1 x2
      xmax = max x1 x2
      ymin = min y1 y2
      ymax = max y1 y2
      targets = do
        y <- [ymin .. ymax]
        x <- [xmin .. xmax]
        pure $ width*y+x
  mapM_ (\i -> VM.write vec i tile) targets
