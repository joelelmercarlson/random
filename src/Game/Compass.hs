{-# LANGUAGE DeriveGeneric #-}
{-

Game.Compass.hs

Basic operations on 2D points

Credit: <https://github.com/LambdaHack/LambdaHack>

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Compass
  ( X, Y, Point(..), (|+|), (|-|)
  , adjacent, chessDist, euclidDistSq, euclidDist
  , bresenhamLine, bla, clamp
  , fromTo, inside
  , toPoint, fromPoint, blackHole, originPoint
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | Spacial dimension for points and vectors.
type X = Int
type Y = Int

data Point = Point
  { px :: X
  , py :: Y }
  deriving (Eq, Ord, Generic)

instance Show Point where
  show (Point x y) = show (x, y)

instance FromJSON Point
instance ToJSON Point

-- | Plus operator to add 2 Point
(|+|) :: Point -> Point -> Point
(|+|) (Point x0 y0) (Point x1 y1) = Point (x0 + x1) (y0 + y1)

-- | Minus operator to subtract 2 Point
(|-|) :: Point -> Point -> Point
(|-|) (Point x0 y0) (Point x1 y1) = Point (x0 - x1) (y0 - y1)

-- | adjacent -- checks whether two coords are adjacent
adjacent :: Point -> Point -> Bool
{-# INLINE adjacent #-}
adjacent from to = chessDist from to == 1

-- | chessDist - Chess distance between two Points.
chessDist :: Point -> Point -> Int
chessDist (Point x0 y0) (Point x1 y1) = max (abs (x1 - x0)) (abs (y1 - y0))

-- | clamp Point to bounds.
clamp :: X -> Y -> Point -> Point
clamp maxX maxY (Point x y) = Point x0 y0
  where
    horiz i = max 0 (min i (maxX-1))
    vert  j = max 0 (min j (maxY-1))
    x0 = horiz x
    y0 = vert y

-- | Euclidean distance between two Points.
euclidDist :: Point -> Point -> Double
euclidDist (Point x0 y0) (Point x1 y1) = sqrt (dX + dY)
  where
    dX = fromIntegral $ (x1 - x0) ^ (2 :: Int)
    dY = fromIntegral $ (y1 - y0) ^ (2 :: Int)

-- | Squared Euclidean distance between two Points.
euclidDistSq :: Point -> Point -> Int
euclidDistSq (Point x0 y0) (Point x1 y1) =
  (x1 - x0) ^ (2 :: Int) + (y1 - y0) ^ (2 :: Int)

-- | Check that a point belongs to an area.
inside :: Point -> (X, Y, X, Y) -> Bool
inside (Point x y) (x0, y0, x1, y1) = x1 >= x && x >= x0 && y1 >= y && y >= y0

-- | Bresenham's line algorithm generalized to arbitrary starting @eps@
-- (@eps@ value of 0 gives the standard BLA).
-- Skips the source point and goes through the second point to the
-- edge of the level.
--
-- >> take 3 $ fromJust $ bresenHamLine 0 (Point 0 0) (Point 10 10)
bresenhamLine :: Int -> Point -> Point -> Maybe [Point]
bresenhamLine eps source target =
  if source == target then Nothing
  else Just $ tail $ blaXY eps source target

-- | bla with X and Y constraints
bla :: X -> Y -> Int -> Point -> Point -> Maybe [Point]
bla maxX maxY eps source target =
  if source == target then Nothing
  else Just $ let
    inBounds p@(Point x y) =
      maxX > x && x >= 0 && maxY > y && y >= 0 && p /= source
      in takeWhile inBounds $ tail $ blaXY eps source target

-- | Bresenham's line algorithm generalized to arbitrary starting @eps@
-- (@eps@ value of 0 gives the standard BLA). Includes the source point
-- and goes through the target point to infinity.
blaXY :: Int -> Point -> Point -> [Point]
blaXY eps (Point x0 y0) (Point x1 y1) = let
  (dx, dy) = (x1 - x0, y1 - y0)
  xyStep b (x, y) = (x + signum dx,     y + signum dy * b)
  yxStep b (x, y) = (x + signum dx * b, y + signum dy)
  (p, q, step) | abs dx > abs dy = (abs dy, abs dx, xyStep)
               | otherwise       = (abs dx, abs dy, yxStep)
  bw = balancedWord p q (eps `mod` max 1 q)
  walk w xy = xy : walk (tail w) (step (head w) xy)
  in map (uncurry Point) $ walk bw (x0, y0)

-- | balancedWord
-- See <http://roguebasin.com/index.php/Digital_lines>
balancedWord :: Int -> Int -> Int -> [Int]
balancedWord p q eps | eps + p < q = 0 : balancedWord p q (eps + p)
balancedWord p q eps               = 1 : balancedWord p q (eps + p - q)

-- | List of all horizontal, vertical lines
-- between two Points. Fails if no such line exists.
fromTo :: Point -> Point -> [Point]
{-# NOINLINE fromTo #-}
fromTo (Point x0 y0) (Point x1 y1) =
  let fromTo1 :: Int -> Int -> [Int]
      fromTo1 z0 z1
        | z0 <= z1 = [z0..z1]
        | otherwise = [z0,z0-1..z1]
      result
        | x0 == x1 = map (Point x0) (fromTo1 y0 y1)
        | y0 == y1 = map (`Point` y0) (fromTo1 x0 x1)
        | otherwise = error $ "diagonal fromTo"
          ++ show (x0, y0)
          ++ show (x1, y1)
  in result

-- | toPoint, fromPoint
toPoint :: [(Int, Int)] -> [Point]
toPoint = map (uncurry Point)

fromPoint :: [Point] -> [(Int, Int)]
fromPoint = map (\(Point x y) -> (x, y))

-- |  Off the Map...
blackHole :: Point
blackHole = Point (-1) (-1)

-- | Origin Point.
originPoint :: Point
originPoint = Point 0 0
