{-

Game.Fov.hs

Precise Permissive Field of View

Credit: <http://www.roguebasin.com/index.php/Precise_Permissive_Field_of_View>

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Fov (checkFov, mkFov) where

import Prelude hiding (pred)
import Data.Set (Set)
import qualified Data.Set as S
import Game.Library.Tile (TileMap)
import qualified Game.Library.Tile as GLT
import Game.Compass

data SightLine = SightLine Int Int Int Int deriving (Eq, Show)
data ViewBump = ViewBump Int Int (Maybe ViewBump) deriving (Eq, Show)

data View = View {
  getShallowBumps :: [Point]
  , getShallowLine :: SightLine
  , getSteepBumps :: [Point]
  , getSteepLine :: SightLine
  } deriving (Eq, Show)

type  VisionBlocked = Point -> Bool

-- | mkFov for @
mkFov :: Point -> TileMap -> [Point]
mkFov from tm = S.toList $ checkFov from vision 4
  where
    vision = GLT.fromVisionBlocked tm

-- | abovePoint infix
abovePoint :: SightLine -> Point -> Bool
abovePoint self p = relativeSlope self p < 0

-- | aboveOrCollinearPoint infix
aboveOrCollinearPoint :: SightLine -> Point -> Bool
aboveOrCollinearPoint self p = relativeSlope self p <= 0

-- | add helper, remove and update are inline
add :: (Integral a) => a -> [b] -> b -> [b]
add a (b:bs) new
  | a < 0 = b : bs
  | a == 0 = new : b : bs
  | otherwise = b : add (a-1) bs new
add 0 [] new = [new]
add _ [] _   = []

-- | addShallowBump
addShallowBump :: Point -> View -> View
addShallowBump loc@(Point x y) view = let
  (SightLine xi yi _ _) = getShallowLine view
  newShallowBumps = loc : getShallowBumps view
  newShallowLine = foldl (foldLineUsing abovePoint) (SightLine xi yi x y) (getSteepBumps view)
  in view { getShallowBumps = newShallowBumps, getShallowLine = newShallowLine }

-- | addSteepBump
addSteepBump :: Point -> View -> View
addSteepBump loc@(Point x y) view = let
  (SightLine xi yi _ _) = getSteepLine view
  newSteepBumps = loc : getSteepBumps view
  newSteepLine = foldl (foldLineUsing belowPoint) (SightLine xi yi x y) (getShallowBumps view)
  in view { getSteepBumps = newSteepBumps, getSteepLine = newSteepLine }

-- | belowOrCollinearPoint infix
belowOrCollinearPoint :: SightLine -> Point -> Bool
belowOrCollinearPoint self p = relativeSlope self p >= 0

-- | belowPoint infix
belowPoint :: SightLine -> Point -> Bool
belowPoint self p = relativeSlope self p > 0

-- | bumpAndCheck checks and returns the new view list in single op
bumpAndCheck :: (Point -> View -> View) -> [View] -> Int -> Point -> [View]
bumpAndCheck bumpf activeViews viewIndex bump = let
  view = activeViews !! viewIndex
  bumpedView = bumpf bump view
  in take viewIndex activeViews ++ if validView bumpedView
  then bumpedView : drop (viewIndex+1) activeViews
  else drop (viewIndex+1) activeViews

-- | calcViewIndex is a version of `dropWhile`
calcViewIndex :: [View] -> Point -> Int
calcViewIndex activeViews bottomRight = let
  go tmp [] = tmp
  go tmp (v:vs) = if getSteepLine v `belowOrCollinearPoint` bottomRight
    then go (tmp+1) vs
    else tmp
  in go 0 activeViews

-- | Perform view calculations on a single Quandrant relative to
-- the start position of the overall FOV computation.
checkQuadrant :: VisionBlocked -> Int -> Point -> Point -> Set Point
checkQuadrant vision range (Point sx sy) (Point qx qy) =
  checkSub coordsToCheck (S.singleton (Point sx sy)) startViewList
  where
    shallowLineStart = SightLine 0 1 range 0
    steepLineStart   = SightLine 1 0 0 range
    startViewList    = [mkView shallowLineStart steepLineStart]
    coordsToCheck    = coordsFromRange range
    checkSub :: [Point] -> Set Point -> [View] -> Set Point
    checkSub _  visited [] = visited
    checkSub [] visited _ = visited
    checkSub ((Point dx dy):cs) visited activeViews =
      checkSub cs newVisited newActiveViews
      where
        (newVisited, newActiveViews) =
          visitCoord (Point sx sy) (Point dx dy) (Point qx qy) activeViews vision visited

-- | checkFov to check the Precise Permissive Field of View
-- for @origin@ vs @xs@ coordinates at @distance@.
--
-- Summary: PPFoV
-- .
-- .
-- .
-- 9
-- 5 8
-- 2 4 7
-- @ 1 3 6 ...
checkFov :: Point -> [Point] -> Int -> Set Point
checkFov origin xs distance = let
  blocked = (`elem` xs)
  in fov blocked distance origin

-- | collinearPpoint infix
collinearPoint :: SightLine -> Point -> Bool
collinearPoint self p = relativeSlope self p == 0

-- | collinearLine infix
collinearLine :: SightLine -> SightLine -> Bool
collinearLine self (SightLine xi yi xf yf) =
  collinearPoint self (Point xi yi) && collinearPoint self (Point xf yf)

-- | coordsFromRange the list of coords from start position
coordsFromRange :: Int -> [Point]
coordsFromRange range = do
  let maxIndex = (2*range) + 1
  i <- [1..(maxIndex-1)]
  let startJ  = max (i-range) 0
  let maxJ    = min i range + 1
  j <- [startJ .. (maxJ-1)]
  toPoint $ pure (i-j, j)

-- | foldLineUsing for SightLine fn
foldLineUsing :: (SightLine -> Point -> Bool) -> SightLine -> Point -> SightLine
foldLineUsing pred line@(SightLine _ _ xf yf) loc@(Point x y) =
  if line `pred` loc then SightLine x y xf yf else line

-- | fov
-- fov checks vision at range from start position
-- vision is fn to check Coord which block vision
-- see checkFov
fov :: VisionBlocked -> Int -> Point -> Set Point
fov vision range start = unionMap (checkQuadrant vision range start)
  [ Point 1 1, Point (-1) 1, Point 1 (-1), Point (-1) (-1) ]

-- | makes a new view using shallow and steep line w/o bumps
mkView :: SightLine -> SightLine -> View
mkView shallowLine steepLine = View {
  getShallowBumps = []
  , getShallowLine = shallowLine
  , getSteepBumps = []
  , getSteepLine = steepLine
  }

-- | relativeSlope infix
relativeSlope :: SightLine -> Point -> Int
relativeSlope (SightLine xi yi xf yf) (Point x y) = let
  dx = xf - xi
  dy = yf - yi
  in (dy * (xf - x)) - (dx * (yf - y))

-- | adds a single location to the set of visited locations if its
-- within one of the views, and updates any views as necessary.
visitCoord :: Point
  -> Point
  -> Point
  -> [View]
  -> VisionBlocked
  -> Set Point
  -> (Set Point, [View])
visitCoord (Point sx sy) (Point dx dy) (Point qx qy) activeViews vision visited = let
  -- inline remove
  remove i list = take i list ++ drop (i+1) list
  topLeft = Point dx (dy+1)
  bottomRight = Point (dx+1) dy
  realX = dx * qx
  realY = dy * qy
  trueLocation = Point (sx + realX) (sy + realY)
  viewIndex = calcViewIndex activeViews bottomRight
  in if viewIndex == length activeViews || getShallowLine (activeViews !! viewIndex) `aboveOrCollinearPoint` topLeft
  then (visited, activeViews) -- no compatible views
  else let newVisited = S.insert trueLocation visited
           visionBlocked = vision trueLocation
           in if visionBlocked
                 then let currentView = activeViews !! viewIndex -- vision is blocked
                          shallowAboveBottomRight = getShallowLine currentView `abovePoint` bottomRight
                          steepBelowTopLeft = getSteepLine currentView `belowPoint` topLeft
                          in case (shallowAboveBottomRight, steepBelowTopLeft) of
                               (True, True) -> (newVisited, remove viewIndex activeViews)
                               (True, False) -> (newVisited, bumpAndCheck addShallowBump activeViews viewIndex topLeft)
                               (False, True) -> (newVisited, bumpAndCheck addSteepBump activeViews viewIndex bottomRight)
                               (False, False) -> do
                                 let clonedViews = add viewIndex activeViews currentView in
                                   let shallowChecked = bumpAndCheck addShallowBump clonedViews (viewIndex+1) topLeft in
                                     let steepChecked = bumpAndCheck addSteepBump shallowChecked viewIndex bottomRight
                                     in (newVisited, steepChecked)
                 else (newVisited, activeViews) -- vision not blocked

-- | unionMap maps fn over list and returns Set
unionMap :: (Ord b) => (a -> Set b) -> [a] -> Set b
unionMap f list = S.unions $ map f list

-- | validView `infix`
validView :: View -> Bool
validView view = not (shallowIsSteep && lineOnExtremity)
  where
    shallowIsSteep = shallowLine' `collinearLine` steepLine'
    lineOnExtremity = shallowLine' `collinearPoint` (Point 0 1) || shallowLine' `collinearPoint` (Point 1 0)
    shallowLine' = getShallowLine view
    steepLine' = getSteepLine view
