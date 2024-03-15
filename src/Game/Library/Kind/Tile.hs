{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-

Game.Library.Kind.Tile

The Tile.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Library.Kind.Tile (
  Dungeon(..)
  , Feature(..)
  , RGB (..)
  , Terrain(..)
  , TileKind(..)
  , TileMap
  , VisualKind(..)
  , addFeat
  , addLit
  , addRGB
  , addVis
  , boxDungeon
  , findPoints
  , rogueDungeon
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import GHC.Generics (Generic)
import Game.Compass
import Game.Library.Kind.Dungeon
import Game.Library.Kind.RGB
import Game.Library.Kind.Visual

type TileMap = Map Int TileKind

-- | Possible Terrain feature
data Feature
  = Dark
  | OftenPlayer
  | OftenMonster
  | OftenLavaMonster
  | OftenWaterMonster
  | NoDig
  | NoMonster
  | AcidPoint
  | ColdPoint
  | FirePoint
  | GrassPoint
  | ForestPoint
  | RallyPoint
  | UniquePoint
  | WaterPoint
  | WayPoint
  | RZ1
  | RZ2
  | RZ3
  | RZ4
  | RZ5
  | RZ6
  | RZ7
  | RZ8
  | RZ9
  deriving (Show, Eq, Generic)

instance FromJSON Feature
instance ToJSON Feature

-- | @Tile@ in Dungeon
-- | tcoord    = Point
-- | tlit      = Visible
-- | tkind     = Terrain
-- | tglyph    = Glyph
-- | talter    = durability
-- | tfeature  = list of features
-- | tcolor    = RGB
data TileKind = TileKind
  { tcoord    :: !Point
  , tlit      :: !Bool
  , tkind     :: !Terrain
  , tglyph    :: !VisualKind
  , talter    :: !Int
  , tfeature  :: ![Feature]
  , tcolor    :: !RGB
  } deriving (Eq, Generic)

instance FromJSON TileKind
instance ToJSON TileKind

instance Show TileKind where
  show TileKind{..} = concat [ show tcoord, ", ", show tfeature ]

-- | addFeat, addLit, addVis Terrain
addFeat :: Terrain -> [Feature]
addFeat t
  | t == Wall   = [Dark]
  | t == Light  = [Dark,NoDig]
  | t == Magma  = [Dark,NoDig]
  | t == Rubble = [Dark]
  | t == Door   = [Dark]
  | t == Open   = [OftenPlayer]
  | t == Rally  = [RallyPoint,OftenMonster]
  | t == Stair  = [WayPoint,NoMonster]
  | t == Unique = [UniquePoint,NoMonster]
  | t == R1 = [RZ1,OftenPlayer]
  | t == R2 = [RZ2,OftenMonster]
  | t == R3 = [RZ3,OftenMonster]
  | t == R4 = [RZ4,OftenMonster]
  | t == R5 = [RZ5,OftenMonster]
  | t == R6 = [RZ6,OftenMonster]
  | t == R7 = [RZ7,OftenMonster]
  | t == R8 = [RZ8,OftenMonster]
  | t == R9 = [RZ9,OftenMonster]
  | t == Forest       = [OftenMonster,ForestPoint]
  | t == Grass        = [OftenPlayer,GrassPoint]
  | t == Ice          = [OftenMonster,ColdPoint,NoDig]
  | t == Lava         = [OftenLavaMonster,FirePoint,NoDig]
  | t == Swamp        = [OftenWaterMonster,AcidPoint,NoDig]
  | t == WaterDeep    = [NoMonster,WaterPoint,NoDig]
  | t == WaterShallow = [OftenWaterMonster,WaterPoint,NoDig]
  | otherwise = []

addLit :: Terrain -> Bool
addLit t
  | t `elem` [Lava, Light, WaterDeep, WaterShallow] = True
  | otherwise = False

-- | miniMap
addRGB :: Terrain -> RGB
addRGB t
  | t == Wall   = RGB 255 255 255
  | t == Light  = RGB 255 255 255
  | t == Magma  = RGB 128 0 0
  | t == Rubble = RGB 128 128 0
  | t == Door   = RGB 192 192 192
  | t == Open   = RGB 128 128 128
  | t == Stair  = RGB 192 192 192
  | t == Forest       = RGB 0 128 0
  | t == Grass        = RGB 0 128 0
  | t == Ice          = RGB 0 255 255
  | t == Lava         = RGB 255 0 0
  | t == Swamp        = RGB 0 255 0
  | t == WaterDeep    = RGB 0 0 128
  | t == WaterShallow = RGB 0 128 128
  | otherwise = RGB 128 128 128

-- | glyph
addVis :: Terrain -> VisualKind
addVis t
  | t == Wall   = VWall
  | t == Light  = VLWall
  | t == Magma  = VMagma
  | t == Rubble = VRubble
  | t == Door   = VDoor
  | t == Open   = VOpen
  | t == Rally  = VOpen
  | t == Stair  = VOpen
  | t == Unique = VOpen
  | t == R1 = VOpen
  | t == R2 = VOpen
  | t == R3 = VOpen
  | t == R4 = VOpen
  | t == R5 = VOpen
  | t == R6 = VOpen
  | t == R7 = VOpen
  | t == R8 = VOpen
  | t == R9 = VOpen
  | t == Forest       = VForest
  | t == Grass        = VGrass
  | t == Ice          = VCold
  | t == Lava         = VFire
  | t == Swamp        = VAcid
  | t == WaterDeep    = VWater2
  | t == WaterShallow = VWater
  | otherwise = VWall
