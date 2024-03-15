{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-

Game.Library.Kind.Visual.hs

The visual gylph.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Library.Kind.Visual (VisualKind(..)) where

import Prelude hiding (lookup)
import Data.Aeson
import GHC.Generics

instance FromJSON VisualKind
instance ToJSON VisualKind

data VisualKind
  = VActor
  | VWall
  | VLWall
  | VMagma
  | VRubble
  | VDoor
  | VOpen
  | VForest
  | VGrass
  | VWater2
  | VWater
  | VLit
  | VArrow
  | VAcid
  | VCold
  | VFire
  | VForce
  | VLightning
  | VNecrotic
  | VPoison
  | VRadiant
  | VThunder
  | VDartN
  | VDartNE
  | VDartE
  | VDartSE
  | VDartS
  | VDartSW
  | VDartW
  | VDartNW
  | VTarget1
  | VTarget2
  | VTarget3
  | VTarget4
  | VTarget5
  | VTrap
  | VLightlyDamaged
  | VModeratelyDamaged
  | VHeavilyDamaged
  | VSeverlyDamaged
  | VAlmostDead
  | VEmpty
  deriving (Show, Eq, Generic)
