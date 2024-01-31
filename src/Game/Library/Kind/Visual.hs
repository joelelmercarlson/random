{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-

Game.Library.Kind.Visual.hs

The visual gylph.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

Core
====
@  = Actor
#  = Wall
.  = Open
%  = Rubble, Corpse
r  = Mouse
:  = Rock
*  = Magma
,  = Mushroom
!  = Potion
>  = Stair Down
<  = Stair Up
^  = Trap
$  = Coin
[  = Item
~  = Arrow
&  = Rune
?  = Magic Device
+  = Door
p  = Person

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
  | VOpen
  | VLWall
  | VWater
  | VWater2
  | VRubble
  | VRock
  | VMagma
  | VDoor
  | VStairDn
  | VStairUp
  | VTrap
  | VCoin
  | VArrow
  | VAcid
  | VCold
  | VFire
  | VForce
  | VLightning
  | VNecrotic
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
  deriving (Show, Eq, Generic)
