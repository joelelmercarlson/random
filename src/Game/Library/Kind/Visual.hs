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
  | VLWall
  | VOpen
  | VLOpen
  | VWater
  | VLWater
  | VWater2
  | VLWater2
  | VRubble
  | VMagma
  | VDoor
  | VTrap
  | VLit
  | VLit1
  | VLit2
  | VLit3
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
