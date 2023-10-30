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
  | VMouse
  | VMisc
  | VMushroom
  | VPotion
  | VScroll
  | VWand
  | VStairDn
  | VStairUp
  | VTrap
  | VCoin
  | VItem
  | VArrow
  | VCorpse
  | VCorpse1
  | VCorpse2
  | VCorpse3
  | VCorpse4
  | VCorpse5
  | VCorpse6
  | VPerson
  | VDagger
  | VBow
  | VRing
  | VAmulet
  | VArmor
  | VCloak
  | VShield
  | VHelmet
  | VHafted
  | VPolearm
  | VGloves
  | VBoots
  | VRune
  | VWolf
  | VDire
  | VSkeleton
  | VZombie
  | VCold
  | VAcid
  | VLightning
  | VFire
  | VForce
  | VRadiant
  | VPsychic
  | VNecrotic
  | VPointer
  | VTarget1
  | VTarget2
  | VTarget3
  deriving (Show, Eq, Generic)
