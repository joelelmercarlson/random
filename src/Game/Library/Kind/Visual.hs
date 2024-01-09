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
  | VAcid
  | VCold
  | VFire
  | VLightning
  | VNecrotic
  | VForce
  | VRadiant
  | VThunder
  | VPsychic
  | VTarget1
  | VTarget2
  | VTarget3
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
  | VArrow
  | VAmulet
  | VArmor
  | VBlood1
  | VBlood10
  | VBlood2
  | VBlood3
  | VBlood4
  | VBlood5
  | VBlood6
  | VBlood7
  | VBlood8
  | VBlood9
  | VBoots1
  | VBoots2
  | VBoots3
  | VBoots4
  | VBow
  | VCloak1
  | VCloak2
  | VCloak3
  | VCloak4
  | VCorpse
  | VCorpse1
  | VCorpse2
  | VCorpse3
  | VCorpse4
  | VCorpse5
  | VCorpse6
  | VDagger
  | VGloves1
  | VGloves2
  | VGloves3
  | VGloves4
  | VHafted
  | VHelmet1
  | VHelmet2
  | VHelmet3
  | VHelmet4
  | VHitPoint1
  | VHitPoint2
  | VHitPoint3
  | VHitPoint4
  | VHitPoint5
  | VNpc
  | VPerson
  | VPolearm
  | VRing
  | VRune
  | VScrap
  | VSleep
  | VShield
  | VThrow1
  | VThrow10
  | VThrow2
  | VThrow3
  | VThrow4
  | VThrow5
  | VThrow6
  | VThrow7
  | VThrow8
  | VThrow9
  deriving (Show, Eq, Generic)
