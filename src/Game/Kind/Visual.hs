{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-

Game.Kind.Visual.hs

This module keeps the gylph

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

@ = Actor
# = Wall
. = Open
% = Rubble
r = Rat
: = Rocks
* = Magma
, = Mushroom
! = Potion
> = Stair Down
< = Stair Up
^ = Trap
$ = Coin
[ = Item
~ = Arrow
A = Angel
c / C = Canine
d / D = Dragon
h / p / t = Human, Person, Towns
o / O = Orc, Ogre
P / T = Giant, Troll
u / U = Demon
M / R = Hydra, Reptile
S = Spider
s / z = Skeleton, Zombie
L / W / V = Lich, Wraith, Vampire

-}
module Game.Kind.Visual (VisualKind(..)) where

import Prelude hiding (lookup)
import Data.Aeson
import GHC.Generics

data VisualKind
  -- row 1
  = VActor
  | VWall
  | VOpen
  | VRubble
  | VMouse
  | VRock
  | VMagma
  | VMushroom
  | VPotion
  | VStairDn
  | VStairUp
  | VTrap
  | VCoin
  | VItem
  | VArrow
  | VLWall
  | VLOpen
  -- row 2
  | VLRock
  | VLMagma
  | VCorpse
  | VLRubble
  | VLMouse
  | VPerson
  | VPoison
  | VFire
  | VCold
  | VOrc
  | VOpen1
  | VOpen2
  | VOpen3
  | VOpen4
  | VOpen5
  | VDoor
  | VOpen6
  | VOpen7
  -- row 3
  | VDagger
  | VBow
  | VRing
  | VAmulet
  | VArmor
  | VCloak
  | VShield
  | VHelmet
  | VGloves
  | VBoots
  | VLOpen1
  | VLOpen2
  | VLOpen3
  | VLOpen4
  | VLOpen5
  | VLOpen6
  | VLOpen7
  -- row 4 Red
  | VDemon1
  | VLDemon1
  | VDragon1
  | VLDragon1
  | VOrc1
  | VGiant1
  | VHuman1
  | VHydra1
  | VJelly1
  | VLich1
  | VTowns1
  | VPerson1
  | VWraith1
  | VAngel1
  | VReptile1
  | VSpider1
  | VVampire1
  -- row 5 Cyan
  | VDemon2
  | VLDemon2
  | VDragon2
  | VLDragon2
  | VOrc2
  | VGiant2
  | VHuman2
  | VHydra2
  | VJelly2
  | VLich2
  | VTowns2
  | VPerson2
  | VWraith2
  | VAngel2
  | VReptile2
  | VSpider2
  | VVampire2
  -- row 6 Purple
  | VDemon3
  | VLDemon3
  | VDragon3
  | VLDragon3
  | VOrc3
  | VGiant3
  | VHuman3
  | VHydra3
  | VJelly3
  | VLich3
  | VTowns3
  | VPerson3
  | VWraith3
  | VAngel3
  | VReptile3
  | VSpider3
  | VVampire3
  -- row 7 Green
  | VDemon4
  | VLDemon4
  | VDragon4
  | VLDragon4
  | VOgre
  | VTroll
  | VHuman4
  | VHydra4
  | VJelly4
  | VLich4
  | VTowns4
  | VPerson4
  | VWraith4
  | VWolf
  | VDire
  | VSkeleton
  | VZombie
  deriving (Ord, Show, Eq, Generic)

instance FromJSON VisualKind
instance ToJSON VisualKind
