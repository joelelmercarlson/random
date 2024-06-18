{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-

Game.Library.Kind.Entity.hs

EntityKind can be Actor, Flavor, Item or Monster.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Library.Kind.Entity (
  AssetMap
  , Conditions
  , EntityMap
  , Entity(..)
  , EntityDmg(..)
  , EntityEmote(..)
  , EntityFeat(..)
  , EntityHabitat(..)
  , EntityHoly(..)
  , EntityKind(..)
  , EntitySize(..)
  , EntitySmart(..)
  , EntityST(..)
  , EntityType(..)
  , EntityUse(..)
  , EntityWeapon(..)
  , Equipment
  , GlyphMap
  , Inventory
  , Properties
  , mkEntityKind
  ) where

import Prelude hiding (lookup)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Game.Compass
import Game.Library.Kind.RGB

-- | Maps used within the game
type AssetMap   = Map Text EntityKind
type Conditions = Map EntityST Int
type EntityMap  = Map Int EntityKind
type Equipment  = Map EntityType EntityKind
type GlyphMap   = Map Text (Int, Int, Int, Int)
type Inventory  = Map Int [EntityKind]
type Properties = Map Text Text

-- | EntityKind
-- | eHP       : Hit Point
-- | eLvl      : Level, HD
-- | eMaxHP    : Max HP
-- | eName     : unique name "+0 dagger"
-- | eXP       : Experience
-- | ecolor    : MiniMap color
-- | equipment : doff/don Items
-- | inventory : Items
-- | kind      : Entity
-- | property  : Descriptions
-- | skill     : Skills
-- | status    : Temporary Conditions
-- | coord     : Position
-- | 'M' data
-- | tid    : name of tile
-- | tid1   : name of decorator tile
-- | eAC    : ArmorClass
-- | eEV    : Evasion
-- | eWP    : WillPower
-- | eBase  : Base mons
-- | eBlock : Block?
-- | eMP    : Mana Point
-- | eMaxMP : Max Mana Point
-- | eSpeed : Speed
-- | 'M' natural resistance
-- | eResAcid      : acid
-- | eResCold      : cold
-- | eResFire      : fire
-- | eResLightning : lightning
-- | eResNecrotic  : necrotic
-- | eResPoison    : poison
-- | eResHoly      : silver, radiant
-- | eResPain      : slashing, piercing, bludgeoning
-- | 'M' flavor
-- | eEmote   : Shout
-- | eHabitat : Habitat
-- | eHoly    : Holiness
-- | eSize    : Size
-- | eSmart   : Intelligence
-- | eUse     : equipment?
-- | 'I' Item data
-- | iValue   : Damage, Value...
-- | iHit     : plus/minus accuracy
-- | iEn      : encumbrance
-- | iPlus    : slay bonuses
-- | iPrice   : relative gp
-- | iDmgType : base damage type
-- | iType    : type of Item
-- | iFeature : Item feature
-- | iSkill   : Skill
-- | iSpell   : spell list
data EntityKind = EntityKind
  { eHP       :: !Int
  , eLvl      :: !Int
  , eMaxHP    :: !Int
  , eXP       :: !Int
  , ecolor    :: !RGB
  , equipment :: !Equipment
  , inventory :: !Inventory
  , kind      :: !Entity
  , property  :: !Properties
  , skill     :: !Conditions
  , status    :: !Conditions
  , coord     :: !Point
  , tid       :: Maybe Text
  , tid1      :: Maybe Text
  , eAC       :: Maybe Int
  , eEV       :: Maybe Int
  , eWP       :: Maybe Int
  , eBase     :: Maybe Text
  , eBlock    :: Maybe Bool
  , eHoly     :: Maybe EntityHoly
  , eMP       :: Maybe Int
  , eMaxMP    :: Maybe Int
  , eName     :: Maybe Text
  , eSpeed    :: Maybe Int
  , eResAcid      :: Maybe Int
  , eResCold      :: Maybe Int
  , eResFire      :: Maybe Int
  , eResLightning :: Maybe Int
  , eResNecrotic  :: Maybe Int
  , eResPoison    :: Maybe Int
  , eResHoly      :: Maybe Int
  , eResPain      :: Maybe Int
  , eEmote   :: Maybe EntityEmote
  , eHabitat :: Maybe EntityHabitat
  , eSmart   :: Maybe EntitySmart
  , eSize    :: Maybe EntitySize
  , eUse     :: Maybe EntityUse
  , eWeapon  :: Maybe EntityWeapon
  , iValue   :: Maybe Int
  , iHit     :: Maybe Int
  , iEn      :: Maybe Int
  , iPlus    :: Maybe Int
  , iDmgType :: Maybe EntityDmg
  , iType    :: Maybe EntityType
  , iFeature :: Maybe [EntityFeat]
  , iSkill   :: Maybe EntityST
  , iSpell   :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON EntityKind
instance ToJSON EntityKind
instance ToJSONKey EntityKind

-- | default EntityKind
mkEntityKind :: Text -> Point -> EntityKind
mkEntityKind x p =
  EntityKind {
  eHP      = 4
  , eLvl   = 1
  , eMaxHP = 4
  , eXP    = 0
  , ecolor = RGB 0 255 255
  , equipment = Map.empty
  , inventory = Map.empty
  , kind      = Flavor
  , property  = Map.insert "Name" x Map.empty
  , skill     = Map.empty
  , status    = Map.empty
  , coord     = p
  , tid  = Nothing
  , tid1 = Nothing
  , eAC  = Just 0
  , eEV  = Just 0
  , eWP  = Just 0
  , eBase  = Nothing
  , eBlock = Just False
  , eMP    = Nothing
  , eMaxMP = Nothing
  , eName  = Nothing
  , eSpeed = Nothing
  , eResAcid      = Just 0
  , eResCold      = Just 0
  , eResFire      = Just 0
  , eResLightning = Just 0
  , eResNecrotic  = Just 0
  , eResPoison    = Just 0
  , eResHoly      = Just 0
  , eResPain      = Just 0
  , eEmote   = Nothing
  , eHabitat = Nothing
  , eHoly    = Nothing
  , eSize    = Nothing
  , eSmart   = Nothing
  , eUse     = Nothing
  , eWeapon  = Nothing
  , iValue   = Nothing
  , iHit     = Nothing
  , iEn      = Nothing
  , iPlus    = Nothing
  , iDmgType = Nothing
  , iType    = Nothing
  , iFeature = Nothing
  , iSkill   = Nothing
  , iSpell   = Nothing
  }

-- | Entity sort
data Entity
  = Actor
  | Sparkle
  | Monster
  | Store
  | Item
  | Coin
  | Corpse
  | Flavor
  | DoorClose
  | DoorOpen
  | ExitUp
  | StairDown
  | StairUp
  | Trap
  deriving (Ord, Show, Eq, Generic)

instance FromJSON Entity
instance ToJSON Entity

-- | M Damage
data EntityDmg
  = Plain
  | Acid
  | Cold
  | Fire
  | Force
  | Lightning
  | Necrotic
  | Poison
  | Radiant
  | Bludgeoning
  | Piercing
  | Slashing
  | Antimagic
  | Barbs
  | Bite
  | Blink
  | BlinkWith
  | Blocked
  | Chaos
  | Claw
  | Confuse
  | Constrict
  | DamageReduction
  | Distort
  | Drag
  | Drain
  | DrainDex
  | DrainSpeed
  | DrainStat
  | Drown
  | Engulf
  | Ensnare
  | FireStrong
  | Flank
  | Gore
  | HeadButt
  | Hit
  | Kick
  | Miss
  | MissClosely
  | Pain
  | PoisonParalyze
  | PoisonStrong
  | Punch
  | Rage
  | Reach
  | ReachSting
  | ReachTongue
  | Rift
  | Scarab
  | Sear
  | Spore
  | Stabbing
  | Sting
  | Swoop
  | TailSlap
  | TentacleSlap
  | Touch
  | Trample
  | TrunkSlap
  | Vampiric
  | Vuln
  | Weakness
  | Zerk
  | NoneDmg
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntityDmg
instance ToJSON EntityDmg

-- | M speaks
data EntityEmote
  = Shout
  | Bark
  | Bellow
  | Bleat
  | Buzz
  | Croak
  | Growl
  | Gurgle
  | Hello
  | Hiss
  | Howl
  | Meow
  | Moan
  | Neigh
  | Roar
  | Scream
  | Screech
  | Silent
  | Skitter
  | Squeak
  | Squeal
  | Taunt
  | Trumpet
  | NoneEmote
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntityEmote
instance ToJSON EntityEmote

-- | M features
data EntityFeat
  = TwoHand
  | Light
  | Finesse
  | Range
  | Stab
  | Acrobat
  | AntiMagic
  | Archer
  | Blessed
  | Chaotic
  | Cursed
  | Defender
  | Dexterity
  | Dispersal
  | Distortion
  | Draining
  | Electric
  | Evasion
  | Faith
  | Fear
  | Feral
  | Fighter
  | Flame
  | Frost
  | Fly
  | Guardian
  | Heavy
  | Hurl
  | Incorporeal
  | Intelligence
  | Life
  | Mage
  | Mana
  | Magic
  | NoBlind
  | NoBrand
  | NoCorpse
  | NoEgo
  | NoMove
  | NoSpawn
  | NoXP
  | NoWeb
  | NPC
  | Penetration
  | Protection
  | Ponderous
  | Rampage
  | Reflect
  | ResAcid
  | ResCold
  | ResFire
  | ResHoly
  | ResLightning
  | ResNecrotic
  | ResPain
  | ResPoison
  | Resistance
  | Silver
  | SInv
  | Slay
  | Spectral
  | Speed
  | Stealth
  | Strength
  | Vamp
  | Venom
  | Vorpal
  | VulnAcid
  | VulnCold
  | VulnFire
  | VulnHoly
  | VulnLightning
  | VulnNecrotic
  | VulnPain
  | VulnPoison
  | Wizard
  | WP
  | Zapper
  | NoneFeat
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntityFeat
instance ToJSON EntityFeat

-- | M home
data EntityHabitat
  = Land
  | Amphibious
  | AmphibiousLava
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntityHabitat
instance ToJSON EntityHabitat

-- | M holiness
data EntityHoly
  = Natural
  | Demonic
  | Holy
  | Undead
  | NonLiving
  | Plant
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntityHoly
instance ToJSON EntityHoly

-- | M conditions, skills, counters
data EntityST
  = Hurt
  | Blinded
  | Confused
  | Deafened
  | Frightened
  | Grappled
  | Incapacitated
  | Invisible
  | Paralyzed
  | Petrified
  | Poisoned
  | Prone
  | Restrained
  | Stunned
  | Unconscious
  | Awake
  | Burnt
  | Corroded
  | Crippled
  | Drained
  | Electrocuted
  | Frozen
  | HurtEvil
  | Noise
  | PlusCombat
  | PlusMoveSpeed
  | PlusSpeed
  | Silenced
  | Slow
  | ARMOR
  | AXE
  | DODGE
  | EVOKE
  | HAFTED
  | INVOKE
  | LONG_BLADE
  | MELEE
  | POLEARM
  | SHIELD
  | SHOOT
  | SHORT_BLADE
  | STAFF
  | STEALTH
  | THROW
  | COIN
  | DECAUT
  | DEPTH
  | ENERGY
  | MAXDEPTH
  | PREVDEPTH
  | REGENERATE
  | REJUVENATE
  | RECALL
  | SEED
  | STAIRDOWN
  | STAIRUP
  | TARGET
  | NoneST
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntityST
instance ToJSON EntityST
instance FromJSONKey EntityST
instance ToJSONKey EntityST

-- | M size
data EntitySize
  = Tiny
  | Small
  | Medium
  | Large
  | Giant
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntitySize
instance ToJSON EntitySize

-- | M intelligence
data EntitySmart
  = Mindless
  | Animal
  | Human
  | NoneSmart
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntitySmart
instance ToJSON EntitySmart

-- | I types
data EntityType
  = Melee
  | Shoot
  | Throw
  | Armor
  | Shield
  | Amulet
  | Boots
  | Gloves
  | Helmet
  | Cloak
  | Food
  | Jewelry
  | Potion
  | Scroll
  | Wand
  | Totem
  | RightHand
  | LeftHand
  | MiscItem
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntityType
instance ToJSON EntityType
instance FromJSONKey EntityType
instance ToJSONKey EntityType

-- | M equipment
data EntityUse
  = MonNone
  | MonDoor
  | MonStartingGear
  | MonEquipment
  | NoneUse
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntityUse
instance ToJSON EntityUse

-- | M Weapons
data EntityWeapon = EntityWeapon
  { hurt0 :: Maybe (EntityDmg, Int)
  , hurt1 :: Maybe (EntityDmg, Int)
  , hurt2 :: Maybe (EntityDmg, Int)
  , hurt3 :: Maybe (EntityDmg, Int)
  }
  deriving (Show, Eq, Generic)

instance FromJSON EntityWeapon
instance ToJSON EntityWeapon
