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
import Game.Library.Kind.Visual

-- | Maps used within the game
type AssetMap   = Map Text EntityKind
type Conditions = Map EntityST Int
type EntityMap  = Map Int EntityKind
type Equipment  = Map EntityType EntityKind
type Inventory  = Map Int [EntityKind]
type Properties = Map Text Text
type Skills     = Map Text Int

-- | EntityKind
-- | eHP       : Hit Point
-- | eLvl      : Level, HD
-- | eMaxHP    : Max HP
-- | eName     : unique name "melee/Dagger"
-- | eXP       : Experience
-- | ecolor    : MiniMap color
-- | equipment : doff/don Items
-- | extra     : Skills
-- | glyph     : VisualKind
-- | inventory : Items
-- | kind      : Entity
-- | property  : Descriptions
-- | status    : Temporary Conditions
-- | coord     : Position
-- | 'M' data
-- | tid          : xy in glyph
-- | eAC          : ArmorClass
-- | eEV          : Evasion
-- | eWP          : WillPower
-- | eBlock       : Block?
-- | eCorpse      : Corpse tid
-- | eMove        : Move?
-- | eMP          : Mana Point
-- | eMaxMP       : Max Mana Point
-- | eSpeed       : Speed
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
data EntityKind = EntityKind
  { eHP       :: !Int
  , eLvl      :: !Int
  , eMaxHP    :: !Int
  , eXP       :: !Int
  , ecolor    :: !RGB
  , equipment :: !Equipment
  , extra     :: !Skills
  , glyph     :: !VisualKind
  , inventory :: !Inventory
  , kind      :: !Entity
  , property  :: !Properties
  , status    :: !Conditions
  , coord     :: !Point
  , tid           :: Maybe (Int,Int)
  , eAC           :: Maybe Int
  , eEV           :: Maybe Int
  , eWP           :: Maybe Int
  , eBlock        :: Maybe Bool
  , eCorpse       :: Maybe (Int,Int)
  , eMove         :: Maybe Bool
  , eHoly         :: Maybe EntityHoly
  , eMP           :: Maybe Int
  , eMaxMP        :: Maybe Int
  , eName         :: Maybe Text
  , eSpeed        :: Maybe Int
  , eResAcid      :: Maybe Int
  , eResCold      :: Maybe Int
  , eResFire      :: Maybe Int
  , eResLightning :: Maybe Int
  , eResNecrotic  :: Maybe Int
  , eResPoison    :: Maybe Int
  , eResHoly      :: Maybe Int
  , eResPain      :: Maybe Int
  , eEmote        :: Maybe EntityEmote
  , eHabitat      :: Maybe EntityHabitat
  , eSmart        :: Maybe EntitySmart
  , eSize         :: Maybe EntitySize
  , eUse          :: Maybe EntityUse
  , eWeapon       :: Maybe EntityWeapon
  , iValue   :: Maybe Int
  , iHit     :: Maybe Int
  , iEn      :: Maybe Int
  , iPlus    :: Maybe Int
  , iDmgType :: Maybe EntityDmg
  , iType    :: Maybe EntityType
  , iFeature :: Maybe [EntityFeat]
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
  , extra     = Map.empty
  , glyph     = VArrow
  , inventory = Map.empty
  , kind      = Flavor
  , property  = Map.insert "Name" x Map.empty
  , status    = Map.empty
  , coord     = p
  -- | '@' and 'M'
  , tid = Just (0,0)
  , eAC = Just 0
  , eEV = Just 0
  , eWP = Just 0
  , eBlock       = Just False
  , eCorpse      = Just (0,0)
  , eMove        = Just False
  , eMP          = Nothing
  , eMaxMP       = Nothing
  , eName        = Nothing
  , eSpeed       = Nothing
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
  }

-- | Entity sort
data Entity
  = Actor
  | Sparkle
  | Monster
  | Item
  | Coin
  | Flavor
  | StairDown
  | StairUp
  | Corpse
  | Trap
  deriving (Ord, Show, Eq, Generic)

instance FromJSON Entity
instance ToJSON Entity

-- | I Damage
data EntityDmg
  = Pain
  | Acid
  | Cold
  | Fire
  | Force
  | Lightning
  | Necrotic
  | Poison
  | Radiant
  | Silver
  | Bludgeoning
  | Piercing
  | Slashing
  | Magic
  | Chaos
  | Constrict
  | Gore
  | HeadButt
  | Pounce
  | Reach
  | ReachTongue
  | Spore
  | Sting
  | Swoop
  | TailSlap
  | TentacleSlap
  | Touch
  | Trample
  | TrunkSlap
  | VampireFang
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
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntityEmote
instance ToJSON EntityEmote

-- | I features
data EntityFeat
  = TwoHand
  | Light
  | Finesse
  | Acrobat
  | Blessed
  | Chaotic
  | Cursed
  | Dexterity
  | Distortion
  | Drain
  | Electric
  | Evasion
  | Faith
  | Fear
  | Feral
  | Flame
  | Frost
  | Fly
  | Heavy
  | Hurl
  | Incorporeal
  | Intelligence
  | Life
  | Mage
  | Mana
  | Mob
  | NoCorpse
  | NoMove
  | NoSpawn
  | NoXP
  | NPC
  | Protection
  | Ponderous
  | Rampage
  | Reflect
  | Slay
  | Spectral
  | Speed
  | Stealth
  | Strength
  | Tunnel
  | Vamp
  | Venom
  | Wizard
  | WP
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

-- | M conditions
data EntityST
  = Hurt
  | Awake
  | Blinded
  | Charmed
  | Deafened
  | Exhaustion
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
  | AC
  | COIN
  | DEPTH
  | ENERGY
  | HUNGER
  | MAXDEPTH
  | PREVDEPTH
  | RECALL
  | REGENERATE
  | REJUVENATE
  | SEED
  | STAIRDOWN
  | STAIRUP
  | TARGET
  | Burnt
  | Corroded
  | Frozen
  | Electrocuted
  | Drained
  | Agile
  | Crippled
  | Flight
  | PlusAC
  | PlusCombat
  | PlusEV
  | PlusHP
  | PlusMP
  | PlusMove
  | PlusSH
  | PlusSpeed
  | PlusSpellPower
  | PlusSpellSpeed
  | PlusWonder
  | Smart
  | Sneak
  | Strong
  | Will
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
  | Brand
  | Food
  | Jewelry
  | Potion
  | Rune
  | Scroll
  | Wand
  | Totem
  | RightHand
  | LeftHand
  | Melee1
  | Melee2
  | Melee3
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
  | MonEquipment
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntityUse
instance ToJSON EntityUse

-- | M Weapons
data EntityWeapon = EntityWeapon
  { hurt0 :: Maybe (EntityDmg, Int)
  , hurt1 :: Maybe (EntityDmg, Int)
  , hurt2 :: Maybe (EntityDmg, Int)
  , shoot :: Maybe (EntityDmg, Int)
  }
  deriving (Show, Eq, Generic)

instance FromJSON EntityWeapon
instance ToJSON EntityWeapon
