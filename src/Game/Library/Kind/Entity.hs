{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-

Game.Library.Kind.Entity.hs

EntityKind can be Actor, Flavor, Item or Monster.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Library.Kind.Entity (
  AssetMap
  , Energies
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
  , EntityType(..)
  , EntityUse(..)
  , Equipment
  , Inventory
  , mkEntityKind
  , Properties
  ) where

import Prelude hiding (lookup)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Game.Compass
import Game.Library.Kind.RGB
import Game.Library.Kind.Visual

-- | Maps used within the game
type AssetMap   = Map Text EntityKind
type Energies   = Map Text Int
type EntityMap  = Map Int EntityKind
type Equipment  = Map Text EntityKind
type Inventory  = Map Int [EntityKind]
type Properties = Map Text Text

-- | Entity stack sort.
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

-- | EntityKind
-- | eHP       : Hit Point
-- | eLvl      : Level, HD
-- | eMaxHP    : Max HP
-- | eXP       : Experience
-- | ecolor    : MiniMap color
-- | energy    : counters
-- | equipment : doff/don Items
-- | extra     : Skills
-- | glyph     : VisualKind
-- | inventory : Items
-- | kind      : Entity
-- | property  : Descriptions
-- | status    : Temporary status positive and negative
-- | coord     : Position
-- | 'M' data
-- | tid          : xy in glyph
-- | eAC          : ArmorClass
-- | eEV          : Evasion
-- | eWP          : WillPower
-- | eBlock       : Block?
-- | eCorpse      : Corpse tid
-- | eFeral       : Mindless?
-- | eFly         : Fly?
-- | eIncorporeal : Ghost?
-- | eMob         : Mob?
-- | eMove        : Move?
-- | eMP          : Mana Point
-- | eMaxMP       : Max Mana Point
-- | eNoMove      : Can move but doesn't want to?
-- | eNoSpawn     : Spawn?
-- | eNoXP        : XP?
-- | eNPC         : NPC?
-- | eSpeed       : Speed
-- | eTunnel      : Digger?
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
  { eHP       :: Int
  , eLvl      :: Int
  , eMaxHP    :: Int
  , eXP       :: Int
  , ecolor    :: RGB
  , energy    :: Energies
  , equipment :: Equipment
  , extra     :: Energies
  , glyph     :: VisualKind
  , inventory :: Inventory
  , kind      :: Entity
  , property  :: Properties
  , status    :: Energies
  , coord     :: Point
  , tid          :: Maybe (Int,Int)
  , eAC          :: Maybe Int
  , eEV          :: Maybe Int
  , eWP          :: Maybe Int
  , eBlock       :: Maybe Bool
  , eCorpse      :: Maybe (Int,Int)
  , eFeral       :: Maybe Bool
  , eFly         :: Maybe Bool
  , eHoly        :: Maybe EntityHoly
  , eIncorporeal :: Maybe Bool
  , eMob         :: Maybe Bool
  , eMove        :: Maybe Bool
  , eMP          :: Maybe Int
  , eMaxMP       :: Maybe Int
  , eNoMove      :: Maybe Bool
  , eNoSpawn     :: Maybe Bool
  , eNoXP        :: Maybe Bool
  , eNPC         :: Maybe Bool
  , eSpeed       :: Maybe Int
  , eTunnel      :: Maybe Bool
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

-- | default EntityKind
mkEntityKind :: Text -> Point -> EntityKind
mkEntityKind x p =
  EntityKind {
  eHP      = 0
  , eLvl   = 1
  , eMaxHP = 0
  , eXP    = 0
  , ecolor = RGB 255 255 0
  , energy    = Map.empty
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
  , eFeral       = Nothing
  , eFly         = Nothing
  , eIncorporeal = Nothing
  , eMob         = Nothing
  , eMove        = Just False
  , eMP          = Nothing
  , eMaxMP       = Nothing
  , eNoMove      = Nothing
  , eNoSpawn     = Nothing
  , eNoXP        = Nothing
  , eNPC         = Nothing
  , eSpeed       = Nothing
  , eTunnel      = Nothing
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
  , iValue = Just 0
  , iHit   = Just 0
  , iEn    = Just 0
  , iPlus  = Just 0
  , iDmgType = Nothing
  , iType    = Nothing
  , iFeature = Nothing
  }

-- | 'I' flavors
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
  | NoneDmg
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntityDmg
instance ToJSON EntityDmg

data EntityFeat
  = TwoHand
  | Light
  | Finesse
  | Activate
  | Charges
  | Strength
  | Dexterity
  | Intelligence
  | WP
  | AC
  | Berserk
  | Brilliance
  | Degeneration
  | Enlightenment
  | Fly
  | Haste
  | Might
  | Stealth
  | NoneFeat
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntityFeat
instance ToJSON EntityFeat

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
  | MiscItem
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntityType
instance ToJSON EntityType

-- | 'M' flavors
data EntityHabitat
  = Land
  | Amphibious
  | AmphibiousLava
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntityHabitat
instance ToJSON EntityHabitat

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

data EntitySize
  = Tiny
  | Small
  | Medium
  | Large
  | Giant
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntitySize
instance ToJSON EntitySize

data EntitySmart
  = Mindless
  | Animal
  | Human
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntitySmart
instance ToJSON EntitySmart

data EntityUse
  = MonNone
  | MonDoor
  | MonEquipment
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntityUse
instance ToJSON EntityUse

data EntityEmote
  = Shout
  | Bark
  | Bellow
  | Bleat
  | Buzz
  | Croak
  | Growl
  | Hello
  | Hiss
  | Howl
  | Meow
  | Moan
  | Ney
  | Roar
  | Scream
  | Screech
  | Silent
  | Squeak
  | Squeal
  | Taunt
  | Trumpet
  deriving (Ord, Read, Show, Eq, Generic)

instance FromJSON EntityEmote
instance ToJSON EntityEmote
