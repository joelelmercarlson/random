{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-

Game.Library.Kind.Entity.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Library.Kind.Entity (
  AssetMap
  , Energies
  , EntityMap
  , Entity(..)
  , EntityKind(..)
  , EntityHoly(..)
  , EntitySize(..)
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
  | Trap
  | Corpse
  deriving (Ord, Show, Eq, Generic)

instance FromJSON Entity
instance ToJSON Entity

data EntityHoly
  = Holy
  | Natural
  | Undead
  | Demonic
  | NonLiving
  | Plant
  deriving (Ord, Show, Eq, Generic)

instance FromJSON EntityHoly
instance ToJSON EntityHoly

data EntitySize
  = Tiny
  | Small
  | Medium
  | Large
  | Giant
  deriving (Ord, Show, Eq, Generic)

instance FromJSON EntitySize
instance ToJSON EntitySize

-- | EntityKind
-- | eHP       : Hit Point
-- | eLvl      : Level
-- | eMaxHP    : Max HP
-- | eXP       : Experience
-- | ecolor    : MiniMap color
-- | energy    : counters
-- | equipment : doff/don ItemKind
-- | extra     : Skills
-- | glyph     : VisualKind
-- | inventory : Items by slot
-- | kind      : Kind of Entity
-- | property  : Text Descriptions
-- | status    : Temporary status positive and negative
-- | tid       : xy in glyph
-- | coord     : Position
-- | '@' and 'M' data
-- | eBlock       : Movable Entity
-- | eFeral       : Mindless?
-- | eFly         : Fly?
-- | eIncorporeal : Ghost?
-- | eMob         : Mob?
-- | eMove        : Move blocked?
-- | eMP          : Mana Point
-- | eMaxMP       : Max Mana Point
-- | eNoMove      : Can move but doesn't want to?
-- | eNoSpawn     : Spawn?
-- | eNoXP        : XP?
-- | eNPC         : NPC?
-- | eSize        : Size
-- | eSpeed       : Speed
-- | eTunnel      : Digger?
data EntityKind = EntityKind
  { eHP       :: !Int
  , eLvl      :: !Int
  , eMaxHP    :: !Int
  , eXP       :: !Int
  , ecolor    :: !RGB
  , energy    :: !Energies
  , equipment :: !Equipment
  , extra     :: !Energies
  , glyph     :: !VisualKind
  , inventory :: !Inventory
  , kind      :: !Entity
  , property  :: !Properties
  , status    :: !Energies
  , tid       :: !(Int,Int)
  , coord     :: !Point
  , eAC          :: Maybe Int
  , eEV          :: Maybe Int
  , eWP          :: Maybe Int
  , eBlock       :: Maybe Bool
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
  , eSize        :: Maybe EntitySize
  , eSpeed       :: Int
  , eTunnel      :: Maybe Bool
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
  , tid       = (24,49)
  , coord     = p
  -- | '@' and 'M'
  , eAC          = Just 0
  , eEV          = Just 0
  , eWP          = Just 0
  , eBlock       = Just False
  , eFeral       = Nothing
  , eFly         = Nothing
  , eHoly        = Nothing
  , eIncorporeal = Nothing
  , eMob         = Nothing
  , eMove        = Just False
  , eMP          = Nothing
  , eMaxMP       = Nothing
  , eNoMove      = Nothing
  , eNoSpawn     = Nothing
  , eNoXP        = Nothing
  , eNPC         = Nothing
  , eSize        = Nothing
  , eSpeed       = 10
  , eTunnel      = Nothing
  }
