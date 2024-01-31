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

-- | EntityKind
-- | eHP       = Hit Point
-- | eLvl      = Level
-- | eMP       = Mana Point
-- | eMaxHP    = Max HP
-- | eMaxMP    = Max MP
-- | eXP       = Experience
-- | ecolor    = MiniMap color
-- | energy    = counters
-- | equipment = doff/don ItemKind
-- | extra     = Skills
-- | glyph     = VisualKind
-- | inventory = Items by slot
-- | kind      = Kind of Entity
-- | property  = Text Descriptions
-- | status    = Temporary status positive and negative
-- | tid       = xy in glyph
-- | 'M' Movement
-- | coord = Position
-- | block = Movable Entity
-- | move  = Move blocked?
-- | eIncorporeal = Ghost?
-- | eSpeed       = Speed
-- | eTunnel      = Digger?
-- | eFeral       = Mindless?
data EntityKind = EntityKind
  { eHP       :: !Int
  , eLvl      :: !Int
  , eMP       :: !Int
  , eMaxHP    :: !Int
  , eMaxMP    :: !Int
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
  , coord :: !Point
  , block :: !Bool
  , move  :: !Bool
  , eFeral       :: !Bool
  , eIncorporeal :: !Bool
  , eSpeed       :: !Int
  , eTunnel      :: !Bool
  } deriving (Show, Eq, Generic)

instance FromJSON EntityKind
instance ToJSON EntityKind

-- | default EntityKind
mkEntityKind :: Text -> Point -> EntityKind
mkEntityKind x p =
  EntityKind {
  eHP      = 0
  , eLvl   = 1
  , eMP    = 0
  , eMaxHP = 0
  , eMaxMP = 0
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
  , coord = p
  , tid   = (24,49)
  , block = False
  , move  = False
  , eFeral = False
  , eIncorporeal = False
  , eSpeed = 10
  , eTunnel = False
  }
