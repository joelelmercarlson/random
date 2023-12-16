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
type AssetMap   = EntityMap
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
  | Corpse
  | Coin
  | Item
  | StairDown
  | StairUp
  | Trap
  | Flavor
  deriving (Ord, Show, Eq, Generic)

instance FromJSON Entity
instance ToJSON Entity

-- | EntityKind
-- | block     = Movable Entity
-- | coord     = Position
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
-- | move      = Move blocked
-- | property  = Text Descriptions
-- | status    = Temporary status positive and negative
data EntityKind = EntityKind
  { block     :: !Bool
  , coord     :: !Point
  , eHP       :: !Int
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
  , move      :: !Bool
  , property  :: !Properties
  , status    :: !Energies
  } deriving (Show, Eq, Generic)

instance FromJSON EntityKind
instance ToJSON EntityKind

-- | default EntityKind
mkEntityKind :: Text -> Point -> EntityKind
mkEntityKind x p =
  EntityKind { block = False
  , coord     = p
  , eHP       = 0
  , eLvl      = 1
  , eMP       = 0
  , eMaxHP    = 0
  , eMaxMP    = 0
  , eXP       = 0
  , ecolor    = RGB 255 255 0
  , energy    = Map.insert "speed" 100 Map.empty
  , equipment = Map.empty
  , extra     = Map.empty
  , glyph     = VArrow
  , inventory = Map.empty
  , kind      = Flavor
  , move      = False
  , property  = Map.insert "Name" x Map.empty
  , status    = Map.empty
  }
