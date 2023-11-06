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
-- | coord     = Entity Position
-- | block     = Movable Entity
-- | move      = Move blocked
-- | kind      = Kind of Entity
-- | glyph     = VisualKind of Entity
-- | spawn     = Where can the Entity spawn?
-- | energy    = counters
-- | equipment = equipped ItemKind
-- | inventory = items by name and count
-- | property  = Textual descriptions of the entity
-- | eLvl      = Level of the entity
-- | eHP       = HitPoint
-- | eMaxHP    = Max HP
-- | eMP       = ManaPoint
-- | eMaxMP    = Max MP
-- | eXP       = Experience
-- | ecolor    = MiniMap color
data EntityKind = EntityKind
  { coord     :: !Point
  , block     :: !Bool
  , move      :: !Bool
  , kind      :: !Entity
  , glyph     :: !VisualKind
  , spawn     :: !Point
  , energy    :: !Energies
  , equipment :: !Equipment
  , inventory :: !Inventory
  , property  :: !Properties
  , eLvl      :: !Int
  , eHP       :: !Int
  , eMaxHP    :: !Int
  , eMP       :: !Int
  , eMaxMP    :: !Int
  , eXP       :: !Int
  , ecolor    :: !RGB
  } deriving (Show, Eq, Generic)

instance FromJSON EntityKind
instance ToJSON EntityKind

-- | default EntityKind
mkEntityKind :: Text -> Point -> EntityKind
mkEntityKind x p = EntityKind { coord = p
                              , block = False
                              , move  = False
                              , kind  = Flavor
                              , glyph = VArrow
                              , spawn = p
                              , energy    = Map.empty
                              , equipment = Map.empty
                              , inventory = Map.empty
                              , property  = Map.insert "Name" x Map.empty
                              , eLvl   = 1
                              , eHP    = 0
                              , eMaxHP = 0
                              , eMP    = 0
                              , eMaxMP = 0
                              , eXP    = 0
                              , ecolor = RGB 255 255 0
                              }
