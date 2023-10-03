{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-

Game.Library.Kind.Entity.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Library.Kind.Entity (
  AssetMap
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
import Game.Library.Kind.Visual

-- | Maps used within the game
type AssetMap   = EntityMap
type EntityMap  = Map Int EntityKind
type Equipment  = Map Text EntityKind
type Inventory  = Map Text Int
type Properties = Map Text Text

-- | Entity stack sort.
data Entity
  = Actor
  | Corpse
  | Monster
  | SparkleAim
  | Sparkle
  | Item
  | Coin
  | StairDown
  | StairUp
  | Trap
  | Flavor
  deriving (Ord, Show, Eq, Generic)

instance FromJSON Entity
instance ToJSON Entity

-- | EntityKind
-- coord     = Entity Position
-- block     = Movable Entity
-- kind      = Kind of Entity
-- glyph     = VisualKind of Entity
-- spawn     = Where can the Entity spawn?
-- property  = Textual descriptions of the entity
-- inventory = Items
-- eLvl      = Level of the entity
-- eHP       = HitPoint
-- eMaxHP    = Max HP
-- eMP       = ManaPoint
-- eMaxMP    = Max MP
-- eXP       = Experience
data EntityKind = EntityKind
  { coord      :: Point
  , block      :: Bool
  , kind       :: Entity
  , glyph      :: VisualKind
  , spawn      :: Point
  , equipment  :: Equipment
  , property   :: Properties
  , inventory  :: Inventory
  , eLvl       :: Int
  , eHP        :: Int
  , eMaxHP     :: Int
  , eMP        :: Int
  , eMaxMP     :: Int
  , eXP        :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON EntityKind
instance ToJSON EntityKind

-- | default EntityKind
mkEntityKind :: Text -> Point -> EntityKind
mkEntityKind n p = EntityKind { coord = p
                              , block = False
                              , kind  = Flavor
                              , glyph = VArrow
                              , spawn = p
                              , equipment = Map.empty
                              , property  = Map.insert "Name" n Map.empty
                              , inventory = Map.empty
                              , eLvl   = 1
                              , eHP    = 0
                              , eMaxHP = 0
                              , eMP    = 0
                              , eMaxMP = 0
                              , eXP    = 0
                              }
