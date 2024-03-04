{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-

Game.Library.Kind.Item.hs

ItemKind generic object

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Library.Kind.Item (
  ItemKind(..)
  , ItemData(..)
  , ItemMap
  ) where

import Prelude hiding (lookup)
import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

type ItemMap = Map Text ItemData

data ItemKind = ItemKind !Text !ItemData deriving (Show, Eq, Generic)

instance FromJSON ItemKind
instance ToJSON ItemKind

data ItemData = ItemData
  { _damage :: Text
  , _detail :: Text
  , _en     :: Text
  , _hit    :: Text
  , _name   :: Text
  , _resistance :: Text
  , _skill  :: Text
  , _spell  :: Text
  , _tile   :: Text
  , _tile1  :: Text
  , _type   :: Text
  , _value  :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ItemData
instance FromJSON ItemData where
  parseJSON (Object v) = do
    x0 <- v .: "Damage"
    x1 <- v .: "Detail"
    x2 <- v .: "En"
    x3 <- v .: "Hit"
    x4 <- v .: "Name"
    x5 <- v .: "Resistance"
    x6 <- v .: "Skill"
    x7 <- v .: "Spell"
    x8 <- v .: "Tile"
    x9 <- v .: "Tile1"
    x10 <- v .: "Type"
    x11 <- v .: "Value"
    return (
      ItemData {
          _damage   = x0
          , _detail = x1
          , _en     = x2
          , _hit    = x3
          , _name   = x4
          , _resistance = x5
          , _skill  = x6
          , _spell  = x7
          , _tile   = x8
          , _tile1  = x9
          , _type   = x10
          , _value  = x11
          }
      )
  parseJSON _ = mempty
