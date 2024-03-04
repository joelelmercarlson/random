{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-

Game.Library.Kind.Monster.hs

MonsterKind generic object

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Library.Kind.Monster (
  MonsterKind(..)
  , MonsterData(..)
  , MonsterMap
  ) where

import Prelude hiding (lookup)
import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

type MonsterMap = Map Text MonsterData

data MonsterKind = MonsterKind !Text !MonsterData deriving (Show, Eq, Generic)

instance FromJSON MonsterKind
instance ToJSON MonsterKind

data MonsterData = MonsterData
  { _Ac :: Text
  , _Base :: Text
  , _Description :: Text
  , _Detail :: Text
  , _Ev :: Text
  , _Emote :: Text
  , _Hd :: Text
  , _Hp :: Text
  , _Habitat :: Text
  , _Holy :: Text
  , _Hurt0 :: Text
  , _Hurt1 :: Text
  , _Hurt2 :: Text
  , _Intelligence :: Text
  , _Name :: Text
  , _Resistance :: Text
  , _Shoot :: Text
  , _Size :: Text
  , _Speed :: Text
  , _Spell :: Text
  , _Tile :: Text
  , _Use :: Text
  , _Wp :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON MonsterData
instance FromJSON MonsterData where
  parseJSON (Object v) = do
    x0  <- v .: "AC"
    x1  <- v .: "Base"
    x2  <- v .: "Description"
    x3  <- v .: "Detail"
    x4  <- v .: "EV"
    x5  <- v .: "Emote"
    x6  <- v .: "HD"
    x7  <- v .: "HP"
    x8  <- v .: "Habitat"
    x9  <- v .: "Holy"
    x10 <- v .: "Hurt0"
    x11 <- v .: "Hurt1"
    x12 <- v .: "Hurt2"
    x13 <- v .: "Intelligence"
    x14 <- v .: "Name"
    x15 <- v .: "Resistance"
    x16 <- v .: "Shoot"
    x17 <- v .: "Size"
    x18 <- v .: "Speed"
    x19 <- v .: "Spell"
    x20 <- v .: "Tile"
    x21 <- v .: "Use"
    x22 <- v .: "WP"
    return (
      MonsterData {
          _Ac = x0
          , _Base = x1
          , _Description = x2
          , _Detail = x3
          , _Ev = x4
          , _Emote = x5
          , _Hd = x6
          , _Hp = x7
          , _Habitat = x8
          , _Holy = x9
          , _Hurt0 = x10
          , _Hurt1 = x11
          , _Hurt2 = x12
          , _Intelligence = x13
          , _Name = x14
          , _Resistance = x15
          , _Shoot = x16
          , _Size = x17
          , _Speed = x18
          , _Spell = x19
          , _Tile = x20
          , _Use = x21
          , _Wp = x22
          }
      )
  parseJSON _ = mempty
