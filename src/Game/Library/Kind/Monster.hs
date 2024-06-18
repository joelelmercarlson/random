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
  , _Hurt3 :: Text
  , _Intelligence :: Text
  , _Name :: Text
  , _Resistance :: Text
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
    x2  <- v .: "Detail"
    x3  <- v .: "EV"
    x4  <- v .: "Emote"
    x5  <- v .: "HD"
    x6  <- v .: "HP"
    x7  <- v .: "Habitat"
    x8  <- v .: "Holy"
    x10 <- v .: "Hurt0"
    x11 <- v .: "Hurt1"
    x12 <- v .: "Hurt2"
    x13 <- v .: "Hurt3"
    x14 <- v .: "Intelligence"
    x15 <- v .: "Name"
    x16 <- v .: "Resistance"
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
          , _Detail = x2
          , _Ev = x3
          , _Emote = x4
          , _Hd = x5
          , _Hp = x6
          , _Habitat = x7
          , _Holy = x8
          , _Hurt0 = x10
          , _Hurt1 = x11
          , _Hurt2 = x12
          , _Hurt3 = x13
          , _Intelligence = x14
          , _Name = x15
          , _Resistance = x16
          , _Size = x17
          , _Speed = x18
          , _Spell = x19
          , _Tile = x20
          , _Use = x21
          , _Wp = x22
          }
      )
  parseJSON _ = mempty
