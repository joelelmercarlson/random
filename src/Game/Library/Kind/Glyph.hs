{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-

Game.Library.Kind.Glyph.hs

GlyphKind texcoord

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Library.Kind.Glyph (
  GlyphKind(..)
  , GlyphData(..)
  ) where

import Prelude hiding (lookup)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data GlyphKind = GlyphKind !Text !GlyphData deriving (Show, Eq, Generic)

instance FromJSON GlyphKind
instance ToJSON GlyphKind

data GlyphData = GlyphData
  { w :: Int
  , h :: Int
  , ox :: Int
  , oy :: Int
  , sx :: Int
  , sy :: Int
  , ex :: Int
  , ey :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON GlyphData
instance ToJSON GlyphData
