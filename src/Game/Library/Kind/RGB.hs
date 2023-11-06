{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-

Game.Library.Kind.RGB

The RGB.

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Game.Library.Kind.RGB (
  RGB (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | RGB color code terrain
data RGB = RGB Int Int Int deriving (Show, Eq, Generic)

instance FromJSON RGB
instance ToJSON RGB
