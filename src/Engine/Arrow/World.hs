{-# LANGUAGE OverloadedStrings #-}
{-

Engine.Arrow.World.hs

Loads the World json

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.World (
  decodePlayerFromFile
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Engine.Arrow.Catch
import Game.Library.Kind.Entity

decodePlayerFromFile :: FilePath -> IO (Either String EntityKind)
decodePlayerFromFile fp = catchShowIO (BS.readFile fp)
  >>= return . either Left eitherDecode
