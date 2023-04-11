{-# LANGUAGE OverloadedStrings #-}
{-

Save.hs - load Asset.yaml from <https://github.com/joelelmercarlson/arrow>

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Save (loadFile) where

import Data.Aeson
import Data.Either
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Y
import System.Directory
import Engine.Arrow.World
import Game.Compass
import Game.Library.Kind.Entity

-- | loadFile
loadFile :: IO EntityMap
loadFile = do
  homeDir <- getHomeDirectory
  let dest = homeDir ++ "/Documents/Arrow"
      assetYaml = dest ++ "/Assets.yaml"
      playerJson = dest ++ "/player.json"

  createDirectoryIfMissing True dest
  a <- loadAsset assetYaml
  p <- decodePlayerFromFile playerJson

  print $ fst $ partitionEithers [a]
  print $ fst $ partitionEithers [p]

  let player = case p of
        Left _ -> mkEntityKind "Player" originPoint
        Right x -> x
  return $ case a of
    Left _ -> Map.empty
    Right xs -> Map.insert 0 player xs

-- | loadAsset -- all the Items, Monsters, Traps and more...
loadAsset :: FilePath -> IO (Either Y.ParseException EntityMap)
loadAsset = Y.decodeFileEither
