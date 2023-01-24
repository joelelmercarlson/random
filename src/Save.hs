{-# LANGUAGE OverloadedStrings #-}
{-

Save.hs

Read asset.yaml from <https://github.com/joelelmercarlson/arrow>


Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Save (loadFile) where

import Data.Aeson
import Data.Either
import qualified Data.Map as Map
import qualified Data.Yaml as Y
import System.Directory
import Game.Kind.Entity
import Game.Kind.Entity

-- | loadFile
-- Expected: $HOME/Documents/Arrow
loadFile :: IO EntityMap
loadFile = do
  homeDir <- getHomeDirectory
  createDirectoryIfMissing False (homeDir ++ "/Documents/Arrow")
  e <- loadAsset (homeDir ++ saveAssetYaml)
  p <- loadPlayer (homeDir ++ savePlayerYaml)
  let assetMap = if null p then head e else Map.insert 0 (head p) (head e)
  return assetMap

-- | loadAsset -- all the Items, Monsters, Traps and more...
loadAsset :: FilePath -> IO [EntityMap]
loadAsset fp = do
  e <- Y.decodeFileEither fp
  let (err, load) = partitionEithers [e]
  print $ fp ++ ", " ++ show err
  return load

-- | loadPlayer -- load '@'
loadPlayer :: FilePath -> IO [EntityKind]
loadPlayer fp = do
  e <- Y.decodeFileEither fp
  let (err, load) = partitionEithers [e]
  print $ fp ++ ", " ++ show err
  return load

-- | saveAsset in YAML
saveAssetYaml :: FilePath
saveAssetYaml = "/Documents/Arrow/asset.yaml"

-- | savePlayer in YAML
savePlayerYaml :: FilePath
savePlayerYaml = "/Documents/Arrow/player.yaml"

-- | touch file in case doesn't exist
touch :: FilePath -> IO ()
touch fp = appendFile fp ""
