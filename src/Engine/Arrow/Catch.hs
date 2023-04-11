{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-

Engine.Arrow.Catch.hs

Engine.Arrow.Catch Exceptions

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Engine.Arrow.Catch (
  catchShowIO
  ) where

import Prelude hiding (lookup)
import Control.Exception (IOException)
import qualified Control.Exception as Exception

catchShowIO :: IO a -> IO (Either String a)
catchShowIO action = fmap Right action `Exception.catch` handleIOException
  where
    handleIOException :: IOException -> IO (Either String a)
    handleIOException = return . Left . show
