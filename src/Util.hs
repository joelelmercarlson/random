{-# LANGUAGE OverloadedStrings #-}
{-

Util.hs

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>

-}
module Util where
import Data.Text (Text)
import qualified Data.Text as T

-- | bonus
abilityMod :: Int -> Int
abilityMod n = (n-10) `div` 2

abilityFmt :: Int -> Text
abilityFmt n = let
  s = (n-10) `div` 2
  stat
    | s > 0 = T.append "+" (T.pack $ show s)
    | s < 0 = T.pack $ show s
    | otherwise = "+0"
  in stat

-- | average
average :: [Int] -> Float
average [] = 1
average n = fromIntegral (sum n) / fromIntegral (length n)

-- | clamp
clamp :: Int -> Int
clamp n = if n > 0 then n else 0

-- | adventure genders
genders :: Int -> Text
genders n
  | n < 5 = "Female"
  | otherwise = "Male"

-- | height calculator
heightF :: Int -> Text
heightF x = T.concat [ T.pack $ show ht_f,  "\'", T.pack $ show ht_i, "\"" ]
  where
    ht_f = x `div` 12
    ht_i = x - (ht_f * 12)

-- name picker
names :: Text -> Int -> [Text] -> [Text] -> Text
names m n female male
  | m == "Female" = pick n female
  | m == "Male" = pick n male
  | otherwise = "nil"

-- | nth safe chooser
nth :: Int -> [a] -> Maybe a
nth _ []     = Nothing
nth 1 (x:_)  = Just x
nth n (_:xs) = nth (n - 1) xs

-- | pick
pick :: Int -> [Text] -> Text
pick x xs = case nth x xs of
  Nothing  -> "nil"
  (Just y) -> y
