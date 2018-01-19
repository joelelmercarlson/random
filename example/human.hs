#!/usr/bin/env stack
-- stack script --system-ghc --resolver lts-9.18 --package "MonadRandom" --package "text"
  import System.Environment
  import System.Exit
  import System.IO

  import Control.Monad.Random
  import Text.Printf

  -- | dice
  d6 :: (RandomGen g) => Rand g Int
  d6 = getRandomR(1,6)

  d10 :: (RandomGen g) => Rand g Int
  d10 = getRandomR(1,10)

  d12 :: (RandomGen g) => Rand g Int
  d12 = getRandomR(1,12)

  d6s :: (RandomGen g) => Int -> Rand g [Int]
  d6s n = sequence (replicate n d6)

  d10s :: (RandomGen g) => Int -> Rand g [Int]
  d10s n = sequence (replicate n d10)

  d12s :: (RandomGen g) => Int -> Rand g [Int]
  d12s n = sequence (replicate n d12)

  -- | d any
  dx :: (RandomGen g) => Int -> Rand g Int
  dx n = getRandomR(1,n)

  -- | pick
  pick :: Int -> [String] -> String
  pick n m = case nth n m of
              Nothing  -> "Nil"
              (Just y) -> y

  -- | nth safe chooser
  nth :: Int -> [a] -> Maybe a
  nth _ []     = Nothing
  nth 1 (x:_)  = Just x
  nth n (x:xs) = nth (n - 1) xs

  main :: IO ()
  main = do
    xs <- getArgs
    human

  -- | human know thyself
  human :: IO ()
  human = do
    -- | rolls
    ws_r  <- evalRandIO $ d10s 2
    bs_r  <- evalRandIO $ d10s 2
    s_r   <- evalRandIO $ d10s 2
    t_r   <- evalRandIO $ d10s 2
    ag_r  <- evalRandIO $ d10s 2
    int_r <- evalRandIO $ d10s 2
    wp_r  <- evalRandIO $ d10s 2
    fel_r <- evalRandIO $ d10s 2
    b_w   <- evalRandIO $ d10
    b_f   <- evalRandIO $ d10
    -- | birth options
    b_sex   <- evalRandIO $ dx 2
    b_young <- evalRandIO $ d6s 6
    b_wt    <- evalRandIO $ dx 100 
    b_ht    <- evalRandIO $ d10
    b_name  <- evalRandIO $ dx (length male)
    b_r     <- evalRandIO $ dx (length places)
    b_r1    <- evalRandIO $ dx (length places1)
    b_r2    <- evalRandIO $ dx (length places2)
    b_mark  <- evalRandIO $ dx (length marks)
    b_eye   <- evalRandIO $ dx (length eyes)
    b_hair  <- evalRandIO $ dx (length hairs)
    -- | primary stats
    let ws  = 20 + (sum ws_r)
        bs  = 20 + (sum bs_r)
        s   = 20 + (sum s_r)
        t   = 20 + (sum t_r)
        ag  = 20 + (sum ag_r)
        int = 20 + (sum int_r)
        wp  = 20 + (sum wp_r)
        fel = 20 + (sum fel_r)
    -- | secondary stats
        a   = 1  :: Int
        w   = wounds b_w
        sb  = s `div` 10 :: Int
        tb  = t `div` 10 :: Int
        m   = 4 :: Int
        mag = 0 :: Int
        ip  = 0 :: Int
        fp  = fates b_f
    -- | Personal Details
        race   = "Human" 
        gender = (pick b_sex genders)
        name   = names gender b_name
        place  = world b_r b_r1 b_r2
        age    = (sum b_young)
        eye    = (pick b_eye eyes)
        weight = 110 + b_wt
        hair   = (pick b_hair hairs)
        ht     = (heights gender) + b_ht
        ht_ft  = ht `div` 12 
        ht_in  = ht - (ht_ft * 12)
        mark   = (pick b_mark marks)

    printf "%s, the %s from %s\n\n" name race place
    printf "| Age: %-19d | Gender: %-7s |\n" age gender
    printf "| Eye  Color: %-12s | Weight: %3d lbs |\n" eye weight
    printf "| Hair Color: %-12s | Height: %3d\'%2d\" |\n" hair ht_ft ht_in
    printf "| Distinguishing Mark: %-21s |\n\n" mark
    printf "Main Profile\n"
    printf "   | WS | BS | S  | T  | Ag | Int | Wp | Fel |\n"
    printf "   | %-2d | %-2d | %-2d | %-2d | %-2d | %-3d | %-2d | %-3d |\n" ws bs s t ag int wp fel
    printf "Secondary Profile\n"
    printf "   | A  | W  | SB | TB | M  | Mag | IP | FP  |\n"
    printf "   | %-2d | %-2d | %-2d | %-2d | %-2d | %-3d | %-2d | %-3d |\n" a w sb tb m mag ip fp

  -- | birth functions
  genders :: [String]
  genders = [ "Female", "Male" ]

  heights :: String -> Int
  heights n = case n of
               "Female" -> 61
               "Male"   -> 64
               _        -> 59

  names :: String -> Int -> String
  names m n = case m of
                "Female" -> (pick n female)
                "Male"   -> (pick n male)
                _        -> "nil"

  wounds :: Int -> Int
  wounds n = if n >= 1 && n < 4
             then 10
             else if n >= 4 && n < 7
                  then 11
                  else if n >= 7 && n < 9
                       then 12
                       else 13

  fates :: Int -> Int
  fates n = if n >= 1 && n < 5
             then 2
             else if n >= 5 && n < 8
                  then 3
                  else 3

  world :: Int -> Int -> Int -> String
  world m n o = case (pick m places) of
              "Human" -> "a " ++ (pick o places2) ++ " in " ++ (pick n places1)
              _       -> pick m places

  -- | data
  female :: [String]
  female = [ "Alexa"
           , "Alfrida"
           , "Betrix"
           , "Bianka"
           , "Carlott"
           , "Elfrida"
           , "Elise"
           , "Gabrielle"
           , "Gretchen"
           , "Hanna"
           , "Ilsa"
           , "Klara"
           , "Jarla"
           , "Ludmilla"
           , "Mathilde"
           , "Regina"
           , "Solveig"
           , "Theodora"
           , "Ulrike"
           , "Wertha"
           ]

  male :: [String]
  male = [ "Adelbert"
         , "Albrecht"
         , "Berthold"
         , "Dieter"
         , "Eckhardt"
         , "Felix"
         , "Gottfried"
         , "Gustav"
         , "Heinz"
         , "Johann"
         , "Konrad"
         , "Leopold"
         , "Magnus"
         , "Otto"
         , "Pieter"
         , "Rudiger"
         , "Seigfried"
         , "Ulrich"
         , "Waldemar"
         , "Wolfgang"
         ]

  eyes :: [String]
  eyes = [ "Pale Grey"
         , "Grey Blue"
         , "Blue"
         , "Green"
         , "Copper"
         , "Light Brown"
         , "Brown"
         , "Dark Brown"
         , "Purple"
         , "Black"
         ]

  hairs :: [String]
  hairs = [ "Ash Blond"
          , "Corn"
          , "Yellow"
          , "Copper"
          , "Red"
          , "Light Brown"
          , "Brown"
          , "Brown"
          , "Dark Brown"
          , "Black"
          ]

  places :: [String]
  places = [ "Human" ]

  places1 :: [String]
  places1 = [ "Averland"
            , "Hochland"
            , "Middenland"
            , "Nordland"
            , "Ostermark"
            , "Ostland"
            , "Reikland"
            , "Stirland"
            , "Talabecland"
            , "Wissenland"
            ]

  places2 :: [String]
  places2 = [ "City"
            , "Prosperous Town"
            , "Market Town"
            , "Fortified Town"
            , "Farming Village"
            , "Poor Village"
            , "Small Settlement"
            , "Pig/Cattle Farm"
            , "Arable Farm"
            , "Hovel"
            ]

  marks :: [String]
  marks = [ "Pox Marks"
          , "Ruddy Faced"
          , "Scar"
          , "Tattoo"
          , "Earring"
          , "Ragged Ear"
          , "Nose Ring"
          , "Wart"
          , "Broken Nose"
          , "Missing Tooth"
          , "Snaggle Teeth"
          , "Lazy Eye"
          , "Missing Eyebrow(s)"
          , "Missing Digit"
          , "Missing Nail"
          , "Distinctive Gait"
          , "Curious Smell"
          , "Huge Nose"
          , "Large Mole"
          , "Small Bald Patch"
          , "Strange Coloured Eye(s)"
          ]
