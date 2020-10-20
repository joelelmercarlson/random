module Elf (genElf) where
  import Character
  import Util

  -- | elf know thy self
  -- | require lots of d10
  genElf :: [Int] -> Character
  genElf m = do
    Character {
      d10_rolls_t = m
      , weaponSkill  = 30 + pn 1 m + pn 2 m
      , ballisticSkill  = 30 + pn 3 m + pn 4 m
      , strength  = 20 + pn 5 m + pn 6 m
      , toughness = 20 + pn 7 m + pn 8 m
      , initiative = 40 + pn 9 m + pn 10 m
      , agility = 30 + pn 11 m + pn 12 m
      , dexterity = 30 + pn 13 m + pn 14 m
      , intelligence = 30 + pn 15 m + pn 16 m
      , willpower = 30 + pn 17 m + pn 18 m
      , fellowship = 20 + pn 19 m + pn 20 m
      , movement  = 5
      , fate = 0
      , race   = "Elf"
      , gender = genders (average m)
      , age    = 30 + sum (take 10 m)
      , place  = worlds (pickBirth (pn 21 m)) (pn 22 m) (pn 23 m) places places1 places2
      , eye    = pick (pn 24 m) eyes
      , hair   = pick (pn 25 m) hairs
      , height = 71 + pn 1 m
      , weight = 75 + (pn 26 m + pn 27 m) * 5
      , mark   = "Nil"
      , name   = names (genders (average m)) (pn 28 m + pn 29 m) female male
      , career = "basic"
    }

  pickBirth :: Int -> Int
  pickBirth n
    | n < 2 = 1
    | n < 4 = 2
    | n < 6 = 3
    | n < 8 = 4
    | otherwise = 5

  -- | data
  female :: [String]
  female = [ "Nil"
           , "Alane"
           , "Altronia"
           , "Davandrel"
           , "Eldril"
           , "Eponia"
           , "Fanriel"
           , "Filamir"
           , "Gallina"
           , "Halion"
           , "Illudil"
           , "Ionor"
           , "Lindara"
           , "Lorandara"
           , "Maruviel"
           , "Pelgrana"
           , "Siluvaine"
           , "Tallana"
           , "Ulliana"
           , "Vivandrel"
           , "Yuviel"
           ]

  male :: [String]
  male = [ "Nil"
         , "Aluthol"
         , "Aamendil"
         , "Angran"
         , "Cavindel"
         , "Dolwen"
         , "Eldillor"
         , "Falandar"
         , "Farnoth"
         , "Gildiril"
         , "Harrond"
         , "Imhol"
         , "Larandar"
         , "Laurenor"
         , "Mellion"
         , "Mormacar"
         , "Ravandil"
         , "Torendil"
         , "Urdithane"
         , "Valahuir"
         , "Yavandir"
         ]

  eyes :: [String]
  eyes = [ "Grey Blue"
         , "Blue"
         , "Green"
         , "Copper"
         , "Light Brown"
         , "Brown"
         , "Dark Brown"
         , "Silver"
         , "Purple"
         , "Black"
         ]

  hairs :: [String]
  hairs = [ "Silver"
          , "Ash Bond"
          , "Corn"
          , "Yellow"
          , "Copper"
          , "Light Brown"
          , "Light Brown"
          , "Brown"
          , "Dark Brown"
          , "Black"
          ]

  places :: [String]
  places = [ "City of Altdorf"
           , "City of Marienburg"
           , "Laurelorn Forest"
           , "The Great Forest"
           , "Reikwald Forest"
           ]

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
