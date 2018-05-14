module Elf ( genElf
             ) where

  import Character
  import Util

  -- | elf know thy self
  -- | require lots of d10
  genElf :: [Int] -> Character
  genElf m = do
    let fate_t   = [1, 2, 2]
        gender_b = genders (average m)
        height_t = [64, 66]
        wound_t  = [4, 5, 6, 6]

    Character {
      d10_rolls_t = m
      , ws  = 20 + pn 1 m  + pn 2 m
      , bs  = 30 + pn 3 m  + pn 4 m
      , s   = 20 + pn 5 m  + pn 6 m
      , t   = 20 + pn 7 m  + pn 8 m
      , ag  = 30 + pn 9 m  + pn 10 m
      , int = 20 + pn 11 m + pn 12 m
      , wp  = 30 + pn 13 m + pn 14 m
      , fel = 30 + pn 15 m + pn 16 m
      , a   = 1
      , w   = wounds (pn 17 m) wound_t
      , m   = 5
      , fp  = fates (pn 18 m) fate_t
      , race      = "Elf"
      , gender    = gender_b
      , age       = 20 + sum (take 12 m)
      , place     = worlds (pickBirth (pn 20 m)) (pn 21 m) (pn 22 m) places places1 places2
      , eye       = pick (pn 23 m) eyes
      , hair      = pick (pn 24 m) hairs
      , height    = heights gender_b height_t + pn 25 m
      , weight    = 75 + (pn 26 m + pn 27 m) * 5
      , mark      = "Nil"
      , name      = names gender_b (pn 28 m + pn 29 m) female male
      , career    = "basic"
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
