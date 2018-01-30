module Elf ( genElf
             ) where

  import Character
  import Util

  -- | elf know thy self
  -- | require 20 d10 and 5 d20
  genElf :: [Int] -> [Int] -> Character
  genElf m n = do
    let wound_t  = [9,10,11,12]
        fate_t   = [1,2,2]
        height_t = [64, 66]
        g_b      = genders (pn 1 m)
        p_b      = pick_birth (pn 2 n)

    Character {
      d10_rolls_t = m
      , d20_rolls_t = n
      , ws  = 20 + (pn 1 m)  + (pn 2 m)
      , bs  = 30 + (pn 3 m)  + (pn 4 m)
      , s   = 20 + (pn 5 m)  + (pn 6 m)
      , t   = 20 + (pn 7 m)  + (pn 8 m)
      , ag  = 30 + (pn 9 m)  + (pn 10 m)
      , int = 20 + (pn 11 m) + (pn 12 m)
      , wp  = 30 + (pn 13 m) + (pn 14 m)
      , fel = 30 + (pn 15 m) + (pn 16 m)
      , a   = 1
      , w   = wounds (pn 17 m) wound_t
      , m   = 5
      , mag = 0
      , ip  = 0
      , fp  = fates (pn 18 m) fate_t
      , race      = "Elf"
      , gender    = g_b
      , age       = 20 + sum (take 12 m)
      , place     = worlds p_b (pn 2 m) (pn 3 m) places places1 places2
      , eye       = pick (pn 4 m) eyes
      , hair      = pick (pn 5 m) hairs
      , height    = (heights g_b height_t) + (pn 6 m)
      , weight    = 75 + (pn 1 n) * 5
      , mark      = pick (pn 2 n) marks
      , name      = names g_b (pn 3 n) female male
      , wounds_t  = wound_t
      , fates_t   = fate_t
      , heights_t = height_t
    }

  pick_birth :: Int -> Int
  pick_birth n = if n < 4
                 then 1
                 else if n < 8
                      then 2
                      else if n < 12
                           then 3
                           else if n < 16
                                then 4
                                else 5

  -- | data
  female :: [String]
  female = [ "Alane"
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
  male = [ "Aluthol"
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

  marks :: [String]
  marks = [ "Tattoo"
          , "Earring"
          , "Nose Ring"
          , "Strange Coloured Eye(s)"
          , "None"
          , "None"
          , "None"
          , "None"
          , "None"
          , "None"
          ]
