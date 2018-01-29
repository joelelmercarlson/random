module Human ( genHuman
             ) where

  import Character
  import Util

  -- | human know thy self
  -- | require 25 d10 and 5 d20
  genHuman :: [Int] -> [Int] -> Character
  genHuman m n = do
    let wound_t  = [10,11,12,13]
        fate_t   = [1,3,3]
        height_t = [60, 64]
        g_b      = genders (pn 1 m)

    Character {
      d10_rolls_t = m
      , d20_rolls_t = n
      , ws  = 20 + (pn 1 m)  + (pn 2 m)
      , bs  = 20 + (pn 3 m)  + (pn 4 m)
      , s   = 20 + (pn 5 m)  + (pn 6 m)
      , t   = 20 + (pn 7 m)  + (pn 8 m)
      , ag  = 20 + (pn 9 m)  + (pn 10 m)
      , int = 20 + (pn 11 m) + (pn 12 m)
      , wp  = 20 + (pn 13 m) + (pn 14 m)
      , fel = 20 + (pn 15 m) + (pn 16 m)
      , a   = 1
      , w   = wounds (pn 17 m) wound_t
      , m   = 4
      , mag = 0
      , ip  = 0
      , fp  = fates (pn 18 m) fate_t
      , dex = 20 + (pn 19 m) + (pn 20 m)
      , ld  = 20 + (pn 21 m) + (pn 22 m)
      , cl  = 20 + (pn 23 m) + (pn 24 m)
      , race      = "Human"
      , gender    = g_b
      , age       = 10 + sum (take 4 m)
      , place     = worlds 1 (pn 2 m) (pn 3 m) places places1 places2
      , eye       = pick (pn 4 m) eyes
      , hair      = pick (pn 5 m) hairs
      , height    = (heights g_b height_t) + (pn 6 m)
      , weight    = 100 + (pn 1 n) * 5
      , mark      = pick (pn 2 n) marks
      , name      = names g_b (pn 3 n) female male
      , wounds_t  = wound_t
      , fates_t   = fate_t
      , heights_t = height_t
    }

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
          , "Distinctive Gait"
          , "Curious Smell"
          , "Huge Nose"
          , "Large Mole"
          , "Small Bald Patch"
          , "Strange Coloured Eye(s)"
          ]
