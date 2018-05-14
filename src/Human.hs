module Human ( genHuman
             ) where

  import Character
  import Util

  -- | human know thy self
  -- | require lots of d10
  genHuman :: [Int] -> Character
  genHuman m = do
    let fate_t   = [1, 3, 3]
        gender_b = genders (average m)
        height_t = [60, 64]
        wound_t  = [5, 6, 7, 8]

    Character {
      d10_rolls_t = m
      , ws  = 20 + pn 1 m  + pn 2 m
      , bs  = 20 + pn 3 m  + pn 4 m
      , s   = 20 + pn 5 m  + pn 6 m
      , t   = 20 + pn 7 m  + pn 8 m
      , ag  = 20 + pn 9 m  + pn 10 m
      , int = 20 + pn 11 m + pn 12 m
      , wp  = 20 + pn 13 m + pn 14 m
      , fel = 20 + pn 15 m + pn 16 m
      , a   = 1
      , w   = wounds (pn 17 m) wound_t
      , m   = 4
      , fp  = fates (pn 18 m) fate_t
      , race      = "Human"
      , gender    = gender_b
      , age       = 10 + sum (take 4 m)
      , place     = worlds 1 (pn 20 m) (pn 21 m) places places1 places2
      , eye       = pick (pn 23 m) eyes
      , hair      = pick (pn 24 m) hairs
      , height    = heights gender_b height_t + pn 25 m
      , weight    = 100 + (pn 26 m + pn 27 m) * 5
      , mark      = pick (pn 28 m + pn 29 m) marks
      , name      = names gender_b (pn 30 m + pn 30 m) female male
      , career    = "basic"
    }

  -- | data
  female :: [String]
  female = [ "Nil"
           , "Alexa"
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
  male = [ "Nil"
         , "Adelbert"
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
  marks = [ "Nil"
          , "Pox Marks"
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
