module Halfling ( genHalfling
             ) where

  import Character
  import Util

  -- | hobbit know thy self
  -- | require 20 d10 and 5 d20
  genHalfling :: [Int] -> [Int] -> Character
  genHalfling m n = do
    let wound_t  = [8,9,10,11]
        fate_t   = [2,2,3]
        height_t = [38, 40]
        g_b = genders (pn 19 m)
        s_b = 10 + (pn 5 m)  + (pn 6 m)
        t_b = 10 + (pn 7 m)  + (pn 8 m)
        p_b = pick_birth (pn 1 m)

    Character {
      d10_rolls_t = m
      , d20_rolls_t = n
      , ws  = 10 + (pn 1 m)  + (pn 2 m)
      , bs  = 30 + (pn 3 m)  + (pn 4 m)
      , s   = s_b
      , t   = t_b
      , ag  = 30 + (pn 9 m)  + (pn 10 m)
      , int = 20 + (pn 11 m) + (pn 12 m)
      , wp  = 30 + (pn 13 m) + (pn 14 m)
      , fel = 30 + (pn 15 m) + (pn 16 m)
      , a   = 1
      , w   = wounds (pn 17 m) wound_t
      , sb  = s_b `div` 10
      , tb  = t_b `div` 10
      , m   = 4
      , mag = 0
      , ip  = 0
      , fp  = fates (pn 18 m) fate_t
      , race      = "Hobbit"
      , gender    = g_b
      , age       = 10 + sum (take 6 m)
      , place     = worlds p_b (pn 2 m) (pn 3 m) places places1 places2
      , eye       = pick (pn 4 m) eyes
      , hair      = pick (pn 5 m) hairs
      , height    = (heights g_b height_t) + (pn 6 m)
      , weight    = 75 + (pn 1 n) * 3
      , mark      = pick (pn 2 n) marks
      , name      = names g_b (pn 3 n) female male
      , wounds_t  = wound_t
      , fates_t   = fate_t
      , heights_t = height_t
    }

  pick_birth :: Int -> Int
  pick_birth n = if n < 5
                 then 1
                 else 2
  -- | data
  female :: [String]
  female = [ "Agnes"
           , "Alice"
           , "Elena"
           , "Eva"
           , "Frida"
           , "Greta"
           , "Hanna"
           , "Heidi"
           , "Hilda"
           , "Janna"
           , "Karin"
           , "Leni"
           , "Marie"
           , "Petra"
           , "Silma"
           , "Sophia"
           , "Susi"
           , "Theda"
           , "Ulla"
           , "Wanda"
           ]


  male :: [String]
  male = [ "Adam"
         , "Albert"
         , "Alfred"
         , "Axel"
         , "Carl"
         , "Edgar"
         , "Hugo"
         , "Jakob"
         , "Ludo"
         , "Max"
         , "Niklaus"
         , "Oskar"
         , "Paul"
         , "Ralf"
         , "Rudi"
         , "Theo"
         , "Thomas"
         , "Udo"
         , "Viktor"
         , "Walter"
         ]

  eyes :: [String]
  eyes = [ "Blue"
         , "Hazel"
         , "Hazel"
         , "Light Brown"
         , "Light Brown"
         , "Brown"
         , "Brown"
         , "Dark Brown"
         , "Dark Brown"
         , "Dark Brown"
         ]

  hairs :: [String]
  hairs = [ "Ash Blond"
          , "Corn"
          , "Yellow"
          , "Yellow"
          , "Copper"
          , "Red"
          , "Light Brown"
          , "Brown"
          , "Dark Brown"
          , "Black"
          ]

  places :: [String]
  places = [ "The Moot"
           , "Human"
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
