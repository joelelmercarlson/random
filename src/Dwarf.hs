module Dwarf ( genDwarf
             ) where

  import Character
  import Util

  -- | dwarf know thy self
  -- | require 20 d10 and 5 d20
  genDwarf :: [Int] -> [Int] -> Character
  genDwarf m n = do
    let wound_t  = [11,12,13,14]
        fate_t   = [1,2,3]
        height_t = [48, 50]
        g_b      = genders (pn 1 m)

    Character {
      d10_rolls_t = m
      , d20_rolls_t = n
      , ws  = 30 + (pn 1 m)  + (pn 2 m)
      , bs  = 20 + (pn 3 m)  + (pn 4 m)
      , s   = 20 + (pn 5 m)  + (pn 6 m)
      , t   = 30 + (pn 7 m)  + (pn 8 m)
      , ag  = 10 + (pn 9 m)  + (pn 10 m)
      , int = 20 + (pn 11 m) + (pn 12 m)
      , wp  = 40 + (pn 13 m) + (pn 14 m)
      , fel = 10 + (pn 15 m) + (pn 16 m)
      , a   = 1
      , w   = wounds (pn 17 m) wound_t
      , m   = 3
      , mag = 0
      , ip  = 0
      , fp  = fates (pn 18 m) fate_t
      , race      = "Dwarf"
      , gender    = g_b
      , age       = 10 + sum (take 10 m)
      , place     = worlds (pn 1 m) (pn 2 m) (pn 3 m) places places1 places2
      , eye       = pick (pn 4 m) eyes
      , hair      = pick (pn 5 m) hairs
      , height    = (heights g_b height_t) + (pn 6 m)
      , weight    = 90 + (pn 1 n) * 5
      , mark      = pick (pn 2 n) marks
      , name      = names g_b (pn 3 n) female male
      , wounds_t  = wound_t
      , fates_t   = fate_t
      , heights_t = height_t
    }

  -- | data
  female :: [String]
  female = [ "Anika"
           , "Asta"
           , "Astrid"
           , "Berta"
           , "Brigit"
           , "Dagmar"
           , "Elsa"
           , "Erika"
           , "Fanziska"
           , "Greta"
           , "Hunni"
           , "Ingrid"
           , "Janna"
           , "Karin"
           , "Petra"
           , "Sigrid"
           , "Sigrun"
           , "Silma"
           , "Thylda"
           , "Ulla"
           ]

  male :: [String]
  male = [ "Bardin"
         , "Brokk"
         , "Dimzad"
         , "Durak"
         , "Garil"
         , "Gottri"
         , "Grundi"
         , "Hargin"
         , "Imrak"
         , "Kargun"
         , "Jorunn"
         , "Magnar"
         , "Modrin"
         , "Nargond"
         , "Ozad"
         , "Ragnar"
         , "Snorri"
         , "Storri"
         , "Thingrim"
         , "Urgrim"
         ]

  eyes :: [String]
  eyes = [ "Pale Grey"
         , "Blue"
         , "Copper"
         , "Light Brown"
         , "Light Brown"
         , "Brown"
         , "Brown"
         , "Dark Brown"
         , "Dark Brown"
         , "Purple"
         ]

  hairs :: [String]
  hairs = [ "Ash Blond"
          , "Yellow"
          , "Red"
          , "Copper"
          , "Light Brown"
          , "Brown"
          , "Brown"
          , "Dark Brown"
          , "Blue Black"
          , "Black"
          ]

  places :: [String]
  places = [ "Karak Norn (Grey Mountains)"
           , "Karak Izor (the Vaults)"
           , "Karak Hirm (Black Mountains)"
           , "Karak Kadrin (World's Edge Mountains)"
           , "Karaz-A-Karak (World's Edge Mountains)"
           , "Zhufbar (World's Edge Mountains)"
           , "Baraz Varr (the Black Gulf)"
           , "Human"
           , "Human"
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
