{-# LANGUAGE RecordWildCards #-}

module Dwarf ( genDwarf
             , king
             , thane
             , dragonSeeker
             , runicSmith
             , greyBeard
             , deepWatch
             , clanWarrior
             ) where

  import Character
  import Util

  -- | dwarf know thy self
  -- | require lots of d10
  genDwarf :: [Int] -> Character
  genDwarf m = do
    let fate_t   = [1, 2, 3]
        gender_b = genders (average m)
        height_t = [48, 50]
        wound_t  = [6, 7, 8, 9]

    Character {
      d10_rolls_t = m
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
      , fp  = fates (pn 18 m) fate_t
      , race      = "Dwarf"
      , gender    = gender_b
      , age       = 10 + sum (take 10 m)
      , place     = worlds (pn 20 m) (pn 21 m) (pn 22 m) places places1 places2
      , eye       = pick (pn 23 m) eyes
      , hair      = pick (pn 24 m) hairs
      , height    = (heights gender_b height_t) + (pn 25 m)
      , weight    = 90 + ((pn 26 m) + (pn 27 m)) * 5
      , mark      = pick ((pn 28 m) + (pn 29 m)) marks
      , name      = names gender_b ((pn 30 m) + (pn 31 m)) female male
      , career    = "basic"
    }

  -- | RecordWildCards syntax
  king :: Character -> Character
  king n@Character{..} = n { ws=ws+31, bs=bs+11, s=s+11, t=t+11, w=15, ag=ag+21, a=a+3, wp=wp+31, fel=fel+21, career="King - Lords of Stone" }

  thane :: Character -> Character
  thane n@Character{..} = n { ws=ws+21, bs=bs+11, s=s+11, t=t+11, w=15, ag=ag+11, a=a+2, wp=wp+21, fel=fel+21, career="Thane - Lords of Stone" }

  dragonSeeker :: Character -> Character
  dragonSeeker n@Character{..} = n { ws=ws+31, bs=bs+11, s=s+31, t=t+11, w=15, ag=ag+31, a=a+4, wp=wp+21, fel=fel+21, career="Hero - Dragon Seeker" }

  runicSmith :: Character -> Character
  runicSmith n@Character{..} = n { ws=ws+11, s=s+11, w=15, a=a+1, wp=wp+11, fel=fel+11, career="Hero - Runic Smith" }

  greyBeard :: Character -> Character
  greyBeard n@Character{..} = n { ws=ws+11, s=s+11, ag=ag+11, wp=wp+11, fel=fel+11, career="Veteran - Greybeards" }

  deepWatch :: Character -> Character
  deepWatch n@Character{..} = n { ws=ws+11, s=s+11, ag=ag+11, a=a+1, wp=wp+11, fel=fel+11, career="Elite - Deep Watch" }

  clanWarrior :: Character -> Character
  clanWarrior n@Character{..} = n { s=s+11, ag=ag+11, wp=wp+11, fel=fel+11, career="Veteran - Clan Warriors" }

  -- | Data Tables
  female :: [String]
  female = [ "Nil"
           , "Anika"
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
  male = [ "Nil"
         , "Bardin"
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
