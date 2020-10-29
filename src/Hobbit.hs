module Hobbit (genHobbit) where
  import DiceSet (d10, twoD10, d50)
  import Character
  import Util

  -- | hobbit know thy self
  -- | require lots of d10
  genHobbit :: IO Character
  genHobbit = do
    ws <- twoD10
    bs <- twoD10
    s <- twoD10
    t <- twoD10
    i <- twoD10
    ag <- twoD10
    dex <- twoD10
    int' <- twoD10
    wp <- twoD10
    fel <- twoD10
    age' <- d50
    r0 <- d10
    r1 <- d10
    r2 <- d10
    r3 <- d10
    r4 <- d10
    r5 <- d10
    r6 <- d10
    r7 <- d10
    r8 <- d10
    r9 <- d10
    return $ Character {
      weaponSkill = 10 + ws
      , ballisticSkill = 30 + bs
      , strength = 10 + s
      , toughness = 20 + t
      , initiative = 20 + i
      , agility = 20 + ag
      , dexterity = 30 + dex
      , intelligence = 20 + int'
      , willpower = 30 + wp
      , fellowship = 30 + fel
      , movement = 3
      , fate = 0
      , resilience = 2
      , race   = "Hobbit"
      , gender = genders age'
      , age    = 15 + age'
      , place  = worlds (pickBirth r0) r1 r2 places places1 places2
      , eye    = pick r3 eyes
      , hair   = pick r4 hairs
      , height = 37 + r5
      , mark   = pick (r6 + r7) marks
      , name   = names (genders age') (r8 + r9) female male
    }

  pickBirth :: Int -> Int
  pickBirth n = if n < 5
                 then 1
                 else 2
  -- | data
  female :: [String]
  female = [ "Nil"
           , "Cerasta Twofoot"
           , "Mahonia Fallohide"
           , "Amaryllis Townsend"
           , "Calaminth Rumble"
           , "Salva Gamwich"
           , "Sapphire Boffin"
           , "Malva Goodchild"
           , "Kalmia Longhole"
           , "Clematis Brown"
           , "Forsythia Proudfoot"
           , "Belba Greenholm"
           , "Poinsetta Gammidge"
           , "Dianthus Brownlock"
           , "Peona Labingi"
           , "Opal Boffin"
           , "Salva Meadows"
           , "Hanna Harfoot"
           , "Salvia Marsh"
           , "Peony Bunce"
           , "Cleoma Hornblower"
           ]


  male :: [String]
  male = [ "Nil"
         , "Wilcome Gardner"
         , "Brocard Diggle"
         , "Haiduc Brown"
         , "Britius Burrow"
         , "Reginar Burrows"
         , "Hartgard Sandheaver"
         , "Reolus Harfoot"
         , "Porro Twofoot"
         , "Brice Gamgee"
         , "Pepin Goldworthy"
         , "Medard Burrow"
         , "Anno Banks"
         , "Turpin Burrows"
         , "Munderic Brockhouse"
         , "Dreux Stoor"
         , "Folmar Chubb"
         , "Orderic Stoor"
         , "Fortinbras Smallburrow"
         , "Jago Sandheaver"
         , "Isengar Brownlock"
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
