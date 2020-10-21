module Hobbit (genHobbit) where
  import Control.Monad.Random
  import DiceSet
  import Character
  import Util

  -- | hobbit know thy self
  -- | require lots of d10
  genHobbit :: IO Character
  genHobbit = do
    let die = (1, 10)
        dice = (2, 20)
        d50 = (5, 10)
    ws <- evalRandIO (rollDice dice)
    bs <- evalRandIO (rollDice dice)
    s <- evalRandIO (rollDice dice)
    t <- evalRandIO (rollDice dice)
    i <- evalRandIO (rollDice dice)
    ag <- evalRandIO (rollDice dice)
    dex <- evalRandIO (rollDice dice)
    int' <- evalRandIO (rollDice dice)
    wp <- evalRandIO (rollDice dice)
    fel <- evalRandIO (rollDice dice)
    age' <- evalRandIO (rollDice d50)
    r0 <- evalRandIO (rollDice die)
    r1 <- evalRandIO (rollDice die)
    r2 <- evalRandIO (rollDice die)
    r3 <- evalRandIO (rollDice die)
    r4 <- evalRandIO (rollDice die)
    r5 <- evalRandIO (rollDice die)
    r6 <- evalRandIO (rollDice die)
    r7 <- evalRandIO (rollDice die)
    r8 <- evalRandIO (rollDice die)
    r9 <- evalRandIO (rollDice die)
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
      , race   = "Hobbit"
      , gender = genders age'
      , age    = 15 + age'
      , place  = worlds (pickBirth r0) r1 r2 places places1 places2
      , eye    = pick r3 eyes
      , hair   = pick r4 hairs
      , height = 37 + r5
      , mark   = pick (r6 + r7) marks
      , name   = names (genders age') (r8 + r9) female male
      , career = "basic"
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
