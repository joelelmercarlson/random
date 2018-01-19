# random.hs

Random numbers within haskell utilizing MonadRandom and stack in script mode. This is an example program for simulating multiple dice rolls.

* [MonadRandom](https://hackage.haskell.org/package/MonadRandom)

## DiceSet

DiceSet is the primary data type utilized within the program to capture common rolls like 3 d6 + 1

```
  -- | DiceSet "d6" 3 0
  data DiceSet = DiceSet String Int Int deriving (Show)

  dieType :: DiceSet -> String
  dieType (DiceSet x _ _) = x

  dieAmt :: DiceSet -> Int
  dieAmt (DiceSet _ x _) = x

  dieMod :: DiceSet -> Int
  dieMod (DiceSet _ _ x) = x
```

## Usage

```
./random.hs 3 d6 1

3d6 +/- 1 [6,2,1] = 10
```
