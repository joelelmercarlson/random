# Bifrost

Bifrost utilizes the Arrow roguelike engine.

## Table of Contents
- [Abstract](#abstract)
- [Inspiration](#inspiration)
- [Features](#features)
  - [Classes](#classes)
  - [Species](#species)
  - [Elemental](#elemental)
  - [Arcane](#arcane)
  - [Random](#random)
- [Usage](#usage)
- [Requirements](#requirements)
- [Author](#author)


## Abstract
Bifrost is a roguelike developed in Haskell with SDL2. This
project was inspired by RogueBasin Tutorials. Bifrost gameplay comprises
of the Player '@' and a Generic Fantasy World. The World is complete
with Magic, Monsters, and Items to challenge '@'. The goal
is to explore and have Fun!

### Inspiration
&mdash; [RogueBasin.com](http://www.roguebasin.com/index.php/How_to_Write_a_Roguelike_in_15_Steps)

&mdash; [Angband](https://github.com/angband/angband)

### Screenshot
![Screenshot.png](images/Screenshot.png)

## Features
'@' is your character in a classic fantasy world. Bifrost gameplay
is focused on Melee, Shoot, and Zap. The elements of the world 
are wielded with powerful Magic spells.

### Classes
&mdash; `Cleric`, `Fighter`, `Rogue`, and `Wizard`

### Species
&mdash; `Elf`, `Dwarf`, `Halfling`, and `Human`

### Elemental
&mdash; `Acid`, `Cold`, `Fire`; `Lightning` and `Thunder`

### Arcane
&mdash; `Necrotic`, `Radiant`, and `Force`

### Random
&mdash; [Random](https://github.com/joelelmercarlson/Random) for '@' creation.

## Usage

&mdash; vi keys or Arrow Key Movement

&mdash; bump `Monster` to attack

&mdash; `a` to `Acquire` in `Town`

&mdash; `d` to `Drop`

&mdash; `e` to `Eat`

&mdash; `g` to `Get`

&mdash; `i` to `Inventory`

&mdash; `m` to `Magic`

&mdash; `o` to `Open`, `Climb` or `Descend` the `Stairs`

&mdash; `p` to `Pick`, `Dig` in the `Dungeon`

&mdash; `q` to `Quaff`

&mdash; `r` to `Recall` to `Town` or `Dungeon`

&mdash; `t` to `Throw` an `Arrow`

&mdash; `v` to `Target` for `Throw` or `Zap`

&mdash; `w` to `Wield` `Armor`, `Weapon`, `Shield`

&mdash; `x` to `Examine` the `World`

&mdash; `z` to `Zap`

## Requirements
&mdash; [stack](https://haskellstack.org/)

&mdash; [SDL2](https://libsdl.org/)

&mdash; save games live in ```$HOME/Documents/Arrow```

## Author
"Joel E Carlson" &lt;joel.elmer.carlson@gmail.com&gt;
