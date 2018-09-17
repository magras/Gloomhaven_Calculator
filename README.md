# Gloomhaven Attack Modifier Deck Calculator

## Table of Contents

* [Description](#description)
  * [Algorithm](#algorithm)
* [Building](#building)
* [How to use](#how-to-use)
  * [Command line arguments](#command-line-arguments)
  * [Deck format description](#deck-format-description)
  * [Home rules](#home-rules)
  * [Output](#output)
* [TODO](#todo)

## Description

Gloomhaven attack modifier deck calculator produces a kill chance table and some statistical characteristics of a deck based on attack base damage.

### Algorithm

An algorithm used for building probability distribution is simple: calculate a probability of each outcome and merge outcomes with the same result damage. If rolling modifiers involved, the algorithm becomes recursive with a depth of recursion bounded above by N!, and below by N, where N is the number of rolling modifier cards in a deck. On real decks, it works fine but goes out of reach very quick if you try to abuse it.

## Building

To build Gloomhaven Attack Modifier Deck Calculator you need a [Haskell Platform](https://www.haskell.org/downloads#platform). After installing Haskell Platform execute this commands from the root directory of a repository:

    cabal sandbox init
    cabal update
    cabal install

It may take a while to download and build all dependencies. Resulting binary will be inside sandbox directory `./.cabal-sandbox/bin`.

## How to use

### Command line arguments

To run the calculator you need to provide a deck and optional base damage range.

A deck can be read from file or stdin. There are examples of using both options:

    > Gloomhaven-Calculator --deck-file=BaseDeck.json
    +---++--------------------------+
    |   ||             3            |
    +===++==========================+
    | E ||   2.23 /   3.00 /   3.77 |
    | s ||   1.10 /   1.34 /   1.10 |
    | d ||  -0.77 /  +0.00 /  +0.77 |
    +---++--------------------------+
    | 1 ||  90.00 /  95.00 / 100.00 |
    | 2 ||  80.53 /  90.00 /  99.47 |
    | 3 ||  41.05 /  65.00 /  88.95 |
    | 4 ||  11.05 /  35.00 /  58.95 |
    | 5 ||   0.53 /  10.00 /  19.47 |
    | 6 ||    -   /   5.00 /  10.00 |
    +---++--------------------------+

    > Gloomhaven-Calculator --deck-stdin <BaseDeck.json
    +---++--------------------------+
    |   ||             3            |
    +===++==========================+
    | E ||   2.23 /   3.00 /   3.77 |
    | s ||   1.10 /   1.34 /   1.10 |
    | d ||  -0.77 /  +0.00 /  +0.77 |
    +---++--------------------------+
    | 1 ||  90.00 /  95.00 / 100.00 |
    | 2 ||  80.53 /  90.00 /  99.47 |
    | 3 ||  41.05 /  65.00 /  88.95 |
    | 4 ||  11.05 /  35.00 /  58.95 |
    | 5 ||   0.53 /  10.00 /  19.47 |
    | 6 ||    -   /   5.00 /  10.00 |
    +---++--------------------------+

An attack base damage range can be specified with option `--base-damage-range=BEGIN[-END]`:

    > Gloomhaven-Calculator --deck-file=BaseDeck.json --base-damage-range=1-5
    +----++--------------------------+--------------------------+--------------------------+--------------------------+--------------------------+
    |    ||             1            |             2            |             3            |             4            |             5            |
    +====++==========================+==========================+==========================+==========================+==========================+
    |  E ||   0.52 /   1.05 /   1.58 |   1.33 /   2.00 /   2.67 |   2.23 /   3.00 /   3.77 |   3.13 /   4.00 /   4.87 |   4.03 /   5.00 /   5.97 |
    |  s ||   0.69 /   0.92 /   0.82 |   0.92 /   1.14 /   0.92 |   1.10 /   1.34 /   1.10 |   1.32 /   1.58 /   1.32 |   1.57 /   1.84 /   1.57 |
    |  d ||  -0.53 /  +0.05 /  +0.53 |  -0.67 /  +0.00 /  +0.67 |  -0.77 /  +0.00 /  +0.77 |  -0.87 /  +0.00 /  +0.87 |  -0.97 /  +0.00 /  +0.97 |
    +----++--------------------------+--------------------------+--------------------------+--------------------------+--------------------------+
    |  1 ||  41.05 /  65.00 /  88.95 |  80.53 /  90.00 /  99.47 |  90.00 /  95.00 / 100.00 |  90.00 /  95.00 / 100.00 |  90.00 /  95.00 / 100.00 |
    |  2 ||  11.05 /  35.00 /  58.95 |  41.05 /  65.00 /  88.95 |  80.53 /  90.00 /  99.47 |  90.00 /  95.00 / 100.00 |  90.00 /  95.00 / 100.00 |
    |  3 ||    -   /   5.00 /  10.00 |  11.05 /  35.00 /  58.95 |  41.05 /  65.00 /  88.95 |  80.53 /  90.00 /  99.47 |  90.00 /  95.00 / 100.00 |
    |  4 ||    -   /    -   /    -   |   0.53 /  10.00 /  19.47 |  11.05 /  35.00 /  58.95 |  41.05 /  65.00 /  88.95 |  80.53 /  90.00 /  99.47 |
    |  5 ||    -   /    -   /    -   |    -   /    -   /    -   |   0.53 /  10.00 /  19.47 |  11.05 /  35.00 /  58.95 |  41.05 /  65.00 /  88.95 |
    |  6 ||    -   /    -   /    -   |    -   /    -   /    -   |    -   /   5.00 /  10.00 |   0.53 /  10.00 /  19.47 |  11.05 /  35.00 /  58.95 |
    |  7 ||    -   /    -   /    -   |    -   /    -   /    -   |    -   /    -   /    -   |    -   /   5.00 /  10.00 |   0.53 /  10.00 /  19.47 |
    |  8 ||    -   /    -   /    -   |    -   /    -   /    -   |    -   /    -   /    -   |    -   /   5.00 /  10.00 |    -   /   5.00 /  10.00 |
    |  9 ||    -   /    -   /    -   |    -   /    -   /    -   |    -   /    -   /    -   |    -   /    -   /    -   |    -   /   5.00 /  10.00 |
    | 10 ||    -   /    -   /    -   |    -   /    -   /    -   |    -   /    -   /    -   |    -   /    -   /    -   |    -   /   5.00 /  10.00 |
    +----++--------------------------+--------------------------+--------------------------+--------------------------+--------------------------+

    > Gloomhaven-Calculator --deck-file=BaseDeck.json --base-damage-range=10
    +----++--------------------------+
    |    ||            10            |
    +====++==========================+
    |  E ||   8.53 /  10.00 /  11.47 |
    |  s ||   2.96 /   3.30 /   2.96 |
    |  d ||  -1.47 /  +0.00 /  +1.47 |
    +----++--------------------------+
    |  1 ||  90.00 /  95.00 / 100.00 |
    |  2 ||  90.00 /  95.00 / 100.00 |
    |  3 ||  90.00 /  95.00 / 100.00 |
    |  4 ||  90.00 /  95.00 / 100.00 |
    |  5 ||  90.00 /  95.00 / 100.00 |
    |  6 ||  90.00 /  95.00 / 100.00 |
    |  7 ||  90.00 /  95.00 / 100.00 |
    |  8 ||  90.00 /  95.00 / 100.00 |
    |  9 ||  80.53 /  90.00 /  99.47 |
    | 10 ||  41.05 /  65.00 /  88.95 |
    | 11 ||  11.05 /  35.00 /  58.95 |
    | 12 ||   0.53 /  10.00 /  19.47 |
    | 13 ||    -   /   5.00 /  10.00 |
    | 14 ||    -   /   5.00 /  10.00 |
    | 15 ||    -   /   5.00 /  10.00 |
    | 16 ||    -   /   5.00 /  10.00 |
    | 17 ||    -   /   5.00 /  10.00 |
    | 18 ||    -   /   5.00 /  10.00 |
    | 19 ||    -   /   5.00 /  10.00 |
    | 20 ||    -   /   5.00 /  10.00 |
    +----++--------------------------+

Attack base damage doesn't affect probability distribution a lot, but statistical characteristics depend on it, that's why it is required. By default `--base-damage-range=3` used.

### Deck format description

Deck parsed as single JSON object, where name part represents card type and value represents a number of cards of that type.

Card type format is `[r][mod][*]`, where
* `r` prefix means that card has rolling modifier symbol
* `mod` is a value of the modifier for the attack. Possible values:
  * `0` — miss, null, curse
  * `x2` — 2x or bless
  * `-2`, `-1`, `+0`, `+1`, .. — simple additive modifiers
* `*` suffix means that the card has some special effect

Notes:
* `0` and `+0` are very different modifiers. First one means that attack deals no damage, second one — that it deals normal damage.
* There are some special effects that affect result damage, but it isn't clear how to take them into account. Examples known to me are pierce and add target.
* JSON object cannot contain duplicate names. You need to group all duplicated cards into one value.
* JSON standard and my parsing library don't support comments. So if you see `// comment` in this doc, it's just for clarity. It would not work.

Card translation examples:
* +1 Immobilize -> `+1*`
* +0 Stun -> `+0*`
* Rolling +1 -> `r+1`
* Rolling Muddle -> `r+0*`
* Rolling Pierce 3 -> `r+0*`
* Bless -> `x2`
* Curse -> `0`

Whole deck examples:

* BaseDeck.json

      {
         "x2" : 1,
         "+2" : 1,
         "+1" : 5,
         "+0" : 6,
         "-1" : 5,
         "-2" : 1,
          "0" : 1
      }

* MindthiefDeck.json

      {
        "r+1" : 4,
        "r+0*": 11,
         "x2" : 1,
         "+2*": 2,
         "+2" : 3,
         "+1" : 3,
         "+0" : 3,
         "-1" : 1,
         "-2" : 0,
          "0" : 1
      }

### Home rules

This calculator uses official rules to compare cards when an attack has advantage or disadvantage. They are not intuitive, so some groups prefer to use simplified rules. If you want to ignore special effects, modify your deck: replace all cards with special effects with analogs without them. For example, let's take Midthief's deck once again.

Before replacement:

    {
      "r+1" : 4,
      "r+0*": 11,
       "x2" : 1,
       "+2*": 2,
       "+2" : 3,
       "+1" : 3,
       "+0" : 3,
       "-1" : 1,
       "-2" : 0,
        "0" : 1
    }

After:

    {
      "r+1" : 4,
      "r+0" : 11,
       "x2" : 1,
       "+2" : 5, // 3+2
       "+1" : 3,
       "+0" : 3,
       "-1" : 1,
       "-2" : 0,
        "0" : 1
    }

Make sure that there are no duplicates after replacement. This deck would not work because there are two "+2" entries:

    {
      "r+1" : 4,
      "r+0" : 11,
       "x2" : 1,
       "+2" : 2, // duplicate
       "+2" : 3,
       "+1" : 3,
       "+0" : 3,
       "-1" : 1,
       "-2" : 0,
        "0" : 1
    }

### Output

Column header contains base damage range.

Row header consists of two parts
* Statistical characteristics of a deck
  * `E` — expected damage value. Basically, it is an average damage done with an attack.
  * `s` — standard deviation. Basically, how large damage variation.
  * `d` — expected delta from base damage (for normal attack) or from normal attack (for attack with advantage or disadvantage). I find it more convenient to read `d` row instead of `E`, but they show exactly the same information.
* Target health

  On the intersection of base damage and target health, there is the probability of killing that enemy in percents.

Three values inside each cell separated with `/` correspond to disadvantage / normal / advantage.

Let's look at the Scoundrel's full deck:

    {
      "r+1" : 4,
      "r+0*": 9,
       "x2" : 1,
       "+2" : 3,
       "+1" : 6,
       "+0" : 1,
       "-1" : 0,
       "-2" : 0,
        "0" : 1
    }
    
    +----++--------------------------+--------------------------+--------------------------+
    |    ||             1            |             2            |             3            |
    +====++==========================+==========================+==========================+
    |  E ||   1.90 /   2.31 /   2.48 |   2.88 /   3.31 /   3.51 |   3.84 /   4.31 /   4.55 |
    |  s ||   0.84 /   1.09 /   1.06 |   1.13 /   1.34 /   1.28 |   1.47 /   1.65 /   1.58 |
    |  d ||  -0.40 /  +1.31 /  +0.18 |  -0.43 /  +1.31 /  +0.20 |  -0.47 /  +1.31 /  +0.24 |
    +----++--------------------------+--------------------------+--------------------------+
    |  1 ||  89.83 /  91.67 /  93.50 |  89.83 /  91.67 /  93.50 |  89.83 /  91.67 /  93.50 |
    |  2 ||  80.00 /  85.42 /  89.42 |  89.83 /  91.67 /  93.50 |  89.83 /  91.67 /  93.50 |
    |  3 ||  20.50 /  40.00 /  49.17 |  80.00 /  85.42 /  89.42 |  89.83 /  91.67 /  93.50 |
    |  4 ||    -   /  10.89 /  13.56 |  28.00 /  46.25 /  55.58 |  80.00 /  85.42 /  89.42 |
    |  5 ||    -   /   2.03 /   2.03 |    -   /  10.89 /  13.56 |  28.00 /  46.25 /  55.58 |
    |  6 ||    -   /   0.62 /   0.62 |    -   /   3.70 /   4.36 |   6.50 /  17.14 /  20.98 |
    |  7 ||    -   /   0.07 /   0.07 |    -   /   0.62 /   0.62 |    -   /   3.70 /   4.36 |
    |  8 ||    -   /   0.06 /   0.06 |    -   /   0.43 /   0.43 |    -   /   2.29 /   2.96 |
    |  9 ||    -   /   0.00 /   0.00 |    -   /   0.06 /   0.06 |    -   /   0.43 /   0.43 |
    | 10 ||    -   /   0.00 /   0.00 |    -   /   0.06 /   0.06 |    -   /   0.42 /   0.42 |
    | 11 ||    -   /    -   /    -   |    -   /   0.00 /   0.00 |    -   /   0.06 /   0.06 |
    | 12 ||    -   /    -   /    -   |    -   /   0.00 /   0.00 |    -   /   0.06 /   0.06 |
    | 13 ||    -   /    -   /    -   |    -   /    -   /    -   |    -   /   0.00 /   0.00 |
    | 14 ||    -   /    -   /    -   |    -   /    -   /    -   |    -   /   0.00 /   0.00 |
    +----++--------------------------+--------------------------+--------------------------+

So, normal attack (second value in a cell) with base 3 (column labeled with `3`) has expected damage value of 4.31 (row `E`) with standard deviation 1.65 (row `s`). Advantage gives only 0.24 (row `d`, third value in a cell) points of damage, but disadvantage subtracts 0.47 (row `d`, first value in a cell).

Chances to kill 4 health enemy with normal attack 2 is 46.25% (column `2`, row `4`, `2nd` value).
Chances to kill the same enemy with attack 3 with advantage is 89.42% (column `3`, row `4`, `3rd` value).
There is no any chances to kill an enemy with 7 or more health with attack 3 and disadvantage (column `3`, row `7`, `1st` value).
There is some chance to kill an enemy with 14 hp with normal attack 3, but it is less than 1/10000 (column `3`, row `14`, `3rd` value).

## TODO

* restructure project
* use utf if possible (or add --utf flag)
* json output
