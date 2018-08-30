module Gloomhaven.AttackModifierDeckCalculator where

import Control.Applicative (liftA)
import Debug.Trace (trace, traceShow, traceShowId)
import Data.Ratio ((%))
import Data.List (sort)
import qualified Data.Map.Strict as Map

type Count = Integer
type Damage = Integer
type Probability = Rational
type Card = String
type Modifier = Damage -> Damage
type Deck = [(Card, Count)]
type DamageDistribution = [(Damage, Probability)]

data AttackType = Normal | Advantage | Disadvantage deriving (Eq, Show)

group :: (Ord key, Num val) => [(key, val)] -> [(key, val)]
group = Map.toList . Map.fromListWith (+)

filterZeroValues :: (Num val, Eq val) => [(key, val)] -> [(key, val)]
filterZeroValues = filter $ (/=0) . snd

removeCard :: Deck -> Card -> Deck
removeCard deck card = filterZeroValues $ map (\(c, n) -> (c, if c == card then n - 1 else n)) $ deck

drawCard :: (Probability, [Card], Deck) -> [(Probability, [Card], Deck)]
drawCard (prob, cards, deck) =
  map (\(c, n) -> (n % s * prob, c : cards, removeCard deck c)) deck
  where s = sum $ map snd deck

drawOneCard :: Deck -> [(Probability, [Card], Deck)]
drawOneCard deck = [(1, [], deck)] >>= drawCard

drawTwoCards :: Deck -> [(Probability, [Card], Deck)]
drawTwoCards deck = [(1, [], deck)] >>= drawCard >>= drawCard

always :: Damage -> DamageDistribution
always dmg = [(dmg, 1)]

isRolling :: Card -> Bool
isRolling = (=='r') . head

hasSpecialEffect :: Card -> Bool
hasSpecialEffect = (=='*') . last

removeRolling :: Card -> Card
removeRolling card = if isRolling card then tail card else card

removeSpecialEffect :: Card -> Card
removeSpecialEffect card = if hasSpecialEffect card then init card else card

getModifier :: Card -> Modifier
getModifier "x2" = (*2)
getModifier "+4" = (+4)
getModifier "+3" = (+3)
getModifier "+2" = (+2)
getModifier "+1" = (+1)
getModifier "+0" = id
getModifier "-1" = subtract 1
getModifier "-2" = subtract 2
getModifier  "0" = const 0
getModifier card = trace ("Unknown card: " ++ show card) undefined

applyModifier :: Card -> Damage -> Damage
applyModifier card = max 0 . modifier
  where modifier = getModifier $ removeSpecialEffect $ removeRolling $ card

compareCards :: Damage -> Card -> Card -> Ordering
compareCards = compareCardsByOfficialRules

compareCardsByOfficialRules :: Damage -> Card -> Card -> Ordering
compareCardsByOfficialRules dmg lhs rhs
  | isRolling lhs || isRolling rhs = undefined
  | bothSpecial = EQ
  | valueOrd == EQ = specialOrd
  | specialOrd == EQ = valueOrd
  | valueOrd == specialOrd = valueOrd
  | otherwise = EQ
  where
    bothSpecial :: Bool
    bothSpecial = all hasSpecialEffect [lhs, rhs]
    specialOrd :: Ordering
    specialOrd = compare (hasSpecialEffect lhs) (hasSpecialEffect rhs)
    valueOrd :: Ordering
    valueOrd = compare (applyModifier lhs dmg) (applyModifier rhs dmg)

compareCardsByHomeRules :: Damage -> Card -> Card -> Ordering
compareCardsByHomeRules dmg lhs rhs =
  compare (applyModifier lhs dmg) (applyModifier rhs dmg)

bestCard :: Damage -> Card -> Card -> Card
bestCard dmg lhs rhs =
  case compareCards dmg lhs rhs of
    LT -> rhs
    _  -> lhs

worstCard :: Damage -> Card -> Card -> Card
worstCard dmg lhs rhs =
  case compareCards dmg lhs rhs of
    GT -> rhs
    _  -> lhs

attack :: AttackType -> Deck -> Damage -> DamageDistribution
attack Normal deck dmg =
  group $ concatMap applyCard $ drawOneCard $ group $ filterZeroValues $ deck
  where
    applyCard :: (Probability, [Card], Deck) -> DamageDistribution
    applyCard (prob, [card], deck') = scaleProb $
      let d = applyModifier card dmg in
      if isRolling card then
        attack Normal deck' d
      else
        always d
      where
        scaleProb :: DamageDistribution -> DamageDistribution
        scaleProb = map (\(d, p) -> (d, p * prob))

attack Advantage deck dmg =
  group $ concatMap applyCards $ drawTwoCards $ group $ filterZeroValues $ deck
  where
    applyCards :: (Probability, [Card], Deck) -> DamageDistribution
    applyCards (prob, [c2,c1], deck') = scaleProb $
      case liftA isRolling [c1, c2] of
        [True, True] -> attack Normal deck' $ applyModifier c2 $ applyModifier c1 $ dmg
        [True, False] -> always $ applyModifier c2 $ applyModifier c1 $ dmg
        [False, True] -> always $ applyModifier c1 $ applyModifier c2 $ dmg
        [False, False] -> always $ applyModifier (bestCard dmg c1 c2) dmg
      where
        scaleProb :: DamageDistribution -> DamageDistribution
        scaleProb = map (\(d, p) -> (d, p * prob))

attack Disadvantage deck dmg =
  group $ concatMap applyCards $ drawTwoCards $ group $ filterZeroValues $ deck
  where
    applyCards :: (Probability, [Card], Deck) -> DamageDistribution
    applyCards (prob, [c2,c1], deck') = scaleProb $
      case liftA isRolling [c1, c2] of
        [True, True] -> attack Normal (filter (not . isRolling . fst) deck') dmg
        [True, False] -> always $ applyModifier c2 dmg
        [False, True] -> always $ applyModifier c1 dmg
        [False, False] -> always $ applyModifier (worstCard dmg c1 c2) dmg
      where
        scaleProb :: DamageDistribution -> DamageDistribution
        scaleProb = map (\(d, p) -> (d, p * prob))

meanAndVariance :: DamageDistribution -> (Float, Float)
meanAndVariance distrib =
  (mean, var)
  where
    (m, m2) = foldr folder (0,0) distrib
    folder :: (Damage, Probability) -> (Rational, Rational) -> (Rational, Rational)
    folder (d,p) (m,m2) = (m + fromIntegral d * p, m2 + fromIntegral d ^ 2 * p)
    mean = fromRational m
    var = fromRational $ m2 - m ^ 2

tailDistribution :: DamageDistribution -> DamageDistribution
tailDistribution =
  scanr1 (\(d,p) (_,prob) -> (d, p + prob)) . sort
