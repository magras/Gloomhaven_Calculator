module Gloomhaven.AttackModifierDeckCalculator where

import Control.Applicative (liftA)
import Data.Ratio ((%))
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Count = Integer
type Damage = Integer
type Probability = Rational
type Card = String
type Modifier = Damage -> Damage
type Deck = Map Card Count
type DamageDistribution = Map Damage Probability
type DamageDistributionTable = Map (AttackType, Damage) DamageDistribution
type KillChanceTable = Map (AttackType, Damage) DamageDistribution

data AttackType = Normal | Advantage | Disadvantage deriving (Eq, Ord, Show)

filterZeroValues :: (Num v, Eq v) => Map k v -> Map k v
filterZeroValues = Map.filter (/=0)

removeCard :: Card -> Deck -> Deck
removeCard = Map.update (\n -> if n > 0 then Just (n - 1) else Nothing)

drawCard :: (Probability, [Card], Deck) -> [(Probability, [Card], Deck)]
drawCard (prob, cards, deck) =
  Map.foldMapWithKey (\c n -> [(n % s * prob, c : cards, removeCard c deck)]) deck
  where s = Map.foldl' (+) 0 deck

drawOneCard :: Deck -> [(Probability, [Card], Deck)]
drawOneCard deck = [(1, [], deck)] >>= drawCard

drawTwoCards :: Deck -> [(Probability, [Card], Deck)]
drawTwoCards deck = [(1, [], deck)] >>= drawCard >>= drawCard

always :: Damage -> DamageDistribution
always dmg = Map.singleton dmg 1

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
getModifier card = error ("Unknown card: " ++ show card)

applyModifier :: Card -> Damage -> Damage
applyModifier card = max 0 . modifier
  where modifier = getModifier $ removeSpecialEffect $ removeRolling $ card

compareCards :: Damage -> Card -> Card -> Ordering
compareCards dmg lhs rhs
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

removeRollingFromDeck :: Deck -> Deck
removeRollingFromDeck = Map.filterWithKey (\card _ -> not $ isRolling card)

attack :: AttackType -> Deck -> Damage -> DamageDistribution
attack Normal deck dmg =
  foldr (Map.unionWith (+)) Map.empty $ map applyCard $ drawOneCard $ filterZeroValues $ deck
  where
    applyCard :: (Probability, [Card], Deck) -> DamageDistribution
    applyCard (prob, [card], deck') = Map.map (*prob) $
      let d = applyModifier card dmg in
      if isRolling card then
        attack Normal deck' d
      else
        always d

attack Advantage deck dmg =
  foldr (Map.unionWith (+)) Map.empty $ map applyCards $ drawTwoCards $ filterZeroValues $ deck
  where
    applyCards :: (Probability, [Card], Deck) -> DamageDistribution
    applyCards (prob, [c2,c1], deck') = Map.map (*prob) $
      case liftA isRolling [c1, c2] of
        [True, True] -> attack Normal deck' $ applyModifier c2 $ applyModifier c1 $ dmg
        [True, False] -> always $ applyModifier c2 $ applyModifier c1 $ dmg
        [False, True] -> always $ applyModifier c1 $ applyModifier c2 $ dmg
        [False, False] -> always $ applyModifier (bestCard dmg c1 c2) dmg

attack Disadvantage deck dmg =
  foldr (Map.unionWith (+)) Map.empty $ map applyCards $ drawTwoCards $ filterZeroValues $ deck
  where
    applyCards :: (Probability, [Card], Deck) -> DamageDistribution
    applyCards (prob, [c2,c1], deck') = Map.map (*prob) $
      case liftA isRolling [c1, c2] of
        [True, True] -> attack Normal (removeRollingFromDeck deck') dmg
        [True, False] -> always $ applyModifier c2 dmg
        [False, True] -> always $ applyModifier c1 dmg
        [False, False] -> always $ applyModifier (worstCard dmg c1 c2) dmg

meanAndVariance :: DamageDistribution -> (Float, Float)
meanAndVariance distrib =
  (mean, var)
  where
    (m, m2) = Map.foldrWithKey folder (0,0) distrib
    folder :: Damage -> Probability -> (Rational, Rational) -> (Rational, Rational)
    folder d p (m,m2) = (m + fromIntegral d * p, m2 + fromIntegral d ^ 2 * p)
    mean = fromRational m
    var = fromRational $ m2 - m ^ 2

tailDistribution :: Map k Probability -> Map k Probability
tailDistribution =
  snd . Map.mapAccumRWithKey (\prob _ p -> (prob + p, prob + p)) 0

buildDamageDistributionTable :: Deck -> [Damage] -> DamageDistributionTable
buildDamageDistributionTable deck baseDmgRange =
  foldr (\(key, dist) tbl -> Map.insert key dist tbl) Map.empty
    [((atkType, baseDmg), attack atkType deck baseDmg) | baseDmg <- baseDmgRange, atkType <- [Normal, Advantage, Disadvantage]]

buildMeanAndVarianceTable :: DamageDistributionTable -> Map (AttackType, Damage) (Float, Float)
buildMeanAndVarianceTable = Map.map meanAndVariance

buildKillChanceTable :: DamageDistributionTable -> KillChanceTable
buildKillChanceTable = Map.map tailDistribution

killChance :: KillChanceTable -> AttackType -> Damage -> Damage -> Maybe Probability
killChance dict atkType baseDmg resultDmg =
  pure dict >>= Map.lookup (atkType, baseDmg) >>= Map.lookupGE resultDmg >>= return . snd
