module Gloomhaven.AttackModifierDeckCalculator where

import Control.Applicative (liftA)
import Data.Ratio ((%))
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

removeCard :: Card -> Deck -> Deck
removeCard = Map.update (\n -> if n > 1 then Just (n - 1) else Nothing)

drawCard :: (Probability, [Card], Deck) -> [(Probability, [Card], Deck)]
drawCard (prob, cards, deck) =
  Map.foldMapWithKey (\c n -> [(n % (sum deck) * prob, c : cards, removeCard c deck)]) deck

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
  | isRolling lhs || isRolling rhs = error "Rolling card in a comparison function. Something went wrong inside calculator."
  | bothSpecial                    = EQ
  | valueOrd == EQ                 = specialOrd
  | specialOrd == EQ               = valueOrd
  | valueOrd == specialOrd         = valueOrd
  | otherwise                      = EQ
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

attack :: Deck -> AttackType -> Damage -> DamageDistribution
attack deck Normal dmg =
  foldr (Map.unionWith (+)) Map.empty $ map applyCard $ drawOneCard $ deck
  where
    applyCard :: (Probability, [Card], Deck) -> DamageDistribution
    applyCard (prob, [card], deck) = Map.map (*prob) $
      let d = applyModifier card dmg in
      if isRolling card then
        attack deck Normal d
      else
        always d

attack deck Advantage dmg =
  foldr (Map.unionWith (+)) Map.empty $ map applyCards $ drawTwoCards $ deck
  where
    applyCards :: (Probability, [Card], Deck) -> DamageDistribution
    applyCards (prob, [c2,c1], deck) = Map.map (*prob) $
      case liftA isRolling [c1, c2] of
        [True, True] -> attack deck Normal $ applyModifier c2 $ applyModifier c1 $ dmg
        [True, False] -> always $ applyModifier c2 $ applyModifier c1 $ dmg
        [False, True] -> always $ applyModifier c1 $ applyModifier c2 $ dmg
        [False, False] -> always $ applyModifier (bestCard dmg c1 c2) dmg

attack deck Disadvantage dmg =
  foldr (Map.unionWith (+)) Map.empty $ map applyCards $ drawTwoCards $ deck
  where
    applyCards :: (Probability, [Card], Deck) -> DamageDistribution
    applyCards (prob, [c2,c1], deck) = Map.map (*prob) $
      case liftA isRolling [c1, c2] of
        [True, True] -> attack (removeRollingFromDeck deck) Normal dmg
        [True, False] -> always $ applyModifier c2 dmg
        [False, True] -> always $ applyModifier c1 dmg
        [False, False] -> always $ applyModifier (worstCard dmg c1 c2) dmg

meanAndVariance :: DamageDistribution -> (Float, Float)
meanAndVariance distrib =
  (mean, var)
  where
    folder :: Damage -> Probability -> (Rational, Rational) -> (Rational, Rational)
    folder d p (m,m2) = (m + fromIntegral d * p, m2 + fromIntegral d ^ 2 * p)
    (m, m2) = Map.foldrWithKey folder (0,0) distrib
    mean = fromRational m
    var = fromRational $ m2 - m ^ 2

tailDistribution :: Map k Probability -> Map k Probability
tailDistribution =
  snd . Map.mapAccumRWithKey (\prob _ p -> (prob + p, prob + p)) 0

buildDamageDistributionTable :: Deck -> [Damage] -> DamageDistributionTable
buildDamageDistributionTable deck baseDmgRange =
  foldr (\(key, dist) tbl -> Map.insert key dist tbl) Map.empty
    [((atkType, baseDmg), attack deck atkType baseDmg) | baseDmg <- baseDmgRange, atkType <- [Normal, Advantage, Disadvantage]]

buildMeanAndVarianceTable :: DamageDistributionTable -> Map (AttackType, Damage) (Float, Float)
buildMeanAndVarianceTable = Map.map meanAndVariance

buildKillChanceTable :: DamageDistributionTable -> KillChanceTable
buildKillChanceTable = Map.map tailDistribution

killChance :: KillChanceTable -> AttackType -> Damage -> Damage -> Maybe Probability
killChance dict atkType baseDmg resultDmg =
  pure dict >>= Map.lookup (atkType, baseDmg) >>= Map.lookupGE resultDmg >>= return . snd
