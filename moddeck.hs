module GloomhavenModDeck where

import Debug.Trace (trace, traceShow, traceShowId)
import Text.Printf (printf)
import Data.Ratio ((%))
import Data.List (nub, sort)
import Data.Monoid
import Control.Applicative
import qualified Data.Map.Strict as Map

type Count = Integer
type Damage = Integer
type Probability = Rational
type Card = String
type Modifier = Damage -> Damage
type Deck = [(Card, Count)]
type DamageDistribution = [(Damage, Probability)]

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
applyModifier card = getModifier $ removeSpecialEffect $ removeRolling $ card

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

normalAttack :: Deck -> Damage -> DamageDistribution
normalAttack deck dmg =
  group $ concatMap applyCard $ drawOneCard $ group $ filterZeroValues $ deck
  where
    applyCard :: (Probability, [Card], Deck) -> DamageDistribution
    applyCard (prob, [card], deck') = scaleProb $
      let d = applyModifier card dmg in
      if isRolling card then
        normalAttack deck' d
      else
        always d
      where
        scaleProb :: DamageDistribution -> DamageDistribution
        scaleProb = map (\(d, p) -> (d, p * prob))

attackWithAdvantage :: Deck -> Damage -> DamageDistribution
attackWithAdvantage deck dmg =
  group $ concatMap applyCards $ drawTwoCards $ group $ filterZeroValues $ deck
  where
    applyCards :: (Probability, [Card], Deck) -> DamageDistribution
    applyCards (prob, [c2,c1], deck') = scaleProb $
      case liftA isRolling [c1, c2] of
        [True, True] -> normalAttack deck' $ applyModifier c2 $ applyModifier c1 $ dmg
        [True, False] -> always $ applyModifier c2 $ applyModifier c1 $ dmg
        [False, True] -> always $ applyModifier c1 $ applyModifier c2 $ dmg
        [False, False] -> always $ applyModifier (bestCard dmg c1 c2) dmg
      where
        scaleProb :: DamageDistribution -> DamageDistribution
        scaleProb = map (\(d, p) -> (d, p * prob))

attackWithDisadvantage :: Deck -> Damage -> DamageDistribution
attackWithDisadvantage deck dmg =
  group $ concatMap applyCards $ drawTwoCards $ group $ filterZeroValues $ deck
  where
    applyCards :: (Probability, [Card], Deck) -> DamageDistribution
    applyCards (prob, [c2,c1], deck') = scaleProb $
      case liftA isRolling [c1, c2] of
        [True, True] -> normalAttack (filter (not . isRolling . fst) deck') dmg
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

printDeckStats :: Deck -> Damage -> IO ()
printDeckStats deck baseDmg = do
  printf "      normal: mean=%.2f sigma=%.2f delta=%+.2f\n" nrmMean (sqrt nrmVar) (nrmMean - fromIntegral baseDmg)
  printf "   advantage: mean=%.2f sigma=%.2f delta=%+.2f\n" advMean (sqrt advVar) (advMean - nrmMean)
  printf "disadvantage: mean=%.2f sigma=%.2f delta=%+.2f\n" disMean (sqrt disVar) (disMean - nrmMean)
  where
    (nrmMean, nrmVar) = meanAndVariance $ normalAttack deck baseDmg
    (advMean, advVar) = meanAndVariance $ attackWithAdvantage deck baseDmg
    (disMean, disVar) = meanAndVariance $ attackWithDisadvantage deck baseDmg

baseDeck :: Deck
baseDeck = [
  ( "x2" , 1),
  ( "+2" , 1),
  ( "+1" , 5),
  ( "+0" , 6),
  ( "-1" , 5),
  ( "-2" , 1),
  (  "0" , 1)]

bruteDeck = [
  ("r+0*", 14),
  ( "x2" , 1),
  ( "+3" , 1),
  ( "+2" , 1),
  ( "+1*", 1),
  ( "+1" , 11),
  ( "+0" , 6),
  ( "-1" , 2),
  ( "-2" , 1),
  (  "0" , 1)]

tinkererDeck = [
  ("r+0*", 5),
  ( "x2" , 1),
  ( "+3" , 1),
  ( "+2" , 1),
  ( "+1*", 6),
  ( "+1" , 7),
  ( "+0*", 1),
  ( "+0" , 7),
  ( "-1" , 1),
  ( "-2" , 0),
  (  "0" , 1)]

spellweaverDeck = [
  ("r+0*", 4),
  ( "x2" , 1),
  ( "+2*", 4),
  ( "+2" , 1),
  ( "+1*", 3),
  ( "+1" , 11),
  ( "+0*", 1),
  ( "+0" , 2),
  ( "-1" , 3),
  ( "-2" , 1),
  (  "0" , 1)]

scoundrelDeck = [
  ("r+1" , 4),
  ("r+0*", 9),
  ( "x2" , 1),
  ( "+2" , 3),
  ( "+1" , 6),
  ( "+0" , 1),
  ( "-1" , 0),
  ( "-2" , 0),
  (  "0" , 1)]

cragheartDeck = [
  ("r+0*", 8),
  ( "x2" , 1),
  ( "+2*", 2),
  ( "+2" , 3),
  ( "+1*", 2),
  ( "+1" , 8),
  ( "+0" , 2),
  ( "-1" , 2),
  ( "-2" , 2),
  (  "0" , 1)]

mindthiefDeck = [
  ("r+1" , 4),
  ("r+0*", 11),
  ( "x2" , 1),
  ( "+2*", 2),
  ( "+2" , 3),
  ( "+1" , 3),
  ( "+0" , 3),
  ( "-1" , 1),
  ( "-2" , 0),
  (  "0" , 1)]

sunkeeperDeck = [
  ("r+1" , 4),
  ("r+0*", 15),
  ( "x2" , 1),
  ( "+2" , 2),
  ( "+1" , 7),
  ( "+0" , 2),
  ( "-1" , 1),
  ( "-2" , 0),
  (  "0" , 1)]

summonerDeck = [
  ("r+0*", 14),
  ( "x2" , 1),
  ( "+2" , 3),
  ( "+1" , 10),
  ( "+0" , 7),
  ( "-1" , 0),
  ( "-2" , 0),
  (  "0" , 1)]

removeRollingFromDeck :: Deck -> Deck
removeRollingFromDeck = filter $ not . isRolling . fst

removeRollingPlusZeroFromDeck :: Deck -> Deck
removeRollingPlusZeroFromDeck = filter $ (/="r+0*") . fst

printDeckVariantStats :: String -> Deck -> Damage -> IO ()
printDeckVariantStats name deck baseDmg = do
  printf "%s Deck - without rolling modifiers\n" name
  printDeckStats (removeRollingFromDeck deck) baseDmg
  putChar '\n'
  printf "%s Deck - without rolling +0\n" name
  printDeckStats (removeRollingPlusZeroFromDeck deck) baseDmg
  putChar '\n'
  printf "%s Deck - full\n" name
  printDeckStats deck baseDmg
  putChar '\n'

printAllDeckStats :: Damage -> IO ()
printAllDeckStats baseDmg = do
  putStrLn "Base Deck"
  printDeckStats baseDeck baseDmg
  putChar '\n'
  printDeckVariantStats "Brute" bruteDeck baseDmg
  printDeckVariantStats "Tinkerer" tinkererDeck baseDmg
  printDeckVariantStats "Spellweaver" spellweaverDeck baseDmg
  printDeckVariantStats "Scoundrel" scoundrelDeck baseDmg
  printDeckVariantStats "Cragheart" cragheartDeck baseDmg
  printDeckVariantStats "Mindthief" mindthiefDeck baseDmg
  printDeckVariantStats "Sunkeeper" sunkeeperDeck baseDmg
  printDeckVariantStats "Summoner" summonerDeck baseDmg

cragheartDeck0 = [
  ( "x2", 1),
  ( "+2", 3),
  ( "+1", 8),
  ( "+0", 2),
  ( "-1", 2),
  ( "-2", 1),
  (  "0", 1)]

cragheartDeck1 = [
  ( "x2", 1),
  ( "+2", 5),
  ( "+1", 8),
  ( "+0", 2),
  ( "-1", 2),
  ( "-2", 2),
  (  "0", 1)]

cragheartDeck2 = [
  ( "x2", 1),
  ( "+2", 3),
  ( "+1", 9),
  ( "+0", 2),
  ( "-1", 2),
  ( "-2", 1),
  (  "0", 1)]

cragheartDeck3 = [
  ( "x2", 1),
  ( "+2", 5),
  ( "+1", 9),
  ( "+0", 2),
  ( "-1", 2),
  ( "-2", 2),
  (  "0", 1)]

cragheartDeck4 = [
  ( "x2", 1),
  ( "+2", 3),
  ( "+1", 10),
  ( "+0", 2),
  ( "-1", 2),
  ( "-2", 1),
  (  "0", 1)]

cragheartDeck5 = [
  ( "x2", 1),
  ( "+2", 5),
  ( "+1", 10),
  ( "+0", 2),
  ( "-1", 2),
  ( "-2", 2),
  (  "0", 1)]

printCragheartDeckVariantStats :: Damage -> IO ()
printCragheartDeckVariantStats baseDmg = do
  putStrLn "Cragheart Deck - Current"
  printDeckStats cragheartDeck0 baseDmg
  putChar '\n'
  putStrLn "Cragheart Deck - Add one (-2) and two (+2)"
  printDeckStats cragheartDeck1 baseDmg
  putChar '\n'
  putStrLn "Cragheart Deck - Add one (+1 IMMOBILIZE)"
  printDeckStats cragheartDeck2 baseDmg
  putChar '\n'
  putStrLn "Cragheart Deck - Add one (-2) and two (+2) and one (+1 IMMOBILIZE)"
  printDeckStats cragheartDeck3 baseDmg
  putChar '\n'
  putStrLn "Cragheart Deck - Add two (+1 IMMOBILIZE)"
  printDeckStats cragheartDeck4 baseDmg
  putChar '\n'
  putStrLn "Cragheart Deck - Add one (-2) and two (+2) and two (+1 IMMOBILIZE)"
  printDeckStats cragheartDeck5 baseDmg
  putChar '\n'

testDeck0 = [
  ("r+2", 0),
  ("r+1", 0),
  ("r+0", 0),
  ( "+4", 0),
  ( "+3", 0),
  ( "+2", 0),
  ( "+1", 0),

  ( "x2", 1),
  ( "+2", 1),
  ( "+1", 5),
  ( "+0", 7),
  ( "-1", 5),
  ( "-2", 0),
  (  "0", 1)]

testDeck1 = [
  ("r+2", 0),
  ("r+1", 2),
  ("r+0", 0),
  ( "+4", 0),
  ( "+3", 0),
  ( "+2", 0),
  ( "+1", 0),

  ( "x2", 1),
  ( "+2", 1),
  ( "+1", 5),
  ( "+0", 6),
  ( "-1", 5),
  ( "-2", 1),
  (  "0", 1)]

printTestDeckStats :: Damage -> IO ()
printTestDeckStats baseDmg = do
  putStrLn "Base Deck"
  printDeckStats baseDeck baseDmg
  putChar '\n'
  putStrLn "Test Deck 0"
  printDeckStats testDeck0 baseDmg
  putChar '\n'
  putStrLn "Test Deck 1"
  printDeckStats testDeck1 baseDmg
  putChar '\n'

main :: IO ()
main = do
  printAllDeckStats baseDmg
  -- printCragheartDeckVariantStats baseDmg
  -- printTestDeckStats baseDmg
  where
    baseDmg = 3
