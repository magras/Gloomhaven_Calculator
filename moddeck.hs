module GloomhavenModDeck where

import Debug.Trace (trace, traceShow, traceShowId)
import Text.Printf (printf)
import Data.Ratio ((%))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Count = Integer
type Damage = Integer
type Card = String
type Deck = [(Card, Count)]
type DamageDistribution = [(Damage, Rational)]

damageDistributionOfAttack :: (Damage -> Card -> Deck -> DamageDistribution) -> Deck -> Damage -> DamageDistribution
damageDistributionOfAttack applyCard deck dmg =
  group $ normalize $ filterZeroValues $ concatMap processOneCard deck
  where
    group :: (Ord key, Num val) => [(key, val)] -> [(key, val)]
    group = Map.toList . Map.fromListWith (+)

    normalize :: DamageDistribution -> DamageDistribution
    normalize list =
      let s = sum $ map snd list in
      map (\(item, w) -> (item, w / s)) list

    filterZeroValues :: (Num val, Eq val) => [(key, val)] -> [(key, val)]
    filterZeroValues = filter $ (/=0) . snd

    processOneCard :: (Card, Count) -> DamageDistribution
    processOneCard (card, n) = map (scaleWeight n) $ applyCard dmg card (removeCard deck card)

    scaleWeight :: Count -> (Damage, Rational) -> (Damage, Rational)
    scaleWeight n (d, p) = (d, fromIntegral n * p)

    removeCard :: Deck -> Card -> Deck
    removeCard deck card = filter ((>0) . snd) $ map (\(c, n) -> (c, if c == card then n - 1 else n)) $ group deck

always :: Damage -> DamageDistribution
always dmg = [(dmg, 1)]

isRollingModifier :: Card -> Bool
isRollingModifier = (=='r') . head

applyModifier :: Damage -> Card -> Damage
applyModifier dmg "r+2" = dmg + 2
applyModifier dmg "r+1" = dmg + 1
applyModifier dmg "r+0" = dmg
applyModifier dmg  "*2" = dmg * 2
applyModifier dmg  "+4" = dmg + 4
applyModifier dmg  "+3" = dmg + 3
applyModifier dmg  "+2" = dmg + 2
applyModifier dmg  "+1" = dmg + 1
applyModifier dmg  "+0" = dmg
applyModifier dmg  "-1" = dmg - 1
applyModifier dmg  "-2" = dmg - 2
applyModifier dmg   "0" = 0
applyModifier dmg card = trace ("Unknown card: " ++ show card) undefined

applyCardNormally :: Damage -> Card -> Deck -> DamageDistribution
applyCardNormally dmg card deck =
  let d = applyModifier dmg card in
  if isRollingModifier card then
    normalAttack deck d
  else
    always d

applyCardWithAdvantage :: Damage -> Card -> Deck -> DamageDistribution
applyCardWithAdvantage dmg card deck =
  if isRollingModifier card then
    applyCardNormally dmg card deck
  else
    damageDistributionOfAttack (\d c _ -> always $ max (applyModifier dmg card) (applyModifier d c)) deck dmg

applyCardWithDisadvantage :: Damage -> Card -> Deck -> DamageDistribution
applyCardWithDisadvantage dmg card deck =
  if isRollingModifier card then
    attackWithDisadvantage deck dmg
  else
    let dmg' = applyModifier dmg card in
    damageDistributionOfAttack (\d c _ -> always $ if isRollingModifier c then dmg' else min dmg' (applyModifier d c)) deck dmg

normalAttack :: Deck -> Damage -> DamageDistribution
normalAttack = damageDistributionOfAttack applyCardNormally

attackWithAdvantage :: Deck -> Damage -> DamageDistribution
attackWithAdvantage = damageDistributionOfAttack applyCardWithAdvantage

attackWithDisadvantage :: Deck -> Damage -> DamageDistribution
attackWithDisadvantage = damageDistributionOfAttack applyCardWithDisadvantage

meanAndVariance :: DamageDistribution -> (Float, Float)
meanAndVariance distrib =
  (mean, var)
  where
    (m, m2) = foldr folder (0,0) distrib
    folder :: (Damage, Rational) -> (Rational, Rational) -> (Rational, Rational)
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
  ( "*2", 1),
  ( "+2", 1),
  ( "+1", 5),
  ( "+0", 6),
  ( "-1", 5),
  ( "-2", 1),
  (  "0", 1)]

bruteDeck = [
  ( "*2", 1),
  ( "+3", 1),
  ( "+2", 1),
  ( "+1", 11),
  ( "+0", 6),
  ( "-1", 3),
  ( "-2", 1),
  (  "0", 1)]

tinkererDeck = [
  ( "*2", 1),
  ( "+3", 1),
  ( "+2", 1),
  ( "+1", 13),
  ( "+0", 8),
  ( "-1", 1),
  ( "-2", 0),
  (  "0", 1)]

spellweaverDeck = [
  ( "*2", 1),
  ( "+2", 5),
  ( "+1", 14),
  ( "+0", 3),
  ( "-1", 3),
  ( "-2", 1),
  (  "0", 1)]

scoundrelDeck = [
  ( "*2", 1),
  ( "+2", 3),
  ( "+1", 6),
  ( "+0", 1),
  ( "-1", 0),
  ( "-2", 0),
  (  "0", 1)]

cragheartDeck = [
  ( "*2", 1),
  ( "+2", 5),
  ( "+1", 10),
  ( "+0", 2),
  ( "-1", 2),
  ( "-2", 2),
  (  "0", 1)]

mindthiefDeck = [
  ( "*2", 1),
  ( "+2", 5),
  ( "+1", 3),
  ( "+0", 3),
  ( "-1", 1),
  ( "-2", 0),
  (  "0", 1)]

sunkeeperDeck = [
  ( "*2", 1),
  ( "+2", 2),
  ( "+1", 7),
  ( "+0", 2),
  ( "-1", 1),
  ( "-2", 0),
  (  "0", 1)]

printAllDeckStats :: Damage -> IO ()
printAllDeckStats baseDmg = do
  putStrLn "Base Deck"
  printDeckStats baseDeck baseDmg
  putChar '\n'
  putStrLn "Brute Deck"
  printDeckStats bruteDeck baseDmg
  putChar '\n'
  putStrLn "Tinkerer Deck"
  printDeckStats tinkererDeck baseDmg
  putChar '\n'
  putStrLn "Spellweaver Deck"
  printDeckStats spellweaverDeck baseDmg
  putChar '\n'
  putStrLn "Scoundrel Deck"
  printDeckStats scoundrelDeck baseDmg
  putChar '\n'
  putStrLn "Cragheart Deck"
  printDeckStats cragheartDeck baseDmg
  putChar '\n'
  putStrLn "Mindthief Deck"
  printDeckStats mindthiefDeck baseDmg
  putChar '\n'
  putStrLn "Sunkeeper Deck"
  printDeckStats sunkeeperDeck baseDmg
  putChar '\n'

cragheartDeck0 = [
  ( "*2", 1),
  ( "+2", 3),
  ( "+1", 8),
  ( "+0", 2),
  ( "-1", 2),
  ( "-2", 1),
  (  "0", 1)]

cragheartDeck1 = [
  ( "*2", 1),
  ( "+2", 5),
  ( "+1", 8),
  ( "+0", 2),
  ( "-1", 2),
  ( "-2", 2),
  (  "0", 1)]

cragheartDeck2 = [
  ( "*2", 1),
  ( "+2", 3),
  ( "+1", 9),
  ( "+0", 2),
  ( "-1", 2),
  ( "-2", 1),
  (  "0", 1)]

cragheartDeck3 = [
  ( "*2", 1),
  ( "+2", 5),
  ( "+1", 9),
  ( "+0", 2),
  ( "-1", 2),
  ( "-2", 2),
  (  "0", 1)]

cragheartDeck4 = [
  ( "*2", 1),
  ( "+2", 3),
  ( "+1", 10),
  ( "+0", 2),
  ( "-1", 2),
  ( "-2", 1),
  (  "0", 1)]

cragheartDeck5 = [
  ( "*2", 1),
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

  ( "*2", 1),
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

  ( "*2", 1),
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
