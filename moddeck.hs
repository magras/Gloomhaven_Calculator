module GloomhavenModDeck where

import Debug.Trace (traceShow, traceShowId)
import Text.Printf (printf)

type MultiList counter element = [(counter, element)]
type Count = Int
type Deck element = MultiList Count element

type Damage = Int
type Modifier = Damage -> Damage
type ModDeck = Deck Modifier
type DmgDeck = Deck Damage

applyModDeck :: ModDeck -> Damage -> DmgDeck
applyModDeck modDeck baseDmg = fmap (fmap ($baseDmg)) modDeck

doubleDeck :: DmgDeck -> Deck (Damage, Damage)
doubleDeck deck = [(n,(d1,d2)) | let enmDeck = zip [0..] deck,
                                 (i1,(n1,d1)) <- enmDeck,
                                 (i2,(n2,d2)) <- enmDeck,
                                 let n = if i1 == i2 then n1 * (n2 - 1) else n1 * n2]

meanAndVariance :: DmgDeck -> (Float, Float)
meanAndVariance deck =
  (mean, var)
  where
    (count, sum, sum2) = foldr folder (0,0,0) deck
    folder :: (Count, Damage) -> (Count, Damage, Damage) -> (Count, Damage, Damage)
    folder (n,d) (count,sum,sum2) = (count + n, sum + d * n, sum2 + d ^ 2 * n)
    mean = fromIntegral sum / fromIntegral count
    var = fromIntegral sum2 / fromIntegral count - mean ^ 2

normal :: ModDeck -> Damage -> (Float, Float)
normal modDeck baseDmg =
  meanAndVariance $ applyModDeck modDeck baseDmg

advantage :: ModDeck -> Damage -> (Float, Float)
advantage modDeck baseDmg =
 -- traceShow advDeck
  meanAndVariance advDeck
  where
    dmgDeck = applyModDeck modDeck baseDmg
    dblDeck = doubleDeck dmgDeck
    advDeck = fmap (fmap (uncurry max)) dblDeck

disadvantage :: ModDeck -> Damage -> (Float, Float)
disadvantage modDeck baseDmg =
  meanAndVariance $ fmap (fmap (uncurry min)) $ doubleDeck $ applyModDeck modDeck baseDmg


baseDeck :: ModDeck
baseDeck = [
  (1, (max 0) . (*2)),
  (1, (max 0) . (+2)),
  (5, (max 0) . (+1)),
  (6, (max 0) . (+0)),
  (5, (max 0) . subtract 1),
  (1, (max 0) . subtract 2),
  (1, (max 0) . const 0)]

bruteDeck = [
  (1, (max 0) . (*2)),
  (1, (max 0) . (+3)),
  (1, (max 0) . (+2)),
  (11, (max 0) . (+1)),
  (6, (max 0) . (+0)),
  (3, (max 0) . subtract 1),
  (1, (max 0) . subtract 2),
  (1, (max 0) . const 0)]

tinkererDeck = [
  (1, (max 0) . (*2)),
  (1, (max 0) . (+3)),
  (1, (max 0) . (+2)),
  (13, (max 0) . (+1)),
  (8, (max 0) . (+0)),
  (1, (max 0) . subtract 1),
  (0, (max 0) . subtract 2),
  (1, (max 0) . const 0)]

spellweaverDeck = [
  (1, (max 0) . (*2)),
  (5, (max 0) . (+2)),
  (14, (max 0) . (+1)),
  (3, (max 0) . (+0)),
  (3, (max 0) . subtract 1),
  (1, (max 0) . subtract 2),
  (1, (max 0) . const 0)]

scoundrelDeck = [
  (1, (max 0) . (*2)),
  (3, (max 0) . (+2)),
  (6, (max 0) . (+1)),
  (1, (max 0) . (+0)),
  (0, (max 0) . subtract 1),
  (0, (max 0) . subtract 2),
  (1, (max 0) . const 0)]

cragheartDeck = [
  (1, (max 0) . (*2)),
  (5, (max 0) . (+2)),
  (10, (max 0) . (+1)),
  (2, (max 0) . (+0)),
  (2, (max 0) . subtract 1),
  (2, (max 0) . subtract 2),
  (1, (max 0) . const 0)]

cragheartDeck' = [
  (1, (max 0) . (*2)),
  (3, (max 0) . (+2)),
  (10, (max 0) . (+1)),
  (2, (max 0) . (+0)),
  (2, (max 0) . subtract 1),
  (1, (max 0) . subtract 2),
  (1, (max 0) . const 0)]

mindthiefDeck = [
  (1, (max 0) . (*2)),
  (5, (max 0) . (+2)),
  (3, (max 0) . (+1)),
  (3, (max 0) . (+0)),
  (1, (max 0) . subtract 1),
  (0, (max 0) . subtract 2),
  (1, (max 0) . const 0)]

sunkeeperDeck = [
  (1, (max 0) . (*2)),
  (2, (max 0) . (+2)),
  (7, (max 0) . (+1)),
  (2, (max 0) . (+0)),
  (1, (max 0) . subtract 1),
  (0, (max 0) . subtract 2),
  (1, (max 0) . const 0)]


printDeckStats :: ModDeck -> Damage -> IO ()
printDeckStats deck baseDmg = do
  printf "      normal: mean=%.2f var=%.2f delta=%+.2f\n" nrmMean nrmVar (nrmMean - fromIntegral baseDmg)
  printf "   advantage: mean=%.2f var=%.2f delta=%+.2f\n" advMean advVar (advMean - nrmMean)
  printf "disadvantage: mean=%.2f var=%.2f delta=%+.2f\n" disMean disVar (disMean - nrmMean)
  where
    (nrmMean, nrmVar) = normal deck baseDmg
    (advMean, advVar) = advantage deck baseDmg
    (disMean, disVar) = disadvantage deck baseDmg

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

foo :: Damage -> IO ()
foo baseDmg = do
  putStrLn "Base Deck"
  printDeckStats baseDeck baseDmg
  putChar '\n'
  putStrLn "Cragheart Deck"
  printDeckStats cragheartDeck baseDmg
  putChar '\n'
  putStrLn "Cragheart' Deck"
  printDeckStats cragheartDeck' baseDmg
  putChar '\n'

main :: IO ()
main = do
  printAllDeckStats baseDmg
  -- foo baseDmg
  where
    baseDmg = 3
