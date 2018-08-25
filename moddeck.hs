module GloomhavenModDeck where

import Debug.Trace (trace, traceShow, traceShowId)
import Text.Printf (printf)
import Data.Ratio ((%))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type WeightedList weight item = [(weight, item)]
type Count = Integer
type Deck item = WeightedList Count item

type Damage = Integer
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



cragheartDeck0 = [
  (1, (max 0) . (*2)),
  (3, (max 0) . (+2)),
  (8, (max 0) . (+1)),
  (2, (max 0) . (+0)),
  (2, (max 0) . subtract 1),
  (1, (max 0) . subtract 2),
  (1, (max 0) . const 0)]

cragheartDeck1 = [
  (1, (max 0) . (*2)),
  (5, (max 0) . (+2)),
  (8, (max 0) . (+1)),
  (2, (max 0) . (+0)),
  (2, (max 0) . subtract 1),
  (2, (max 0) . subtract 2),
  (1, (max 0) . const 0)]

cragheartDeck2 = [
  (1, (max 0) . (*2)),
  (3, (max 0) . (+2)),
  (9, (max 0) . (+1)),
  (2, (max 0) . (+0)),
  (2, (max 0) . subtract 1),
  (1, (max 0) . subtract 2),
  (1, (max 0) . const 0)]

cragheartDeck3 = [
  (1, (max 0) . (*2)),
  (5, (max 0) . (+2)),
  (9, (max 0) . (+1)),
  (2, (max 0) . (+0)),
  (2, (max 0) . subtract 1),
  (2, (max 0) . subtract 2),
  (1, (max 0) . const 0)]

cragheartDeck4 = [
  (1, (max 0) . (*2)),
  (3, (max 0) . (+2)),
  (10, (max 0) . (+1)),
  (2, (max 0) . (+0)),
  (2, (max 0) . subtract 1),
  (1, (max 0) . subtract 2),
  (1, (max 0) . const 0)]

cragheartDeck5 = [
  (1, (max 0) . (*2)),
  (5, (max 0) . (+2)),
  (10, (max 0) . (+1)),
  (2, (max 0) . (+0)),
  (2, (max 0) . subtract 1),
  (2, (max 0) . subtract 2),
  (1, (max 0) . const 0)]

printDeckStats :: ModDeck -> Damage -> IO ()
printDeckStats deck baseDmg = do
  printf "      normal: mean=%.2f sigma=%.2f delta=%+.2f\n" nrmMean (sqrt nrmVar) (nrmMean - fromIntegral baseDmg)
  printf "   advantage: mean=%.2f sigma=%.2f delta=%+.2f\n" advMean (sqrt advVar) (advMean - nrmMean)
  printf "disadvantage: mean=%.2f sigma=%.2f delta=%+.2f\n" disMean (sqrt disVar) (disMean - nrmMean)
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


applyCard :: Damage -> AttackModifierCard -> Deck AttackModifierCard -> WeightedList Rational Damage
applyCard dmg (AttackModifierCard  "*2") _ = [(1, dmg * 2)]
applyCard dmg (AttackModifierCard  "+2") _ = [(1, dmg + 2)]
applyCard dmg (AttackModifierCard  "+1") _ = [(1, dmg + 1)]
applyCard dmg (AttackModifierCard  "+0") _ = [(1, dmg    )]
applyCard dmg (AttackModifierCard  "-1") _ = [(1, dmg - 1)]
applyCard dmg (AttackModifierCard  "-2") _ = [(1, dmg - 2)]
applyCard dmg (AttackModifierCard   "0") _ = [(1, 0      )]
applyCard dmg (AttackModifierCard "r+1") deck = normalAttack deck (dmg + 1)
applyCard _ _ _ = undefined

bar :: ModDeck -> Damage -> (Float, Float)
bar modDeck baseDmg =
  meanAndVariance $
  traceShowId $
  fmap (fmap (\m -> m baseDmg)) modDeck

-- type AttackModifierCard = String
data AttackModifierCard = AttackModifierCard {idnt::String} -- CRAP: id will shadow standard function id?
  deriving (Eq, Show)

-- TODO: group equal damge
normalize :: WeightedList Rational item -> WeightedList Rational item
normalize wl =
  map (\(w, item) -> (w / s, item)) wl
  where
    s = sum $ map fst wl

groupEqualItems :: WeightedList weight item -> WeightedList weight item
groupEqualItems = undefined

removeCard :: Eq card => Deck card -> card -> Deck card
removeCard deck card =
  filter (\(n,_) -> n > 0) $
  map (\(n,c) -> (if c == card then n - 1 else n, c)) deck

normalAttack :: Deck AttackModifierCard -> Damage -> WeightedList Rational Damage
normalAttack modDeck baseDmg =
  -- undefined
  -- traceShowId foo
  normDeck
  where
    normDeck :: WeightedList Rational Damage
    normDeck = normalize foo
    foo :: WeightedList Rational Damage
    foo = concatMap bar modDeck
    bar :: (Count, AttackModifierCard) -> WeightedList Rational Damage
    bar (n, m) = map (scaleWeight n) $ applyCard baseDmg m (traceShowId $ removeCard modDeck m)
    scaleWeight :: Count -> (Rational, Damage) -> (Rational, Damage)
    scaleWeight n (w, d) = (fromIntegral n * w, d)


cragheartDeck' = [
  (1, AttackModifierCard("*2")),
  (3, AttackModifierCard("+2")),
  (7, AttackModifierCard("+1")),
  (6, AttackModifierCard("+0")),
  (3, AttackModifierCard("-1")),
  (1, AttackModifierCard("-2")),
  (1, AttackModifierCard( "0"))]

baseDeck' = [
  (1, AttackModifierCard("r+1")),
  (1, AttackModifierCard( "*2")),
  (1, AttackModifierCard( "+2")),
  (5, AttackModifierCard( "+1")),
  (6, AttackModifierCard( "+0")),
  (5, AttackModifierCard( "-1")),
  (1, AttackModifierCard( "-2")),
  (1, AttackModifierCard(  "0"))]

type AttackModifierCard' = String
type AttackModifierDeck = [(AttackModifierCard', Count)]

attack' :: (Damage -> AttackModifierCard' -> AttackModifierDeck -> [(Damage, Rational)]) -> AttackModifierDeck -> Damage -> [(Damage, Rational)]
attack' applyCard deck dmg =
  group' $ normalize' $ filterZeroValues $ concatMap processOneCard deck
  where
    group' :: (Ord key, Num val) => [(key, val)] -> [(key, val)]
    group' = Map.toList . Map.fromListWith (+)

    normalize' :: [(Damage, Rational)] -> [(Damage, Rational)]
    normalize' list =
      let s = sum $ map snd list in
      map (\(item, w) -> (item, w / s)) list

    filterZeroValues :: (Num val, Eq val) => [(key, val)] -> [(key, val)]
    filterZeroValues = filter $ (/=0) . snd

    processOneCard :: (AttackModifierCard', Count) -> [(Damage, Rational)]
    processOneCard (card, n) = map (scaleWeight n) $ applyCard dmg card (removeCard deck card)

    scaleWeight :: Count -> (Damage, Rational) -> (Damage, Rational)
    scaleWeight n (d, p) = (d, fromIntegral n * p)

    removeCard :: AttackModifierDeck -> AttackModifierCard' -> AttackModifierDeck
    removeCard deck card = filter ((>0) . snd) $ map (\(c, n) -> (c, if c == card then n - 1 else n)) $ group' deck


normalAttack' :: AttackModifierDeck -> Damage -> [(Damage, Rational)]
normalAttack' = attack' applyCardNormally

attackWithAdvantage' :: AttackModifierDeck -> Damage -> [(Damage, Rational)]
attackWithAdvantage' = attack' applyCardWithAdvantage

attackWithDisadvantage' :: AttackModifierDeck -> Damage -> [(Damage, Rational)]
attackWithDisadvantage' = attack' applyCardWithDisadvantage

applyCardNormally :: Damage -> AttackModifierCard' -> AttackModifierDeck -> [(Damage, Rational)]
applyCardNormally dmg card deck =
  let d = applyModifier dmg card in
  if isRollingModifier card then
    normalAttack' deck d
  else
    always d

applyCardWithAdvantage :: Damage -> AttackModifierCard' -> AttackModifierDeck -> [(Damage, Rational)]
applyCardWithAdvantage dmg card deck =
  if isRollingModifier card then
    applyCardNormally dmg card deck
  else
    attack' (\d c _ -> always $ max (applyModifier dmg card) (applyModifier d c)) deck dmg

applyCardWithDisadvantage :: Damage -> AttackModifierCard' -> AttackModifierDeck -> [(Damage, Rational)]
applyCardWithDisadvantage dmg card deck =
  if isRollingModifier card then
    attackWithDisadvantage' deck dmg
  else
    let dmg' = applyModifier dmg card in
    attack' (\d c _ -> always $ if isRollingModifier c then dmg' else min dmg' (applyModifier d c)) deck dmg

always :: Damage -> [(Damage, Rational)]
always dmg = [(dmg, 1)]

isRollingModifier :: AttackModifierCard' -> Bool
isRollingModifier = (=='r') . head

applyModifier :: Damage -> AttackModifierCard' -> Damage
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

meanAndVariance' :: [(Damage, Rational)] -> (Float, Float)
meanAndVariance' distrib =
  (mean, var)
  where
    (m, m2) = foldr folder (0,0) distrib
    folder :: (Damage, Rational) -> (Rational, Rational) -> (Rational, Rational)
    folder (d,p) (m,m2) = (m + fromIntegral d * p, m2 + fromIntegral d ^ 2 * p)
    mean = fromRational m
    var = fromRational $ m2 - m ^ 2

baseDeck'' :: AttackModifierDeck
baseDeck'' = [
  ("r+2", 0),
  ("r+1", 0),
  ("r+0", 0),
  ( "*2", 1),
  ( "+4", 0),
  ( "+3", 0),
  ( "+2", 1),
  ( "+1", 5),
  ( "+0", 6),
  ( "-1", 5),
  ( "-2", 1),
  (  "0", 1)]

cragheartDeck0'' = [
  ( "*2", 1),
  ( "+2", 3),
  ( "+1", 8),
  ( "+0", 2),
  ( "-1", 2),
  ( "-2", 1),
  (  "0", 1)]

testDeck0'' = [
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

testDeck1'' = [
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

main :: IO ()
main = do
  -- printAllDeckStats baseDmg
  -- foo baseDmg
  -- printDeckStats baseDeck baseDmg
  -- print $ normal cragheartDeck0 baseDmg
  -- print $ advantage cragheartDeck0 baseDmg
  -- print $ disadvantage cragheartDeck0 baseDmg
  -- putChar '\n'
  print $ meanAndVariance' $ normalAttack' baseDeck'' baseDmg
  print $ meanAndVariance' $ attackWithAdvantage' baseDeck'' baseDmg
  print $ meanAndVariance' $ attackWithDisadvantage' baseDeck'' baseDmg
  putChar '\n'
  print $ meanAndVariance' $ normalAttack' testDeck0'' baseDmg
  print $ meanAndVariance' $ attackWithAdvantage' testDeck0'' baseDmg
  print $ meanAndVariance' $ attackWithDisadvantage' testDeck0'' baseDmg
  putChar '\n'
  print $ meanAndVariance' $ normalAttack' testDeck1'' baseDmg
  print $ meanAndVariance' $ attackWithAdvantage' testDeck1'' baseDmg
  print $ meanAndVariance' $ attackWithDisadvantage' testDeck1'' baseDmg
  where
    baseDmg = 3
