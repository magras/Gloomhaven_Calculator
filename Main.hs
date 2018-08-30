module Main where

import Gloomhaven.AttackModifierDeckCalculator
import Prelude hiding (Left, Right)
import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString, getContents)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Text.Printf (printf)

data Alignment = Left | Right | Center

baseDeck :: Deck
baseDeck = Map.fromList [
  ( "x2" , 1),
  ( "+2" , 1),
  ( "+1" , 5),
  ( "+0" , 6),
  ( "-1" , 5),
  ( "-2" , 1),
  (  "0" , 1)]

bruteDeck = Map.fromList [
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

tinkererDeck = Map.fromList [
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

spellweaverDeck = Map.fromList [
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

scoundrelDeck = Map.fromList [
  ("r+1" , 4),
  ("r+0*", 9),
  ( "x2" , 1),
  ( "+2" , 3),
  ( "+1" , 6),
  ( "+0" , 1),
  ( "-1" , 0),
  ( "-2" , 0),
  (  "0" , 1)]

cragheartDeck = Map.fromList [
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

mindthiefDeck = Map.fromList [
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

sunkeeperDeck = Map.fromList [
  ("r+1" , 4),
  ("r+0*", 15),
  ( "x2" , 1),
  ( "+2" , 2),
  ( "+1" , 7),
  ( "+0" , 2),
  ( "-1" , 1),
  ( "-2" , 0),
  (  "0" , 1)]

summonerDeck = Map.fromList [
  ("r+0*", 14),
  ( "x2" , 1),
  ( "+2" , 3),
  ( "+1" , 10),
  ( "+0" , 7),
  ( "-1" , 0),
  ( "-2" , 0),
  (  "0" , 1)]

removeRollingPlusZeroFromDeck :: Deck -> Deck
removeRollingPlusZeroFromDeck = Map.filterWithKey (\card _ -> card /= "r+0*")

printDeckStats :: Deck -> Damage -> IO ()
printDeckStats deck baseDmg = do
  printf "      normal: mean=%.2f sigma=%.2f delta=%+.2f\n" nrmMean (sqrt nrmVar) (nrmMean - fromIntegral baseDmg)
  printf "   advantage: mean=%.2f sigma=%.2f delta=%+.2f\n" advMean (sqrt advVar) (advMean - nrmMean)
  printf "disadvantage: mean=%.2f sigma=%.2f delta=%+.2f\n" disMean (sqrt disVar) (disMean - nrmMean)
  where
    (nrmMean, nrmVar) = meanAndVariance $ attack Normal deck baseDmg
    (advMean, advVar) = meanAndVariance $ attack Advantage deck baseDmg
    (disMean, disVar) = meanAndVariance $ attack Disadvantage deck baseDmg

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

cragheartDeck0 = Map.fromList [
  ( "x2" , 1),
  ( "+2*", 2),
  ( "+2" , 1),
  ( "+1*", 0),
  ( "+1" , 8),
  ( "+0" , 2),
  ( "-1" , 2),
  ( "-2" , 1),
  (  "0" , 1)]

cragheartDeck1 = Map.fromList [
  ( "x2" , 1),
  ( "+2*", 2),
  ( "+2" , 3),
  ( "+1*", 0),
  ( "+1" , 8),
  ( "+0" , 2),
  ( "-1" , 2),
  ( "-2" , 2),
  (  "0" , 1)]

cragheartDeck2 = Map.fromList [
  ( "x2" , 1),
  ( "+2*", 2),
  ( "+2" , 1),
  ( "+1*", 1),
  ( "+1" , 8),
  ( "+0" , 2),
  ( "-1" , 2),
  ( "-2" , 1),
  (  "0" , 1)]

cragheartDeck3 = Map.fromList [
  ( "x2" , 1),
  ( "+2*", 2),
  ( "+2" , 3),
  ( "+1*", 1),
  ( "+1" , 8),
  ( "+0" , 2),
  ( "-1" , 2),
  ( "-2" , 2),
  (  "0" , 1)]

cragheartDeck4 = Map.fromList [
  ( "x2" , 1),
  ( "+2*", 2),
  ( "+2" , 1),
  ( "+1*", 2),
  ( "+1" , 8),
  ( "+0" , 2),
  ( "-1" , 2),
  ( "-2" , 1),
  (  "0" , 1)]

cragheartDeck5 = Map.fromList [
  ( "x2" , 1),
  ( "+2*", 2),
  ( "+2" , 3),
  ( "+1*", 2),
  ( "+1" , 8),
  ( "+0" , 2),
  ( "-1" , 2),
  ( "-2" , 2),
  (  "0" , 1)]

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

printKillChanceTable :: Deck -> [Damage] -> IO ()
printKillChanceTable deck baseDmgRange = do
  putStrLn $ text
  where
    dict :: KillChanceTable
    dict = killChanceTable deck baseDmgRange

    table :: [[[Maybe Probability]]]
    table = [[[killChance dict atkType baseDmg resultDmg
      | atkType <- [Disadvantage, Normal, Advantage]]
      | baseDmg <- baseDmgRange]
      | resultDmg <- resultDmgRange]
    
    textTable :: [[String]]
    textTable = map (map formatter) table
      where
        formatter :: [Maybe Probability] -> String
        formatter = intercalate " / " . map showMaybeProbability

    textTableWithHeaders :: [[String]]
    textTableWithHeaders = addFirstColumn $ addHeader $ textTable

    text :: String
    text = intercalate "\n" $ map (intercalate " | ") $ textTableWithHeaders

    addHeader :: [[String]] -> [[String]]
    addHeader tbl = map (alignCenter 24 . show) baseDmgRange : tbl

    addFirstColumn :: [[String]] -> [[String]]
    addFirstColumn = zipWith (:) (map (alignRight 2) $ "" : map show resultDmgRange)

    showMaybeProbability :: Maybe Probability -> String
    showMaybeProbability (Just p) = showProbability p
    showMaybeProbability Nothing = "   -  "

    showProbability :: Probability -> String
    showProbability = printf "%6.2f" . toPercent

    toPercent :: Probability -> Float
    toPercent = fromRational . (*100)

    resultDmgRange :: [Damage]
    resultDmgRange = [1..max]
      where max = maximum $ Map.foldMapWithKey (\(_, _, d) _ -> [d]) dict

    alignText :: Alignment -> Int -> String -> String
    alignText algn width str = left ++ str ++ right
      where
        len = length str
        padding = width - len
        (leftPadding, rightPadding) = case algn of
          Left -> (0, padding)
          Right -> (padding, 0)
          Center -> ((padding + 1) `quot` 2, padding `quot` 2)
        left = replicate leftPadding ' '
        right = replicate rightPadding ' '

    alignLeft :: Int -> String -> String
    alignLeft = alignText Left

    alignRight :: Int -> String -> String
    alignRight = alignText Right

    alignCenter :: Int -> String -> String
    alignCenter = alignText Center

parseDeck :: ByteString -> Deck
parseDeck = fromJust . decode

main :: IO ()
main = do
  -- printAllDeckStats baseDmg
  -- printCragheartDeckVariantStats baseDmg
  -- printTestDeckStats baseDmg
  -- printPartyKillChanceTables baseDmgRange

  contents <- Data.ByteString.Lazy.getContents
  let deck = parseDeck contents

  printDeckStats deck baseDmg
  putChar '\n'
  printKillChanceTable deck baseDmgRange

  where
    baseDmg = 3
    baseDmgRange = [3]
