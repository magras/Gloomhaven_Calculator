module Main where

import Gloomhaven.AttackModifierDeckCalculator
import Prelude hiding (Left, Right)
import Data.Aeson
import Data.ByteString.Lazy (ByteString, getContents)
import Data.List (nub, sort, intercalate)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)

data Alignment = Left | Right | Center

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

cragheartDeck0 = [
  ( "x2" , 1),
  ( "+2*", 2),
  ( "+2" , 1),
  ( "+1*", 0),
  ( "+1" , 8),
  ( "+0" , 2),
  ( "-1" , 2),
  ( "-2" , 1),
  (  "0" , 1)]

cragheartDeck1 = [
  ( "x2" , 1),
  ( "+2*", 2),
  ( "+2" , 3),
  ( "+1*", 0),
  ( "+1" , 8),
  ( "+0" , 2),
  ( "-1" , 2),
  ( "-2" , 2),
  (  "0" , 1)]

cragheartDeck2 = [
  ( "x2" , 1),
  ( "+2*", 2),
  ( "+2" , 1),
  ( "+1*", 1),
  ( "+1" , 8),
  ( "+0" , 2),
  ( "-1" , 2),
  ( "-2" , 1),
  (  "0" , 1)]

cragheartDeck3 = [
  ( "x2" , 1),
  ( "+2*", 2),
  ( "+2" , 3),
  ( "+1*", 1),
  ( "+1" , 8),
  ( "+0" , 2),
  ( "-1" , 2),
  ( "-2" , 2),
  (  "0" , 1)]

cragheartDeck4 = [
  ( "x2" , 1),
  ( "+2*", 2),
  ( "+2" , 1),
  ( "+1*", 2),
  ( "+1" , 8),
  ( "+0" , 2),
  ( "-1" , 2),
  ( "-2" , 1),
  (  "0" , 1)]

cragheartDeck5 = [
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

killChanceTable :: Deck -> [Damage] -> [(AttackType, Damage, Damage, Probability)]
killChanceTable deck baseDmgRange =
  concat [killChances deck baseDmg atkType | baseDmg <- baseDmgRange, atkType <- [Normal, Advantage, Disadvantage]]
  where
    killChances :: Deck -> Damage -> AttackType -> [(AttackType, Damage, Damage, Probability)]
    killChances deck baseDmg atkType =
      map (\(resultDmg, prob) -> (atkType, baseDmg, resultDmg, prob)) $
        tailDistribution $ (attack atkType) deck baseDmg

printKillChanceTable :: Deck -> [Damage] -> IO ()
printKillChanceTable deck baseDmgRange = do
  putStrLn $ text
  where
    dict :: [((AttackType, Damage, Damage), Probability)]
    dict =
      map (\(atk, bd, rd, p) -> ((atk,bd,rd),p)) $
        killChanceTable deck baseDmgRange

    killChance :: AttackType -> Damage -> Damage -> Maybe Probability
    killChance atkType baseDmg resultDmg =
      maximumMaybe $ map snd $
        filter (\((atk, bd, rd), _) -> atk == atkType && bd == baseDmg && rd >= resultDmg) dict

    table :: [[[Maybe Probability]]]
    table = [[[killChance atkType baseDmg resultDmg
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
    -- resultDmgRange = nub $ sort $ filter (/=0) $ map (\((_, _, resultDmg), _) -> resultDmg) dict
    resultDmgRange = let max = maximum $ map (\((_, _, resultDmg), _) -> resultDmg) dict in [1..max]

    maximumMaybe :: (Foldable t, Ord a) => t a -> Maybe a
    maximumMaybe xs
      | null xs = Nothing
      | otherwise = Just $ maximum xs

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
parseDeck str = case decode str of
  Just m -> Map.toList m
  Nothing -> error "Can't parse deck"

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
