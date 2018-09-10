module Main where

import Gloomhaven.AttackModifierDeckCalculator
import Prelude hiding (Left, Right)
import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Text.Printf (printf)

data Alignment = Left | Right | Center

printKillChanceTable :: Deck -> [Damage] -> IO ()
printKillChanceTable deck baseDmgRange = do
  putStrLn $ text
  where
    distr :: DamageDistributionTable
    distr = buildDamageDistributionTable deck baseDmgRange

    dict :: KillChanceTable
    dict = buildKillChanceTable distr

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
    textTableWithHeaders = addHeader $ addStats $ addResultDmgColumn $ textTable

    text :: String
    text = intercalate "\n" $ map (intercalate " | ") $ textTableWithHeaders

    addHeader :: [[String]] -> [[String]]
    addHeader tbl = ("  " : map (alignCenter 24 . show) baseDmgRange) : tbl

    addStats :: [[String]] -> [[String]]
    addStats tbl = -- tbl
      (" E" : [printf "%6.2f / %6.2f / %6.2f" (expected (Disadvantage, baseDmg)) (expected (Normal, baseDmg)) (expected (Advantage, baseDmg)) | baseDmg <- baseDmgRange]) :
      (" s" {- σ -} : [printf "%6.2f / %6.2f / %6.2f" (sigma (Disadvantage, baseDmg)) (sigma (Normal, baseDmg)) (sigma (Advantage, baseDmg)) | baseDmg <- baseDmgRange]) :
      (" d" {- Δ -} : [printf "%+6.2f / %+6.2f / %+6.2f" (delta (Disadvantage, baseDmg)) (delta (Normal, baseDmg)) (delta (Advantage, baseDmg)) | baseDmg <- baseDmgRange]) :
      tbl
      where
        stats = buildMeanAndVarianceTable distr
        expected = fst . (stats Map.!)
        sigma = sqrt . snd . (stats Map.!)
        delta key@(Normal, baseDmg) = expected key - fromIntegral baseDmg
        delta key@(Advantage, baseDmg) = expected key - expected (Normal, baseDmg)
        delta key@(Disadvantage, baseDmg) = expected key - expected (Normal, baseDmg)

    addResultDmgColumn :: [[String]] -> [[String]]
    addResultDmgColumn = zipWith (:) (map (alignRight 2) $ map show resultDmgRange)

    showMaybeProbability :: Maybe Probability -> String
    showMaybeProbability (Just p) = showProbability p
    showMaybeProbability Nothing = "   -  "

    showProbability :: Probability -> String
    showProbability = printf "%6.2f" . toPercent

    toPercent :: Probability -> Float
    toPercent = fromRational . (*100)

    resultDmgRange :: [Damage]
    resultDmgRange = [1..max]
      where max = maximum $ fmap (fst . Map.findMax) $ dict

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

validateDeck :: Deck -> Deck
validateDeck deck
  | any (<0) deck = error "The deck contains a card with a negative count."
  | sum deck < 2  = error "The deck contains less than two cards."
  | otherwise     = filterZeroValues deck
  where
    filterZeroValues = Map.filter (/=0)

main :: IO ()
main = do

  contents <- ByteString.getContents
  let deck = validateDeck $ parseDeck contents

  printKillChanceTable deck baseDmgRange

  where
    baseDmg = 3
    baseDmgRange = [3]
