module Main where

import Gloomhaven.AttackModifierDeckCalculator
import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import qualified Options.Applicative as OptParse
import Options.Applicative (flag', option, strOption, helper, long, help,
                            value, metavar, eitherReader,
                            info, fullDesc, header, progDesc,
                            execParser, (<**>), (<|>))
import Data.Semigroup ((<>))
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec
import System.IO (Handle, IOMode(ReadMode), openFile, stdin)

data Alignment = LeftAlignment | RightAlignment | CenterAlignment

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
          LeftAlignment -> (0, padding)
          RightAlignment -> (padding, 0)
          CenterAlignment -> ((padding + 1) `quot` 2, padding `quot` 2)
        left = replicate leftPadding ' '
        right = replicate rightPadding ' '

    alignLeft :: Int -> String -> String
    alignLeft = alignText LeftAlignment

    alignRight :: Int -> String -> String
    alignRight = alignText RightAlignment

    alignCenter :: Int -> String -> String
    alignCenter = alignText CenterAlignment

data Args = Args
  { baseDamageRange :: [Damage]
  , deckInput :: Input }

data Input
  = FileInput FilePath
  | StdInput

arguments :: OptParse.ParserInfo Args
arguments = info (argParser <**> helper)
  ( fullDesc
  <> progDesc "Gloomhaven attack modifier deck calculator produces a kill\
              \ chance table and some statistical characteristics of a deck\
              \ based on attack base damage.")
  where
    argParser :: OptParse.Parser Args
    argParser = Args
      <$> option damageRangeParser
        (  long "base-damage-range"
        <> help "An attack base damage range may be specified as a single\
                \ number, or as a pair of numbers sepparated by '-'.\
                \ Default value is 3."
        <> metavar "BEGIN[-END]"
        <> value [3] )
      <*> deckInputParser

    deckInputParser :: OptParse.Parser Input
    deckInputParser = deckFileInputParser <|> deckStdInputParser

    deckFileInputParser :: OptParse.Parser Input
    deckFileInputParser = FileInput <$> strOption
      (  long "deck-file"
      <> metavar "FILEPATH"
      <> help "Read deck from file" )

    deckStdInputParser :: OptParse.Parser Input
    deckStdInputParser = flag' StdInput
      (  long "deck-stdin"
      <> help "Read deck from stdin" )

    damageRangeParser :: OptParse.ReadM [Damage]
    damageRangeParser = eitherReader $
      \s -> mapLeft show $ Parsec.parse integerRange s s
      where
        buildRange :: Integer -> Maybe Integer -> [Integer]
        buildRange first Nothing = [first]
        buildRange first (Just last) = [first..last]

        integerRange :: Parsec.Parser [Integer]
        integerRange = buildRange
          <$> integer
          <*> Parsec.optionMaybe (Parsec.char '-' *> integer)
          <* Parsec.eof

        integer :: Parsec.Parser Integer
        integer = read <$> Parsec.many1 Parsec.digit

        mapLeft :: (a -> b) -> Either a c -> Either b c
        mapLeft f (Left x) = Left (f x)
        mapLeft _ (Right x) = Right x

openInput :: Input -> IO Handle
openInput input =
  case input of
    FileInput path -> openFile path ReadMode
    StdInput -> return stdin

parseDeck :: ByteString -> Deck
parseDeck = fromMaybe (error "Can not parse deck.") . decode

validateDeck :: Deck -> Deck
validateDeck deck
  | any (<0) deck = error "The deck contains a card with a negative count."
  | sum deck < 2  = error "The deck contains less than two cards."
  | otherwise     = filterZeroValues deck
  where
    filterZeroValues = Map.filter (/=0)

main :: IO ()
main = do

  args <- execParser arguments

  handle <- openInput $ deckInput args
  contents <- ByteString.hGetContents handle
  let deck = validateDeck $ parseDeck contents
  let baseDmgRange = baseDamageRange args

  printKillChanceTable deck baseDmgRange
