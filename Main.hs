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
import Text.Tabular (Table(Table), Header(Header, Group), Properties(NoLine, SingleLine))
import qualified Text.Tabular.AsciiArt

data Alignment = LeftAlignment | RightAlignment | CenterAlignment

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

data Val = ValProb (Maybe Probability) | ValDmg Float | ValDiff Float
data Hdr = HdrDmg Damage | HdrStat Char
type Cell = [Val]
type Tbl = Table Hdr Damage Cell

buildTable :: Deck -> [Damage] -> Tbl
buildTable deck baseDmgRange = Table
  (Group SingleLine
    [ Group NoLine [Header $ HdrStat 'E', Header $ HdrStat 's', Header $ HdrStat 'd']
    , Group NoLine $ map (Header . HdrDmg) resultDmgRange
    ])
  (Group SingleLine $ map Header baseDmgRange)
  (
    [[ValDmg $ expected (atkType, baseDmg)
      | atkType <- [Disadvantage, Normal, Advantage]]
      | baseDmg <- baseDmgRange]
    :
    [[ValDmg $ sigma (atkType, baseDmg)
      | atkType <- [Disadvantage, Normal, Advantage]]
      | baseDmg <- baseDmgRange]
    :
    [[ValDiff $ delta (atkType, baseDmg)
      | atkType <- [Disadvantage, Normal, Advantage]]
      | baseDmg <- baseDmgRange]
    :
    [[[ValProb $ killChance killChanceTbl atkType baseDmg resultDmg
      | atkType <- [Disadvantage, Normal, Advantage]]
      | baseDmg <- baseDmgRange]
      | resultDmg <- resultDmgRange]
  )
  where
    expected = fst . (statsTbl Map.!)
    sigma = sqrt . snd . (statsTbl Map.!)
    delta key@(Normal, baseDmg) = expected key - fromIntegral baseDmg
    delta key@(_, baseDmg) = expected key - expected (Normal, baseDmg)

    distr :: DamageDistributionTable
    distr = buildDamageDistributionTable deck baseDmgRange

    statsTbl :: MeanAndVarianceTable
    statsTbl = buildMeanAndVarianceTable distr

    killChanceTbl :: KillChanceTable
    killChanceTbl = buildKillChanceTable distr

    resultDmgRange :: [Damage]
    resultDmgRange = [1..max]
      where max = maximum $ fmap (fst . Map.findMax) $ distr

printTable :: Deck -> [Damage] -> IO ()
printTable deck baseDmgRange = do
  putStr $ Text.Tabular.AsciiArt.render formatRowHdr formatColHdr formatCell table
  where
    formatRowHdr :: Hdr -> String
    formatRowHdr (HdrDmg dmg) = show dmg
    formatRowHdr (HdrStat ch) = [ch]

    formatColHdr :: Damage -> String
    formatColHdr = alignCenter 24 . show

    formatCell :: Cell -> String
    formatCell = intercalate " / " . map formatVal

    formatVal :: Val -> String
    formatVal (ValProb (Just p)) = printf "%6.2f" $ toFloat (100 * p)
    formatVal (ValProb Nothing) = "   -  "
    formatVal (ValDmg dmg) = printf "%6.2f" dmg
    formatVal (ValDiff delta) = printf "%+6.2f" delta

    toFloat :: Rational -> Float
    toFloat = fromRational

    table :: Tbl
    table = buildTable deck baseDmgRange

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

  printTable deck baseDmgRange
