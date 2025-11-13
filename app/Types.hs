{-# LANGUAGE GADTs #-}
module Types (getArgs, input, output, ratio, fullDelay)

where
import           Options.Applicative      (Parser, ReadM, auto, eitherReader,
                                           execParser, fullDesc, header, help,
                                           helper, info, long, metavar, option,
                                           progDesc, short, strOption, value,
                                           (<**>))
import           Streamly.FileSystem.Path (Path, fromString_)
import           Text.Read                (readMaybe)


data Args where
  Args :: {
    input :: Path,
    output :: Path,
    fromFps :: Rational,
    toFps :: Rational,
    delay :: Delay
             } -> Args

data Delay where
  Monotonous :: Int -> Delay
  Batch :: [(Int, Int)] -> Delay

parseArgs :: Parser Args
parseArgs = Args <$> parseInput <*> parseOutput <*> parseFromFps
  <*> parseToFps <*> parseDelay

getArgs :: IO Args
getArgs = execParser $ info (parseArgs <**> helper) (
  fullDesc <> progDesc "Manipulate PGS files"
  <> header "hpgs - A program to manipulate PGS subtitles"
                                                    )

parseInput :: Parser Path
parseInput = fromString_ <$> strOption (
  long "input" <> short 'i' <> metavar "INPUT"
    <> help "Path to the input file"
                       )
parseOutput :: Parser Path
parseOutput = fromString_ <$> strOption (
  long "output" <> short 'o' <> metavar "OUTPUT"
    <> help "Path to the output file"
                       )

parseFromFps :: Parser Rational
parseFromFps = option auto (
  long "--from-fps" <> short 'f' <> metavar "FROMFPS"
    <> value 24
    <> help "fps of the input file, e.g. 24 or 24000 % 1001"
                           )
parseToFps :: Parser Rational
parseToFps = option auto (
  long "--to-fps" <> short 't' <> metavar "TOFPS"
    <> value 24
    <> help "fps of the output file, e.g. 24 or 24000 % 1001"
                           )

parseDelay :: Parser Delay
parseDelay = option readDelay (
  long "delay" <> short 'd' <> metavar "DELAY"
  <> value (Monotonous 0)
  <> help "delay(s) to apply in ms, either as a single int or as a list of int tuples where the first int is the number pf subtitles"
                                )

readDelay :: ReadM Delay
readDelay =  eitherReader $ \s ->
  case readMaybe s :: Maybe Int of
    Just x -> Right . Monotonous $ x
    Nothing -> case readMaybe s :: Maybe [(Int, Int)] of
                 Just xs -> Right . Batch $ xs
                 Nothing -> Left $ "Cannot parse delay " ++ s

fullDelay :: Int -> Args -> Either String [(Int, Int)]
fullDelay tot a = case delay a of
                       Monotonous x -> Right [(tot, x)]
                       Batch xs     -> check xs
  where
    check xs = case compare tot $ sum . map fst $ xs of
                 EQ -> Right xs
                 GT -> Left $ "more subtitles (" ++ show tot
                  ++ ") than delays "
                  ++ show (sum . map fst $ xs)
                 LT -> Left $ "less subtitles (" ++ show tot
                  ++ ") than delays "
                  ++ show (sum . map fst $ xs)

ratio :: Args -> Rational
ratio a = fromFps a / toFps a
