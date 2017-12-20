-- | Solution to fourth day in Advent of Code 2017 (adventofcode.com)

import System.Environment (getArgs)
import Data.List (nub)

data Part = P1 | P2

main :: IO ()
main = do
  arg <- parseArgs
  case arg of
    Just (P1, strs) -> print $ length . filter (==True) . map passphraseValid $ strs
    Just (P2, strs) -> undefined
    Nothing         -> printUsage

parseArgs :: IO (Maybe (Part, [[String]]))
parseArgs = do
  args <- getArgs
  case args of
    [input]                  -> pure $ (,) <$> pure P1 <*> parseInput input
    ["-p", part, input]      -> pure $ (,) <$> toPart part <*> parseInput input
    ["-p", part, "-f", file] -> readFile file >>= \c -> pure $ (,) <$> toPart part <*> parseInput c
    ["--part", part, input]  -> pure $ (,) <$> toPart part <*> parseInput input
    _                        -> pure Nothing

  where toPart :: String -> Maybe Part
        toPart "1" = pure P1
        toPart "2" = pure P2
        toPart _   = Nothing

parseInput :: String -> Maybe [[String]]
parseInput = pure . map words . lines

printUsage :: IO ()
printUsage = putStrLn $ concat
  [ "Usage: day03 [input]", "\n\n"
  , "input is the problem input that contains the wanted ", "\n"
  , "location and day03 calculates the number of steps ", "\n"
  , "data is carried from a given location to the location 1."
  , "\n\n"
  , "OPTIONS:", "\n"
  , "--part, -p", "\t", "1|2", "\t", "Select between part 1 or part 2"
  , "\n", "\t\t\t\t", "If part is not selected part 1 will be assumed,"
  , "\n\n"
  , "-f", "\t", "FILEPATH", "\t", "Path to file containing the problem input."
  ]

passphraseValid :: [String] -> Bool
passphraseValid words = (length . nub) words == length words
