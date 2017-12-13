-- | Solution to second day in Advent of Code 2017 (adventofcode.com)

import System.Environment (getArgs)
import Data.Maybe (catMaybes)

data Part = P1 | P2

main :: IO ()
main = do
   checkSum <- withArgs go <$> parseArgs
   case checkSum of
     Nothing       -> printUsage
     Just c -> print c

   where go :: Part -> [[Maybe Int]] -> Maybe Int
         go P1 grid = calcChecksum grid
         go P2 grid = undefined

parseArgs :: IO (Maybe (Part, [[Maybe Int]]))
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

printUsage :: IO ()
printUsage = putStrLn $ concat
  [ "Usage: day02 [OPTIONS] [input]", "\n\n"
  , "input is the problem input that contains "
  , "a grid of _only_ digits and day02 calculates the checksum "
  , "for the grid by summing the greatest differences between "
  , "the numbers in the grid."
  , "\n\n"
  , "OPTIONS:", "\n"
  , "--part, -p", "\t", "1|2", "\t", "Select between part 1 or part 2"
  , "\n", "If part is not selected part 1 will be assumed,"
  , "\n\n"
  , "-f", "\t", "FILEPATH", "Path to file containing the problem input."
  ]


withArgs :: (Part -> [[Maybe Int]] -> Maybe Int) -> Maybe (Part, [[Maybe Int]]) -> Maybe Int
withArgs f arg = arg >>= \(p, grid) -> f p grid

parseInput :: String -> Maybe [[Maybe Int]]
parseInput = pure . filter (not . null) . map ((map (pure . read)) . words) . lines

calcChecksum :: [[Maybe Int]] -> Maybe Int
calcChecksum = foldr go (Just 0)
  where go :: [Maybe Int] -> Maybe Int -> Maybe Int
        go row prevSum = (+) <$> prevSum <*> pure checksum
          where mx = maximum (catMaybes row)
                mn = minimum (catMaybes row)
                checksum = mx - mn
