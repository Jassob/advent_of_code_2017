-- | Solution to second day in Advent of Code 2017 (adventofcode.com)

import System.Environment (getArgs)
import Data.Maybe (fromJust, catMaybes)
import Data.List (sortBy)

data Part = P1 | P2
  deriving (Eq, Show)

type PartFun = [Maybe Int] -> Maybe Int -> Maybe Int

main :: IO ()
main = do
   checkSum <- withArgs (go [(P1, calcChecksum), (P2, calcDivSum)]) <$> parseArgs
   case checkSum of
     Nothing -> printUsage
     Just c  -> print c

   where go :: [(Part, PartFun)] -> Part -> [[Maybe Int]] -> Maybe Int
         go fs part = foldr (fromJust . lookup part $ fs) (Just 0)

withArgs :: (Part -> [[Maybe Int]] -> Maybe Int) -> Maybe (Part, [[Maybe Int]]) -> Maybe Int
withArgs f arg = arg >>= uncurry f

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

parseInput :: String -> Maybe [[Maybe Int]]
parseInput = pure . filter (not . null) . map (map (pure . read) . words) . lines

-- | Part 1
calcChecksum :: [Maybe Int] -> Maybe Int -> Maybe Int
calcChecksum row prevSum = (+) <$> prevSum <*> pure checksum
  where mx = maximum (catMaybes row)
        mn = minimum (catMaybes row)
        checksum = mx - mn

-- | Part 2
calcDivSum :: [Maybe Int] -> Maybe Int -> Maybe Int
calcDivSum row prevSum = (+) <$> prevSum <*> (findDivisibles . sortBy (flip compare)) row
  where findDivisibles :: [Maybe Int] -> Maybe Int
        findDivisibles [] = Nothing
        findDivisibles (n:ns) =
          case filter (\d -> (mod <$> n <*> d) == Just 0) ns of
            []       -> findDivisibles ns
            (n':_) -> div <$> n <*> n'
