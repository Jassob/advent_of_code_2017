-- Solution to first day in Advent of Code 2017 (adventofcode.com)

import System.Environment (getArgs)
import Data.Char (digitToInt, isDigit)

data Part = P1 | P2

main :: IO ()
main = do
  args <- parseArgs
  case args of
    Just (P1, input) -> go (calcSum (+1)) input
    Just (P2, input) -> go (calcSum (part2 (length input))) input
    Nothing          -> printUsage

  where go :: (String -> Int) -> String -> IO ()
        go f arg | checkInput arg = print $ f arg
                 | otherwise      = printUsage

        part2 :: Int -> Int -> Int
        part2 len i = i + (len `div` 2)

-- | Looks at the argument to the program and returns Nothing if it's
-- not whats expected
parseArgs :: IO (Maybe (Part, String))
parseArgs = do
  args <- getArgs
  return $ case args of
    ["--part", part, input] -> pure (toPart part, input)
    ["-p"    , part, input] -> pure (toPart part, input)
    [input]                 -> pure (P1, input)
    _                       -> Nothing

  where toPart :: String -> Part
        toPart "1" = P1
        toPart "2" = P2
        toPart _   = error "parseArgs: not a valid part"

-- | Verifies that the input is valid
checkInput :: String -> Bool
checkInput = all isDigit

printUsage :: IO ()
printUsage = putStrLn $ concat
  [ "Usage: day01 [OPTIONS] $inputSequence", "\n", "\n"
  , "$inputSequence is the problem input that contains "
  , "a sequence of _only_ digits and day01 calculates the sum "
  , "of every digit that match the next digit in the sequence."
  , "\n", "\n"
  , "OPTIONS:", "\n"
  , "--part, -p", "\t", "1|2", "\t", "Select between part 1 or part 2"
  , "\n", "If part is not selected part 1 will be assumed."
  ]

-- | Calculate the sum of digit that that matches another digit in a
-- circular array.
calcSum :: (Int -> Int) -- ^ Next index
        -> String       -- ^ Digit sequence
        -> Int          -- ^ Result
calcSum indFun digits = sum $ map digitToInt $ go digits 0 (length digits - 1)
  where go :: String -> Int -> Int -> String
        go cs i end
          | i == end && curItem == nextItem = [curItem]
          | i == end = []
          | curItem == nextItem = curItem : go cs (i + 1) end
          | otherwise = go cs (i + 1) end

          where curItem = cs !! i
                nextI = indFun i
                nextItem = if nextI > end
                  then cs !! (nextI - end - 1)
                  else cs !! nextI
