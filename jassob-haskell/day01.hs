-- Solution to first day in Advent of Code 2017 (adventofcode.com)

import System.Environment (getArgs)
import Data.Maybe (maybe)
import Data.Char (digitToInt, isDigit)
import Data.List (group, nub)

main :: IO ()
main = do
  args <- parseArgs
  maybe printUsage go args

  where go :: String -> IO ()
        go arg | checkInput arg = print $ calcSum arg
               | otherwise      = printUsage

-- | Looks at the argument to the program and returns Nothing if it's
-- not whats expected
parseArgs :: IO (Maybe String)
parseArgs = do
  args <- getArgs
  return $ case args of
    [input] -> pure input
    _       -> Nothing

-- | Verifies that the input is valid
checkInput :: String -> Bool
checkInput = all isDigit

printUsage :: IO ()
printUsage = putStrLn $ concat
  [ "Usage: day01 $inputSequence", "\n", "\n"
  , "$inputSequence is the problem input that contains "
  , "a sequence of _only_ digits and day01 calculates the sum "
  , "of every digit that match the next digit in the sequence."
  ]

-- | Calculates the sum of all digits in the list that match their
-- next digit.
calcSum :: String -> Int
calcSum (c:cs) = sum $ map digitToInt $ go (c:cs) c
  where go :: String -> Char -> String
        go (a:b:cs) fst | a == b    = b : go (b:cs) fst
                        | otherwise = go (b:cs) fst
        go [a]      fst | a == fst  = [a]
                        | otherwise = []
        go []       _               = []

todo = undefined
