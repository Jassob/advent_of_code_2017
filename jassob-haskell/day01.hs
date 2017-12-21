{-# LANGUAGE FlexibleInstances#-}
-- Solution to first day in Advent of Code 2017 (adventofcode.com)

import System.Environment (getArgs)
import Data.Char (digitToInt, isDigit)

import Lib (Arg(..), Part(..), run_)

instance Arg String where
  parseInput = pure . filter (/='\n')
  checkArgs  = all isDigit

main = run_ (calcSum (+1)) calcSum' usage
    where part2 :: Int -> Int -> Int
          part2 len i = i + (len `div` 2)

          calcSum' :: String -> Int
          calcSum' ds = calcSum (part2 (length ds)) ds

usage :: String
usage = concat
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
