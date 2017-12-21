{-# LANGUAGE FlexibleInstances #-}
-- | Solution to second day in Advent of Code 2017 (adventofcode.com)

import System.Environment (getArgs)
import Data.Maybe (fromJust, catMaybes)
import Data.List (sortBy)

import Lib (Part(..), Arg(..), run_)

instance Arg [[Maybe Int]] where
  parseInput = pure . filter (not . null) . map (map (pure . read) . words) . lines

type PartFun = [Maybe Int] -> Maybe Int -> Maybe Int

main :: IO ()
main = run_ (mkRunFun calcChecksum) (mkRunFun calcDivSum) usage

mkRunFun :: ([Maybe Int] -> Maybe Int -> Maybe Int) -> ([[Maybe Int]] -> Int)
mkRunFun f = fromJust . foldr f (Just 0)

usage = concat
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
