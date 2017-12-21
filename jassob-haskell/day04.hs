{-# LANGUAGE FlexibleInstances #-}
-- | Solution to fourth day in Advent of Code 2017 (adventofcode.com)

import System.Environment (getArgs)
import Data.List (nub)

import Lib (Part(..), Arg(..), run_)

-- Some abuse of the type classes
instance Arg [[String]] where
  parseInput = pure . map words . lines

main :: IO ()
main = run_ (mkEvalFun passphraseValid) (mkEvalFun passphraseValid') usage

mkEvalFun :: ([String] -> Bool) -> ([[String]] -> Int)
mkEvalFun f = length . filter (==True) . map f

usage :: String
usage = concat
  [ "Usage: day04 [input]", "\n\n"
  , "input is the problem input that contains a list of ", "\n"
  , "passphrases and day04 calculates how many of the passphrases ", "\n"
  , "that are valid, given the policies defined in the different parts."
  , "\n\n"
  , "OPTIONS:", "\n"
  , "--part, -p", "\t", "1|2", "\t", "Select between part 1 or part 2"
  , "\n", "\t\t\t\t", "If part is not selected part 1 will be assumed,"
  , "\n\n"
  , "-f", "\t", "FILEPATH", "\t", "Path to file containing the problem input."
  ]

passphraseValid :: [String] -> Bool
passphraseValid words = (length . nub) words == length words

passphraseValid' :: [String] -> Bool
passphraseValid' (w:ws) = all (not . isAnagramOf w) ws && passphraseValid' ws
passphraseValid' [] = True

-- | s1 is an anagram of s2 if s1 is a subset of s2 and s2 a subset of s1
subsetOf :: Eq a => [a] -> [a] -> Bool
subsetOf s1 s2 = all (`elem` s2) s1 && all (`elem` s1) s2

isAnagramOf = subsetOf
