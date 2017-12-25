{-# LANGUAGE FlexibleInstances #-}
-- | Solution to sixth day in Advent of Code 2017 (adventofcode.com)

import           Data.Function (on)
import           Data.Foldable (maximumBy)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Vector (Vector, (!), (//), indexed)
import qualified Data.Vector as V

import Lib (Part(..), Arg(..), run_)

type Bank = Int
type Memory = Vector Bank

instance Arg Memory where
  parseInput = pure . V.fromList .  map read . words

main :: IO ()
main = run_ (part1 S.empty) undefined usage

part1 :: Set Memory -> Memory -> Int
part1 mems mem
  | mem `S.member` mems = S.size mems
  | otherwise = part1 (mem `S.insert` mems) (reallocate mem)

reallocate :: Memory -> Memory
reallocate mem = updateMemory mem' 0 blocksPerBank extraBlockIdxs
  where (mem', (idx, blocks)) = findNextBank mem
        blocksPerBank = blocks `div` length mem
        extraBlockIdxs = map (`mod` length mem) [idx+1 .. idx + blocks `mod` length mem]

        updateMemory :: Memory -> Int -> Int -> [Int] -> Memory
        updateMemory mem idx bpb extrb
          | idx >= length mem = mem
          | otherwise = updateMemory mem (idx + 1) bpb extrb
            // [(idx, mem ! idx + blocks' idx bpb extrb)]

        blocks' :: Int -> Int -> [Int] -> Int
        blocks' idx blocks extrb | idx `elem` extrb = blocks + 1
                                 | otherwise = blocks

findNextBank :: Memory -> (Memory, (Int, Bank))
findNextBank mem = (mem // [(idx, 0)], (idx, blocks))
  where (idx, blocks) = V.head
                        . V.filter ((== maximum mem) . snd)
                        . indexed $ mem

usage :: String
usage = concat
  [ "Usage: day06 [OPTIONS] [input]", "\n\n"
  , "input is the problem input that contains a whitespace separated", "\n"
  , "list of numbers representing the number of blocks contained in", "\n"
  , "every memory bank. day06 calculates how many reallocation cycles", "\n"
  , "are needed to reach a configuration that has already been seen.", "\n\n"
  , "OPTIONS:", "\n"
  , "--part, -p", "\t", "1|2", "\t", "Select between part 1 or part 2"
  , "\n", "\t\t\t\t", "If part is not selected part 1 will be assumed,"
  , "\n\n"
  , "-f", "\t", "FILEPATH","\t", "Path to file containing the problem input."
  ]
