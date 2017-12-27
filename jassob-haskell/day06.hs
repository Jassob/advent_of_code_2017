{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
-- | Solution to sixth day in Advent of Code 2017 (adventofcode.com)

import           Data.Bool (bool)
import           Data.Maybe (maybe)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Vector (Vector, (!), (//), indexed, elemIndex)
import qualified Data.Vector as V

import Lib (Part(..), Arg(..), run_)

type Bank = Int
type Memory = Vector Bank

instance Arg Memory where
  parseInput = pure . V.fromList .  map read . words

main :: IO ()
main = run_ (part1 S.empty) (part2 M.empty) usage

part1 :: Set Memory -> Memory -> Int
part1 mems mem
  | mem `S.member` mems = S.size mems
  | otherwise = part1 (mem `S.insert` mems) (reallocate mem)

part2 :: Map Memory Int -> Memory -> Int
part2 mems mem =
  case mem `M.lookup` mems of
    Just i -> if i == 2
              then part2 mems' (reallocate mem)
              else findLoopLength 2 mems

    Nothing -> part2 (M.insert mem 1 mems) (reallocate mem)

  where findLoopLength :: Int -> Map Memory Int -> Int
        findLoopLength itrs = length . filter (==itrs) . M.elems

        mems' :: Map Memory Int
        mems' = M.alter (pure . maybe 0 (+1)) mem mems

reallocate :: Memory -> Memory
reallocate mem = updateMemory (mem // [(idx, 0)]) newBlocks extraBlocks
  where (idx, oldBlocks) = findNextBank mem

        newBlocks = oldBlocks `div` length mem

        extraBlocks = map (`mod` length mem)
                      [idx+1 .. idx + oldBlocks `mod` length mem]

updateMemory :: Memory -> Int -> [Int] -> Memory
updateMemory mem bpb extrb = foldr go mem [0..length mem - 1]
  where go :: Int -> Memory -> Memory
        go idx mem = mem // [(idx, mem ! idx + newBlocks idx)]

        newBlocks :: Int -> Int
        newBlocks = bool bpb (bpb + 1) . (`elem` extrb)

findNextBank :: Memory -> (Int, Bank)
findNextBank mem = maybe (error errmsg) (,max) $ elemIndex max mem
  where max = maximum mem
        errmsg = "findNextBank: " ++ show max ++
                 " is not present in the memory bank."
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
