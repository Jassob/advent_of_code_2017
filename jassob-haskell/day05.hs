{-# LANGUAGE FlexibleInstances #-}
-- | Solution to second day in Advent of Code 2017 (adventofcode.com)

import Prelude hiding (length)
import Data.Vector (Vector, (!), (//), fromList, length)

import Lib (Part(..), Arg(..), run_)

data PC = PC { instructions :: Vector Instruction
             , instrLength :: Int
             , pointer :: Pointer
             , done :: Bool }

newtype Instruction = Instr Int
newtype Pointer = Pointer { ptrIdx :: Int }

instance Arg PC where
  parseInput str = pure $ PC instrs (length instrs) (Pointer 0) False
    where instrs :: Vector Instruction
          instrs = fromList . map (Instr . read) . lines $ str

step :: (Int -> Int) -> PC -> Int
step _ (PC _ _ _ True) = 0
step f pc              = 1 + step f (step' f pc)

step' :: (Int -> Int) -> PC -> PC
step' incFun (PC instrs len (Pointer ptr) done) = PC instrs' len (Pointer ptrIdx') (ptrIdx' >= len)
  where (Instr curInstr) = instrs ! ptr
        instrs' = instrs // [(ptr, Instr . incFun $ curInstr)]
        ptrIdx' = ptr + curInstr

main :: IO ()
main = run_ (step part1) (step part2) usage
  where part1 :: Int -> Int
        part1 = (+1)

        part2 :: Int -> Int
        part2 i = if i >= 3 then i - 1 else i + 1

usage :: String
usage = concat
  [ "Usage: day05 [OPTIONS] [input]", "\n\n"
  , "input is the problem input that contains ", "\n"
  , "a newline separated list of jump instructions and day05", "\n"
  , "calculates how many jumps are needed to escape the \"jump maze\""
  , "\n\n"
  , "OPTIONS:", "\n"
  , "--part, -p", "\t", "1|2", "\t", "Select between part 1 or part 2"
  , "\n", "If part is not selected part 1 will be assumed,"
  , "\n\n"
  , "-f", "\t", "FILEPATH", "Path to file containing the problem input."
  ]
