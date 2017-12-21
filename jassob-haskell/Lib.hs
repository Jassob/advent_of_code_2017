-- | This module takes care of the "plumbing" in my AoC code, that is mainly argument handling.

module Lib where

import System.Environment

data Part = P1 | P2

class Arg a where
  parseInput :: String -> Maybe a

run_ :: Arg a => (a -> Int) -> (a -> Int) -> String -> IO ()
run_ p1 p2 str = do
  res <- withFun p1 p2 <$> parseArgs
  case res of
    Just i  -> print i
    Nothing -> putStrLn str

withFun :: Arg a => (a -> Int) -> (a -> Int) -> Maybe (Part, a) -> Maybe Int
withFun f1 _  (Just (P1, arg)) = (pure . f1) arg
withFun _  f2 (Just (P2, arg)) = (pure . f2) arg

parseArgs :: Arg a => IO (Maybe (Part, a))
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
