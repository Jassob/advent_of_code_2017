-- | This module takes care of the "plumbing" in my AoC code, that is mainly argument handling.

module Lib where

import System.Environment

-- * Types and classes

data Part = P1 | P2
  deriving (Eq, Show)

class Arg a where
  parseInput :: String -> Maybe a
  checkArgs :: a -> Bool

  -- Default implementation of checkArgs is to accept every value.
  checkArgs = const True

-- * Functions

run_ :: Arg a => (a -> Int) -> (a -> Int) -> String -> IO ()
run_ p1 p2 str = do
  res <- withFun p1 p2 <$> parseArgs
  case res of
    Just i  -> print i
    Nothing -> putStrLn str

withFun :: Arg a => (a -> Int) -> (a -> Int) -> Maybe (Part, a) -> Maybe Int
withFun _  _   Nothing          = Nothing
withFun f1 f2  (Just (part, arg)) | checkArgs arg = pure . choose part (f1, f2) $ arg
                                  | otherwise     = Nothing
  where choose :: Part -> (a, a) -> a
        choose P1 = fst
        choose P2 = snd

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
