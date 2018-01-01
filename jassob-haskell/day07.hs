{-# LANGUAGE FlexibleInstances #-}
-- | Solution to seventh day of Advent of Code 2017 (adventofcode.com)

import           Data.Foldable (maximumBy, minimumBy)
import           Data.Function (on)
import           Data.List (sort, group)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (isNothing, fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S

import Lib (Part(..), Arg(..), run)

import Debug.Trace

data Tree a = Node a [Tree a]
  deriving (Eq, Show, Read)

headT :: Tree a -> a
headT (Node a _) = a

data Program = Prog { name :: String
                    , weight :: Int }
  deriving (Eq, Show, Read)

instance Arg (Tree Program) where
  parseInput = either (const Nothing) Just . uncurry3 buildTree . buildMaps . prepareString
    where uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
          uncurry3 f (a, b, c) = f a b c

main :: IO ()
main = do
  res <- run part1 part2 usage
  print res

part1 :: Tree Program -> String
part1 (Node p _) = name p

part2 :: Tree Program -> String
part2 (Node p subs) = case unbalancedSubtree (Node p subs) of
  (Just st@(Node p' _)) -> case unbalancedSubtree st of
    Just st' -> part2 st
    Nothing  -> show $ weight p' - diff
      where weights = group . sort . map totalWeight $ subs
            diff = (head . minimumBy (compare `on` length) $ weights) - (head . maximumBy (compare `on` length) $ weights)
  Nothing   -> error "Tree is balanced."

prepareString :: String -> [String]
prepareString = lines . filter (/=',')

buildMaps :: [String] -> (Map String [String], Map String (Maybe String), Map String Int)
buildMaps ls = go ls M.empty M.empty M.empty
  where go :: [String] -> Map String [String]
           -> Map String (Maybe String)
           -> Map String Int
           -> (Map String [String], Map String (Maybe String), Map String Int)
        go (l:ls) subs prevs weights = case words l of
          [name, weight]                -> go ls (M.insert name [] subs) (insertIfNotIn name Nothing prevs) (M.insert name (read weight) weights)
          (name : weight : "->" : rest) -> go ls (M.insert name rest subs) (addAllPrevs name rest prevs) (M.insert name (read weight) weights)
        go [] subs ps ws = (subs, ps, ws)

        addAllPrevs :: String -> [String] -> Map String (Maybe String) -> Map String (Maybe String)
        addAllPrevs name subs prevMap = foldr (M.alter (const . pure . pure $ name)) (insertIfNotIn name Nothing prevMap) subs

        insertIfNotIn :: Ord k => k -> a -> Map k a -> Map k a
        insertIfNotIn key value = M.alter (maybe (Just value) pure) key

buildTree :: Map String [String] -> Map String (Maybe String) -> Map String Int -> Either String (Tree Program)
buildTree subMap prevMap weightMap = do
  root <- findRoot prevMap weightMap
  buildTree' root

  where buildTree' :: Tree Program -> Either String (Tree Program)
        buildTree' node =
          maybe (error' . headT $ node) (buildNode node) (getSubs . headT $ node)

        error' :: Program -> a
        error' p = error $ "buildTree': Incomplete tree, node " ++ show p ++ " has no sub trees"

        getSubs :: Program -> Maybe [String]
        getSubs = flip M.lookup subMap . name

        buildNode :: Tree Program -> [String] -> Either String (Tree Program)
        buildNode (Node p _) subs = do
          progs <- zipWith Prog subs <$> mapM (getWeight weightMap) subs
          progTrees <- mapM (buildTree' . flip Node []) progs
          pure (Node p progTrees)

findRoot :: Map String (Maybe String) -> Map String Int -> Either String (Tree Program)
findRoot prevMap weightMap = case M.toList . M.filter isNothing $ prevMap of
  [(rootNode,_)] -> maybe (error noRootMsg) (pure . mkNode rootNode) (M.lookup rootNode weightMap)
  _              -> error manyRootsMsg

  where noRootMsg :: String
        noRootMsg = "findRoot: Root node is not associated with a weight."

        manyRootsMsg :: String
        manyRootsMsg = "findRoot: There are more than one tree."

        mkNode :: String -> Int -> Tree Program
        mkNode name = flip Node [] . Prog name


getWeight :: Map String Int -> String -> Either String Int
getWeight map prog = maybe error' pure (M.lookup prog map)
  where error' :: a
        error' = error $ "getWeight: Node " ++ show prog ++ " is not associated with a weight"

totalWeight :: Tree Program -> Int
totalWeight (Node p subs) = weight p + foldr ((+) . totalWeight) 0 subs

unbalancedSubtree :: Tree Program -> Maybe (Tree Program)
unbalancedSubtree (Node p subs)
  | all (== head subweights) subweights = Nothing
  | otherwise = case filter ((== head subweights) . snd) pairs of
      [(t, w)] -> pure t
      (_:_)    -> pure . fst . head . filter ((/= head subweights) . snd) $ pairs
  where subweights = map totalWeight subs
        pairs = zip subs subweights

usage :: String
usage = concat
  [ "Usage: day07 [OPTIONS] [input]", "\n\n"
  , "input is the problem input that contains an unordered list of", "\n"
  , "nodes and edges. Part 1 of day07 finds the root of the tree", "\n"
  , " created from the problem input"
  , "OPTIONS:", "\n"
  , "--part, -p", "\t", "1|2", "\t", "Select between part 1 or part 2"
  , "\n", "\t\t\t\t", "If part is not selected part 1 will be assumed,"
  , "\n\n"
  , "-f", "\t", "FILEPATH","\t", "Path to file containing the problem input."
  ]
