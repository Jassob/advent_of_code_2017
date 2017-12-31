{-# LANGUAGE FlexibleInstances #-}
-- | Solution to seventh day of Advent of Code 2017 (adventofcode.com)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (isNothing, fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S

import Lib (Part(..), Arg(..), run)

import Debug.Trace

data Tree a = Node a [Tree a] | Leaf a
  deriving (Eq, Show, Read)

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
part2 = undefined

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
  buildTree' subMap weightMap root

  where buildTree' :: Map String [String] -> Map String Int -> Tree Program -> Either String (Tree Program)
        buildTree' subMap weightMap (Node p _) = flip (maybe (error $ "buildTree': Incomplete tree, node " ++ show p ++ " has no sub trees"))
                                                 (M.lookup (name p) subMap) $ \subs -> do
          progs <- zipWith Prog subs <$> mapM (getWeight weightMap) subs
          progTrees <- mapM (\p -> buildTree' subMap weightMap (Node p [])) progs
          pure (Node p progTrees)

        findRoot :: Map String (Maybe String) -> Map String Int -> Either String (Tree Program)
        findRoot prevMap weightMap = do
          rootProgram <- case M.toList . M.filter isNothing $ prevMap of
            [(rootNode,_)] -> maybe (error "findRoot: Root node is not associated with a weight.")
                                    (Right . Prog rootNode) (M.lookup rootNode weightMap)
            _              -> error "findRoot: There are more than one tree."
          pure $ Node rootProgram []

getWeight :: Map String Int -> String -> Either String Int
getWeight map prog = maybe (error $ "getWeight: Node " ++ show prog
                                    ++ " is not associated with a weight")
                            pure (M.lookup prog map)
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

exampleProgramTree :: String
exampleProgramTree = "pbga (66)\n\
                     \xhth (57)\n\
                     \ebii (61)\n\
                     \havc (66)\n\
                     \ktlj (57)\n\
                     \fwft (72) -> ktlj, cntj, xhth\n\
                     \qoyq (66)\n\
                     \padx (45) -> pbga, havc, qoyq\n\
                     \tknk (41) -> ugml, padx, fwft\n\
                     \jptl (61)\n\
                     \ugml (68) -> gyxo, ebii, jptl\n\
                     \gyxo (61)\n\
                     \cntj (57)"