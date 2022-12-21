module Lib (Trie, initTrie, traverseTrie, dfs, parDFS) where

import qualified Data.Map as M
import Control.Parallel.Strategies(runEval, parBuffer, rdeepseq)

data Trie = Node Int (M.Map Char Trie)

insert :: [Char] -> Int -> Trie -> Trie
insert [] count (Node n m) = Node (n + count) m 
insert (x:xs) count (Node n m) =
    case M.lookup x m of
        Nothing -> Node n (M.insert x (insert xs count (Node 0 M.empty)) m)
        Just child -> Node n (M.insert x (insert xs count child) m)

initTrie :: Foldable t => t ([Char], Int) -> Trie
initTrie counts = foldl (\t (w, c) -> insert w c t) (Node 0 M.empty) counts

traverseTrie :: [Char] -> Trie -> Trie
traverseTrie [] node = node
traverseTrie (x:xs) (Node _ m) = 
    case M.lookup x m of
        Nothing -> (Node 0 M.empty)
        Just child -> traverseTrie xs child

dfs :: Trie -> [Char] -> [([Char], Int)]
dfs (Node n m) suffix
  | n > 0 = foldl (\l (letter, child) -> l ++ (dfs child (suffix ++ [letter]))) [(suffix, n)] pairs
  | otherwise = foldl (\l (letter, child) -> l ++ (dfs child (suffix ++ [letter]))) [] pairs
  where pairs = M.toList m

parDFS :: Trie -> [Char] -> [([Char], Int)]
parDFS (Node n m) suffix
  | n > 0 = runEval ((parBuffer 100 rdeepseq) (foldl (\l (letter, child) -> l ++ (parDFS child (suffix ++ [letter]))) [(suffix, n)] pairs))
  | otherwise = runEval ((parBuffer 100 rdeepseq) (foldl (\l (letter, child) -> l ++ (parDFS child (suffix ++ [letter]))) [] pairs))
  where pairs = M.toList m