module BFS where

import qualified Data.Set as Set

bfsOn :: Ord b => (a -> b) -> (a -> [a]) -> a -> [a]
bfsOn rep next start = go Set.empty [start] []
  where
    go _ [] [] = []
    go seen [] ys = go seen (reverse ys) []
    go seen (x:xs) ys
      | rep x `Set.member` seen = go seen xs ys
      | otherwise = x : go (rep x `Set.insert` seen) xs (next x ++ ys)

bfs :: Ord a => (a -> [a]) -> a -> [a]
bfs = bfsOn id
