module Graph where

import qualified Data.Map.Strict as Map
import Data.Maybe

data Graph = Graph { graph_size::Int
                   , graph_adj::Map.Map Int [Int]
                   , graph_revadj::Map.Map Int [Int]
                   }

instance Show Graph where
  show (Graph n adj _) = concatMap getAdj [0..(n-1)]
    where getAdj v = show (v, fromMaybe [] $ Map.lookup v adj) ++ "\n"

emptyGraph n = Graph n Map.empty Map.empty

addEdge :: (Int, Int) -> Graph -> Graph
addEdge (v, u) (Graph n adj revadj) =
  let adj' = Map.adjust ((:)u) v (insertIfNotIn v adj)
      revadj' = Map.adjust ((:)v) u (insertIfNotIn u revadj)
   in Graph n adj' revadj'

insertIfNotIn :: Int -> Map.Map Int [Int] -> Map.Map Int [Int]
insertIfNotIn v g = case Map.lookup v g of
                      Nothing -> Map.insert v [] g
                      _ -> g


graphFromEdges :: Int -> [(Int, Int)] -> Graph
graphFromEdges n = foldl (flip addEdge) (emptyGraph n)

predecessors :: Graph -> Int -> [Int]
predecessors g v = fromMaybe [] $ Map.lookup v (graph_revadj g)

successor :: Graph -> Int -> [Int]
successor g v = fromMaybe [] $ Map.lookup v (graph_adj g)
