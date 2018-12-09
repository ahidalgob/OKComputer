module Graph where

import qualified Data.Map.Strict as Map

data Graph = Graph {graph_size::Int, graph_adj::(Map.Map Int [Int]), graph_revadj::(Map.Map Int [Int])}

emptyGraph n = Graph n Map.empty Map.empty

addEdge :: Int -> Int -> Graph -> Graph
addEdge v u (Graph n adj revadj) =
  let adj' = Map.adjust ((:)u) v adj
      revadj' = Map.adjust ((:)v) u revadj
   in Graph n adj' revadj'


graphFromEdges :: Int -> [(Int, Int)] -> Graph
graphFromEdges = undefined
