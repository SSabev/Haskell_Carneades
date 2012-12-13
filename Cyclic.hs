module Cyclic(cyclic) where
import Data.Graph.Inductive

cyclic :: (DynGraph g) => g a b -> Bool
cyclic x | exists_leaf = cyclic remove_leafs
         | isEmpty x = False
         | otherwise = True
            where 
                remove_leafs = loop_through_nodes (nodes x) x
                exists_leaf = find_if_nodes_are_leaf (nodes x) x

find_if_nodes_are_leaf :: (DynGraph g) => [Node] -> g a b -> Bool
find_if_nodes_are_leaf [] graph = False
find_if_nodes_are_leaf (x:xs) graph = is_leaf x (edges graph) || find_if_nodes_are_leaf xs graph 

loop_through_nodes :: (DynGraph g) => [Node] -> g a b -> g a b
loop_through_nodes [] graph = graph
loop_through_nodes (x:xs) graph | is_leaf x (edges graph) = loop_through_nodes (xs) (delNode x graph)
                                | otherwise = loop_through_nodes xs graph

is_leaf :: Node -> [Edge] -> Bool
is_leaf n [] = True
is_leaf n (x:xs) | n /= fst(x) = is_leaf n xs
                 | otherwise   = False

--TESTS
testGraph :: Gr Int ()
testGraph = mkGraph (generateNodes 1 6) (doSomeEdges [(1,2),(2,3),(2,4),(4,5),(4,6),(6,3)])

generateNodes :: Enum a => a -> Int -> [LNode a]
generateNodes q i = take i (zip [1..] [q..])

doSomeEdges :: [Edge] -> [UEdge]
doSomeEdges = map (\(i,j) -> (i,j,()))
--Now fill up the graph.
