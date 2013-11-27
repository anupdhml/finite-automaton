-- By anupdhml
-- A module for creating and handling directed graphs

module Digraph (AdjList, Digraph, Edge,
                insertVertex, insertEdge, makeDigraph, getAdj, empty, vertexInGraph
               ) where

----------------------------------------
-- Types

-- In these type declarations, v is the vertex type and e is the edge label type
-- Note that v and e can be relatively arbitrary types (numbers, strings, etc.),
--   but the vertex type must be in class Eq so that vertices can be compared with (==)

-- A list of (source vertex, adjacency list) pairs
type Digraph v e = [(v, AdjList v e)]

-- A list of (destination vertex, edge label) pairs,
-- associated with a source vertex in the Digraph
type AdjList v e = [(v, e)]

-- A (source vertex, destination vertex, edge label) triple
type Edge v e    = (v, v, e)

----------------------------------------
-- Functions

-- Returns an empty digraph.
empty            :: Digraph v e
empty             = []

-- Inserts a vertex into a digraph, returning the new digraph.
-- You need not verify that the vertex is not already present.
insertVertex          :: (Eq v) => v -> Digraph v e -> Digraph v e
insertVertex v digraph = (v, []) : digraph

-- Inserts a new edge into a digraph, returning the new digraph.
-- Reports an error if the source vertex is not present in the digraph.
-- You need not verify that the destination vertex is present.
insertEdge               :: (Eq v) => Edge v e -> Digraph v e -> Digraph v e
insertEdge e@(sourcev, destv, label) (g@(v, adjlst) : rest)
           | sourcev == v = (v, ((destv, label):adjlst) ) : rest
           | otherwise    = g : insertEdge e rest
insertEdge _ []           = error "Source vertex not present in the graph."

-- Constructs a complete digraph from the list of vertices and the list of edges.
-- For example, g1 below has vertices 'a', 'b', and 'c',
--   an edge labeled 0 from 'a' to 'b', an edge labeled 1 from 'a' to 'c',
--   and an edge labeled 2 from 'c' to 'b'.
-- The order of elements in the lists does not matter.
makeDigraph            :: (Eq v) => [v] -> [Edge v e] -> Digraph v e
makeDigraph verts edges = let elessgraph = foldr insertVertex empty verts
                          in foldr insertEdge elessgraph edges

-- Returns the adjacency list for the given vertex.
-- Reports an error if the vertex is not in the digraph.
getAdj          :: (Eq v) => v -> Digraph v e -> AdjList v e
getAdj v digraph = case lookup v digraph of
                        Nothing -> error "Vertex not present in the graph."
                        (Just adjlist) -> adjlist

-- Returns True if the given vertex is in the given digraph, and False otherwise.
vertexInGraph    :: (Eq v) => v -> Digraph v e -> Bool
vertexInGraph v digraph = elem v (map fst digraph)

----------------------------------------
-- Tests
g1 = makeDigraph ['a', 'b', 'c'] [('a', 'b', 0), ('a', 'c', 1), ('c', 'b', 2)]
g2 = makeDigraph [1, 2, 3, 4] [(1, 3, 'a'), (4, 3, 'b'), (3, 4, 'a'), (2, 1, 'd')]
g3 = makeDigraph ['a', 'b', 'c'] [('a', 'b', 0), ('a', 'c', 1), ('c', 'b', 2), ('d', 'a', 3)]
