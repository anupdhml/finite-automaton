-- By anupdhml 
-- Finite Automata implementation based on the digraph module

module FA where

import Digraph

----------------------------------------
-- Types

-- A labled digraph, a start state, and a list of final states
data Automaton a b = FA {graph :: Digraph a b, start :: a, final :: [a]}
     deriving Show


----------------------------------------
-- Functions

-- Creates an automaton, given list of states, list of edges, initial state,
--   and list of final states
makeAutomaton :: (Eq a, Eq b) => [a] -> [Edge a b] -> a -> [a] -> Automaton a b
makeAutomaton states edges istate fstates = FA {graph = makeDigraph states edges,
                                                start = istate, final = fstates}

-- Helps to step the FA
faHelper                   :: (Eq a, Eq b) => Automaton a b -> b -> a -> [a]
faHelper fa input currstate = let adjl  = getAdj currstate (graph fa)
                                  fadjl = filter (\(state,label) -> label == input) adjl
                               in map fst fadjl

-- Performs one transition in a DFA; given a current state and input symbol, returns the new state.
-- If there is no such current state, reports an error. (handled in the digraph module)
-- If zero or more than one state is reachable, reports an error.
stepDFA                   :: (Eq a, Eq b) => Automaton a b -> a -> b -> a
stepDFA fa currstate input = let newstate = faHelper fa input currstate
                             in case (length newstate == 1) of
                                     True -> head newstate
                                     False -> error "Not a DFA"

-- Simulates a run of a DFA with a list of labels as input.
-- Returns whether or not the input is accepted.
simulateDFA          :: (Eq a, Eq b) => Automaton a b -> [b] -> Bool
simulateDFA fa labels = let endstate = foldl (stepDFA fa) (start fa) labels
                        in endstate `elem` final fa

-- Returns all new states adjacent to states in a given list via transitions for a given label.
stepNFA                    :: (Eq a, Eq b) => Automaton a b -> [a] -> b -> [a]
stepNFA fa currstates input = dupRemove $ concat $ map (faHelper fa input) currstates
    where dupRemove :: (Eq a) => [a] -> [a]
          dupRemove (x:xs) | x `elem` xs = dupRemove xs
                           | otherwise   = x : dupRemove xs
          dupRemove []                   = []

-- Simulates a run of an NFA with a list of labels as input.
-- Returns whether or not the input is accepted.
simulateNFA          :: (Eq a, Eq b) => Automaton a b -> [b] -> Bool
simulateNFA fa labels = let endstates = foldl (stepNFA fa) [start fa] labels
                        in endstates `hasCommon` final fa
    where hasCommon :: (Eq a) => [a] -> [a] -> Bool
          hasCommon (x:xs) ys | x `elem` ys = True
                              | otherwise   = hasCommon xs ys
          hasCommon [] _                    = False

----------------------------------------
-- Tests

-- Accepts sequences of 0's and 1's representing binary numbers that
-- are divisible by 3.  (Draw it out and convince yourself by stepping
-- through it with a few numbers expressed in binary.)
dfa1 = makeAutomaton ['a', 'b', 'c', 'd']
        [('a', 'a', 0), ('a', 'b', 1), ('b', 'a', 1), ('b', 'c', 0), ('c', 'b', 0),
         ('c', 'c', 1), ('d', 'a', 0), ('d', 'b', 1)]
        'd'
        ['a']
dfa2 = makeAutomaton ['a', 'b', 'c']
        [('a', 'b', 0), ('a', 'a', 1), ('b', 'b', 0), ('b', 'b', 1), ('c', 'c', 1), ('c', 'a', 0)]
        'c'
        ['a', 'b']
-- dfa2 = makeAutomaton ['a', 'b', 'c']
        -- [('a', 'b', 1), ('a', 'c', 0), ('b', 'b', 0), ('b', 'c', 1), ('c', 'b', 1)]
        -- 'a'
        -- ['c']

-- Second NFA example from the problem set write-up -- two consecutive 0's or 1's or both.
nfa1 = makeAutomaton ['a', 'b', 'c', 'd', 'e']
         [('a', 'a', 0), ('a', 'a', 1), ('a', 'b', 1), ('a', 'c', 0), ('b', 'd', 1),
          ('c', 'e', 0), ('d', 'd', 0), ('d', 'd', 1), ('e', 'e', 0), ('e', 'e', 1)]
         'a'
         ['d', 'e']
nfa2 = makeAutomaton ['a', 'b', 'c']
        [('a', 'b', 0), ('a', 'c', 1), ('b', 'c', 0), ('b', 'a', 1),('c', 'b', 0)]
        'a'
        ['b', 'c']
