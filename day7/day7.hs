#!/usr/bin/env stack
-- stack --install-ghc runghc --package raw-strings-qq-1.1 --package split-0.2.3.3 --package containers-0.6.0.1 

{-# LANGUAGE QuasiQuotes #-}
import Text.RawString.QQ
import Data.List
import Data.Char
import Data.List.Split
import Data.Graph
import Data.Array
import Debug.Trace

input :: String
input2 = [r|Step A must be finished before step N can begin.
Step P must be finished before step R can begin.
Step O must be finished before step T can begin.
Step J must be finished before step U can begin.
Step M must be finished before step X can begin.
Step E must be finished before step X can begin.
Step N must be finished before step T can begin.
Step W must be finished before step G can begin.
Step Z must be finished before step D can begin.
Step F must be finished before step Q can begin.
Step U must be finished before step L can begin.
Step I must be finished before step X can begin.
Step X must be finished before step Y can begin.
Step D must be finished before step Y can begin.
Step S must be finished before step K can begin.
Step C must be finished before step G can begin.
Step K must be finished before step V can begin.
Step B must be finished before step R can begin.
Step Q must be finished before step L can begin.
Step T must be finished before step H can begin.
Step H must be finished before step G can begin.
Step V must be finished before step L can begin.
Step L must be finished before step R can begin.
Step G must be finished before step Y can begin.
Step R must be finished before step Y can begin.
Step G must be finished before step R can begin.
Step X must be finished before step V can begin.
Step V must be finished before step Y can begin.
Step Z must be finished before step U can begin.
Step U must be finished before step R can begin.
Step J must be finished before step Y can begin.
Step Z must be finished before step C can begin.
Step O must be finished before step L can begin.
Step C must be finished before step H can begin.
Step V must be finished before step G can begin.
Step F must be finished before step K can begin.
Step Q must be finished before step G can begin.
Step S must be finished before step Q can begin.
Step M must be finished before step G can begin.
Step T must be finished before step L can begin.
Step C must be finished before step Q can begin.
Step T must be finished before step V can begin.
Step W must be finished before step Z can begin.
Step C must be finished before step K can begin.
Step I must be finished before step C can begin.
Step X must be finished before step Q can begin.
Step F must be finished before step X can begin.
Step J must be finished before step S can begin.
Step I must be finished before step K can begin.
Step U must be finished before step Q can begin.
Step I must be finished before step Q can begin.
Step N must be finished before step H can begin.
Step A must be finished before step T can begin.
Step T must be finished before step G can begin.
Step D must be finished before step T can begin.
Step A must be finished before step X can begin.
Step D must be finished before step G can begin.
Step C must be finished before step T can begin.
Step W must be finished before step Q can begin.
Step W must be finished before step K can begin.
Step V must be finished before step R can begin.
Step H must be finished before step R can begin.
Step F must be finished before step H can begin.
Step F must be finished before step V can begin.
Step U must be finished before step T can begin.
Step K must be finished before step H can begin.
Step B must be finished before step T can begin.
Step H must be finished before step Y can begin.
Step J must be finished before step Z can begin.
Step B must be finished before step Y can begin.
Step I must be finished before step V can begin.
Step W must be finished before step V can begin.
Step Q must be finished before step R can begin.
Step I must be finished before step S can begin.
Step E must be finished before step H can begin.
Step J must be finished before step B can begin.
Step S must be finished before step G can begin.
Step E must be finished before step S can begin.
Step N must be finished before step I can begin.
Step Z must be finished before step F can begin.
Step E must be finished before step I can begin.
Step S must be finished before step B can begin.
Step D must be finished before step L can begin.
Step Q must be finished before step T can begin.
Step Q must be finished before step H can begin.
Step K must be finished before step Y can begin.
Step M must be finished before step U can begin.
Step U must be finished before step K can begin.
Step W must be finished before step I can begin.
Step J must be finished before step W can begin.
Step K must be finished before step T can begin.
Step P must be finished before step Y can begin.
Step L must be finished before step G can begin.
Step K must be finished before step B can begin.
Step I must be finished before step Y can begin.
Step U must be finished before step B can begin.
Step P must be finished before step O can begin.
Step O must be finished before step W can begin.
Step O must be finished before step J can begin.
Step A must be finished before step J can begin.
Step F must be finished before step G can begin.|]

input = [r|Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.|]

-- FIXME: this list doesn't contain the final step because it
-- has no outdegree!
parseDependency :: String -> (Char, Char)
parseDependency s = 
  let tok = splitOn " " s in
    (head (tok !! 1),
     head (tok !! 7))
        
dependencies = sort( map parseDependency (splitOn "\n" input) )
dependentEdges = groupBy (\x -> \y -> (fst x) == (fst y)) dependencies

toNode ((src,dst):rest) = ( src, src, [dst] ++ (map snd rest) )  
dependentAdj = map toNode dependentEdges
  
-- This is perhaps the least usable interface ever because given an edge
-- list it requires you to do all the work constructing each adjacency
-- list.  This is really "graphFromAdjacencyLists".
--
-- BuildG looks better but it requires you map everything to an Int first
-- because what are parameterized types anyway.
--
-- Also it doesn't complain if you specify an edge to a nonexistent
-- vertex, it just fails silently.
(g, nodeFromVertex, vertexFromKey) =
  graphFromEdges dependentAdj

-- Tried to use http://hackage.haskell.org/package/hit-graph-0.1/docs/Data-Graph-Inductive-Query-Topsort.html and entered dependency hell, so....
-- Graph.topSort doesn't let me control ties, so...
-- Reimplementing Kahn's algorithm it is, I guess, and the RosettaCode
-- version is completely unintelligble.

type IndegreeMap = Array Int Int

initIndegrees = indegree g
initQueue = map fst (filter (\x -> (snd x) == 0) (assocs initIndegrees))

nextNode :: [Vertex] -> Vertex
nextNode = minimumBy (\x -> \y -> (compare (nodeFromVertex x) (nodeFromVertex y)))

vertexLabel = (\ (n,_,_) -> n) . nodeFromVertex

myTopSort :: [Vertex] -> Array Vertex Int -> [Vertex] -> [Vertex]

myTopSort [] _ visited = visited
myTopSort queue indegrees visited
  | trace ( "Q" ++ (show queue) ++ " I" ++ (show indegrees) ++ " V" ++ (show visited )) False = undefined  
myTopSort queue indegrees visited =
  let n = nextNode queue in
    let out = g ! n in
      (myTopSort
        ( (delete n queue) ++ (filter (\nb -> (indegrees !  nb) == 1 ) out) )
        (accum (-) indegrees [ (o,1) | o <- out ] )
        (visited ++ [n])
      )

visit = myTopSort initQueue initIndegrees []

main :: IO ()
main = putStrLn (show dependentAdj) >>
       putStrLn (show g) >>
       putStrLn (show initQueue) >>
       putStrLn (show (map vertexLabel initQueue)) >>
       putStrLn (show initIndegrees) >>
       putStrLn (show (map vertexLabel visit))
       
       




