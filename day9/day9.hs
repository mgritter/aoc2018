#!/usr/bin/env stack
-- stack --install-ghc runghc --package array-0.5.3.0

import qualified Data.Map.Strict as Map
import Data.List (intercalate,scanl,(!!),foldl',maximum)

type Marble = Int
type Circle = [Int]

start :: Circle
start = [0]

data Round =
  Round {
   player :: Int,
   -- Current position will always be location 0 in the circle
   circle :: Circle,
   score :: Map.Map Int Int
  } deriving (Show)

-- start with [current 1 2 ...]
-- insert the marble [current 1 marble 2 ...]
-- make current the nely inserted [marble 2 ...] ++ [current 1]
insertMarble :: Circle -> Marble -> Circle
insertMarble (c:c1:ms) m =
  (m:ms) ++ [c, c1]
insertMarble (c:[]) m = [m, c]

-- start with [current ... m8 m7 m6 m5 m4 m3 m2 m1]
-- remove m7 and make m6 current
-- [m6 .. m1] ++ [current ... m8] 
removeMarble :: Circle -> (Circle, Marble)
removeMarble a =
  let len = length a
      parts = splitAt (len - 7) a in
    ( (tail (snd parts)) ++ (fst parts),
      (head (snd parts)) )
          
nextPlayer :: Int -> Int -> Int
nextPlayer numPlayers x = if x == numPlayers then 1 else x+1

increaseScore :: Int -> Maybe Int -> Maybe Int
increaseScore x (Just y) = Just (x+y)
increaseScore x Nothing = Just x

nextRound :: Int -> Round -> Marble -> Round
nextRound numPlayers (Round p circle score) m
  | m `mod` 23 == 0 =
    let result = removeMarble circle
        removed = snd result
        circle' = fst result        
        points = removed + m
        score' = Map.alter (increaseScore points) p score in
      Round (nextPlayer numPlayers p) circle' score'
  | otherwise =
    let circle' = insertMarble circle m in
      Round (nextPlayer numPlayers p) circle' score
      
main :: IO ()

showCircle :: Circle -> String
showCircle c = intercalate " " (map show c)

showGame :: Round -> String
showGame (Round p circle score) = "[" ++ (show p) ++ "] " ++ (showCircle circle)

round0 = Round 1 start Map.empty

playRounds :: Int -> Int -> Round
playRounds numPlayers lastMarble = foldl' (nextRound numPlayers) round0 [1..lastMarble]

playHistory :: Int -> Int -> [Round]
playHistory numPlayers lastMarble = scanl (nextRound numPlayers) round0 [1..lastMarble]

showHistory :: Int -> Int -> String
showHistory np lm = intercalate "\n" (map showGame (playHistory np lm))

highScore :: Round -> Int
highScore r = maximum (Map.elems (score r))

showHighScore np lm = "numPlayers=" ++ (show np) ++ " lastMarble=" ++ (show lm) ++ " highScore=" ++ (show (highScore (playRounds np lm)))
                                            
main = putStrLn (showHistory 9 25) >>
       putStrLn (showHighScore 10 1618) >>
       putStrLn (showHighScore 13 7999) >>
       putStrLn (showHighScore 17 1104) >>
       putStrLn (showHighScore 21 6111) >>
       putStrLn (showHighScore 30 5807) >>
       putStrLn (showHighScore 470 72170)
