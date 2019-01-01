#!/usr/bin/env stack
-- stack --install-ghc runghc --package array-0.5.3.0

import qualified Data.Map.Strict as Map
import Data.List (intercalate,scanl,(!!),foldl',maximum)
import qualified Circle as Circle
import Debug.Trace

type Marble = Int
type Board = Circle.Circle

start :: Circle.Circle
start = Circle.singleton 0

data Round =
  Round {
   player :: Int,
   -- Current position will always be location 0 in the circle
   circle :: Board,
   score :: Map.Map Int Int
  }

-- start with [current 1 2 ...]
-- insert the marble [current 1 marble 2 ...]
-- make current the nely inserted [marble 2 ...] ++ [current 1]
insertMarble :: Board -> Marble -> Board
-- insertMarble _ m | trace ("insertMarble " ++ (show m) ) False = undefined
insertMarble c m = Circle.insert (Circle.next (Circle.next c)) m

prev7 = Circle.prev . Circle.prev . Circle.prev . Circle.prev . Circle.prev . Circle.prev . Circle.prev

removeMarble :: Board -> (Board, Marble)
removeMarble c = Circle.remove (prev7 c)
          
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
      result `seq` Round (nextPlayer numPlayers p) circle' score'
  | otherwise =
    let circle' = insertMarble circle m
        player' = (nextPlayer numPlayers p) in
      circle' `seq` player' `seq` Round (nextPlayer numPlayers p) circle' score
      
main :: IO ()

showBoard :: Board -> String
showBoard c = intercalate " " (map show (Circle.toList c))

showGame :: Round -> String
showGame (Round p circle score) = "[" ++ (show p) ++ "] " ++ (showBoard circle)

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
       putStrLn (showHighScore 470 72170) >>
       putStrLn (showHighScore 470 7217000)

