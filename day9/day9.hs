#!/usr/bin/env stack
-- stack --install-ghc runghc --package array-0.5.3.0

--import Data.Array.IArray (Array,array,(!),ixmap,(//),bounds,elems)
import Data.Array.Unboxed (UArray,array,(!),ixmap,(//),bounds,elems)
import qualified Data.Map.Strict as Map
import Data.List (intercalate,scanl,(!!),foldl',maximum)

start :: Circle
start = array (0,0) [(0,0)]
--numPlayers = 470
--lastMarble = 72170

type Marble = Int
type Circle = UArray Int Int

data Round =
  Round {
   player :: Int,
   current :: Int,
   circle :: Circle,
   score :: Map.Map Int Int
  } deriving (Show)

-- Rewrite the indices so that 0..i <- 0..i and
-- (i+2)..(n+1) <- (i+1)..n
-- (i+1) needs to come from somewhere, we'll just duplicate i.
rewriteIndexInsert :: Int -> Int -> Int
rewriteIndexInsert i x = if x <= i then x else x-1

-- Rewrite the indices so that 0..d-1 <- 0..d-1 and
-- d..(n-1) <- (d+1)..n
rewriteIndexDelete :: Int -> Int -> Int
rewriteIndexDelete d x = if x < d then x else x+1

moduloSize :: Int -> Circle -> Int
moduloSize x a =
  let n = ((snd (bounds a)) + 1) in
    ( x + n ) `mod` n
  
increaseBounds :: Circle -> (Int,Int)
increaseBounds a =
  let b = bounds a in
    (fst b, 1 + snd b)

decreaseBounds :: Circle -> (Int,Int)
decreaseBounds a =
  let b = bounds a in
    (fst b, (snd b) - 1)
  
insertMarble :: Circle -> Int -> Marble -> (Circle, Int)
insertMarble a current m =
  let after = (current + 1) `moduloSize` a in
    ( (ixmap (increaseBounds a) (rewriteIndexInsert after) a) //
      [(after+1,m)],
      after+1 )

removeMarble :: Circle -> Int -> Circle
removeMarble a pos =
  ixmap (decreaseBounds a) (rewriteIndexDelete pos) a

nextPlayer :: Int -> Int -> Int
nextPlayer numPlayers x = if x == numPlayers then 1 else x+1

increaseScore :: Int -> Maybe Int -> Maybe Int
increaseScore x (Just y) = Just (x+y)
increaseScore x Nothing = Just x

nextRound :: Int -> Round -> Marble -> Round
nextRound numPlayers (Round p current circle score) m
  | m `mod` 23 == 0 =
    let remove = ( current - 7 ) `moduloSize` circle
        nextCurrent = remove
        points = (circle ! remove) + m
        nextScore = Map.alter (increaseScore points) p score
        nextCirc = removeMarble circle remove in
      Round (nextPlayer numPlayers p) nextCurrent nextCirc nextScore      
  | otherwise =
    let nextCirc = insertMarble circle current m in
      Round (nextPlayer numPlayers p) (snd nextCirc) (fst nextCirc) score
      
main :: IO ()

showCircle :: Circle -> String
showCircle c = intercalate " " (map show (elems c))

showGame :: Round -> String
showGame (Round p current circle score) = "[" ++ (show p) ++ "] {" ++ (show current) ++ "} " ++ (showCircle circle)

round0 = Round 1 0 start Map.empty

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
