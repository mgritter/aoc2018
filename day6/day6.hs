#!/usr/bin/env stack
-- stack --install-ghc runghc --package raw-strings-qq-1.1 --package split-0.2.3.3

{-# LANGUAGE QuasiQuotes #-}
import Text.RawString.QQ
import Data.List (minimum,maximum,(\\),intercalate,sortBy)
import Data.List.Split (splitOn)

input = [r|183, 157
331, 86
347, 286
291, 273
285, 152
63, 100
47, 80
70, 88
333, 86
72, 238
158, 80
256, 140
93, 325
343, 44
89, 248
93, 261
292, 250
240, 243
342, 214
192, 51
71, 92
219, 63
240, 183
293, 55
316, 268
264, 151
68, 98
190, 288
85, 120
261, 59
84, 222
268, 171
205, 134
80, 161
337, 326
125, 176
228, 122
278, 151
129, 287
293, 271
57, 278
104, 171
330, 69
141, 141
112, 127
201, 151
331, 268
95, 68
289, 282
221, 359|]

type Coord = (Int,Int)

parsePair :: String -> Coord
parsePair x =
  case splitOn ", " x of
    (a:b:[]) -> (read a, read b)

points :: [Coord]
points = map parsePair (lines input)

nearestPoint :: [Coord] -> Coord -> Maybe Coord
nearestPoint cs c =
  let order = sortBy (closestTo c) cs in
    let first = head order
        second = head (tail order) in
        if (manhattanDistance c first) == (manhattanDistance c second)
          then Nothing
          else Just first

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1,y1) (x2,y2) = abs( x1 - x2 ) + abs( y1 - y2 )

closestTo :: Coord -> Coord -> Coord -> Ordering
closestTo goal a b = compare (manhattanDistance goal a) (manhattanDistance goal b)

longWay = 2000

infiniteArea :: [Coord] -> Coord -> Bool
infiniteArea cs (x,y) =
  ( nearestPoint cs (x+longWay,y) == Just (x,y) ) ||
  ( nearestPoint cs (x-longWay,y) == Just (x,y) ) ||
  ( nearestPoint cs (x,y+longWay) == Just (x,y) ) ||
  ( nearestPoint cs (x,y-longWay) == Just (x,y) )

externalPoints = filter (infiniteArea points) points
internalPoints = points \\ externalPoints

circleAt :: Coord -> Int -> [Coord]
circleAt center 0 = [ center ]

--   A
--  A D
-- A   B
--  C B
--   B
circleAt (x,y) r =
  [ (x-r+i, y  +i) | i <- [0..r] ] ++
  [ (x  +i, y-r+i) | i <- [0..r] ] ++
  [ (x-r+i, y  -i) | i <- [1..r-1] ] ++
  [ (x  +i, y+r-i) | i <- [1..r-1] ]

areaAt points center radius =
  let a = length (filter (\p -> nearestPoint points p == Just center)
                         (circleAt center radius)) in
    if a == 0 then
      0
    else
      a + areaAt points center (radius + 1)

reportArea c =
  "Area around " ++ show( c ) ++ " = " ++ (show (areaAt points c 0))

report = intercalate "\n" [ reportArea c | c <- internalPoints ]

-- Part 2 --
-- We want the locus of all points with d(1) + d(2) + ... + d(50) <= 10000

minX = minimum (map fst points)
maxX = maximum (map fst points)
minY = minimum (map snd points)
maxY = maximum (map snd points)

rasterScanArea = [(i,j) | i <- [minX..maxX], j <- [minY..maxY] ]

sumOfDistances :: [Coord] -> Coord -> Int
sumOfDistances cs c = sum (map (manhattanDistance c) cs)

safeArea = length (filter (\c -> sumOfDistances points c < 10000) rasterScanArea )

main :: IO ()
main =
  putStrLn (show externalPoints) >>
  putStrLn (show internalPoints) >>
  putStrLn report
  putStrLn (show safeArea)
    
