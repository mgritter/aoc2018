#!/usr/bin/env stack
-- stack --install-ghc runghc

import Data.List (zip4,maximum,foldl')
import Data.Array (Array,array,(!))

gridRange = [1..300]

type LevelFunction = Int -> Int -> Int

fuelCellLevel :: Int -> LevelFunction
fuelCellLevel serialNumber y x =
  let rackId = (x+10)
      allDigits = (rackId * y + serialNumber) * rackId in
    ((allDigits `div` 100) `mod` 10) - 5


-- Compute an entire row of 3x3 values by zipping the values of this function
--
-- row 1 = [ x1y1 x2y1 ... x300y1 ]
-- row 2 = [ x1y2 x2y2 ... x300y2 ]
--
-- zip3 rows (drop1 rows) (drop 2 rows)
-- = [ (row1, row2, row3), (row2, row3, row4)
--
-- Adding corresponding entries (column) in each triple of rows gives
--   [ row123, row234, row345 ] each of which is a list

rowLevels fn y = (map (fn y) gridRange)

sum3 (a,b,c) = a + b + c

sumColumns (y,as,bs,cs) =
    (y, map sum3 (zip3 as bs cs))

sumSquare y (x,a,b,c) = (a+b+c, x, y)

sumSquares :: (Int,[Int]) -> [(Int,Int,Int)]
sumSquares (y,cols) =
  let threeCols = zip4 gridRange cols (drop 1 cols) (drop 2 cols) in
    map (sumSquare y) threeCols

threeByThreeLevels :: (Int -> Int -> Int) -> [(Int,Int,Int)]
threeByThreeLevels fn =
  let rows = [ rowLevels fn y | y <- gridRange ] :: [[Int]]
      threeRows = zip4 gridRange rows (drop 1 rows) (drop 2 rows) :: [(Int,[Int],[Int],[Int])]
      threeRowsSummed = map sumColumns threeRows in
    concat (map sumSquares threeRowsSummed)

maxSquare serialNumber = maximum (threeByThreeLevels (fuelCellLevel serialNumber))

-- Return an entire row's worth of sums 
rowPartialSums :: LevelFunction -> Int -> Array Int Int -> Array Int Int
rowPartialSums fn y prevRow =
  let a = array (1,300) ((1, (prevRow!1) + fn y 1) :
                         [(x, (a!(x-1)) + (prevRow!x) + (fn y x) - (prevRow!(x-1))) | x <- [2..300] ])  in a

-- Entire matrix of sums, (array ! y) ! x = sum from (1,1) to (y,x)
partialSums :: LevelFunction -> Array Int (Array Int Int)
partialSums fn =
  let zero = array (1,300) [(x,0) | x <- [1..300]]
      rows = array (1,300) ((1, rowPartialSums fn 1 zero) :
                            [(y, rowPartialSums fn y (rows!(y-1))) | y <- [2..300] ]) in rows

areaSum :: Array Int (Array Int Int) -> Int -> Int -> Int -> Int
areaSum a 1 1 size = let
  x' = size
  y' = size in
    (a ! y') ! x'

areaSum a 1 x size = let
  x' = x + size - 1
  y' = size in
  (a ! y') ! x' - (a ! y') ! (x-1)

areaSum a y 1 size = let
  x' = size
  y' = y + size - 1 in
  (a ! y') ! x' - (a ! (y-1)) ! x'

areaSum a y x size = let
  x' = x + size - 1
  y' = y + size - 1 in
  (a ! y') ! x' - (a ! (y-1)) ! x' - (a ! y') ! (x-1) + (a ! (y-1)) ! (x-1)

sums serialNumber = partialSums (fuelCellLevel serialNumber)

maximum' = foldl' max (0,0,0,0)

maxSquareK :: Int -> (Int,Int,Int,Int)
maxSquareK sn = let a = sums sn in
  maximum' [ (areaSum a y x size, x, y, size) |
            size <- [1..300],
            x <- [1..301-size],
            y <- [1..301-size] ]

main :: IO ()
main = 
  putStrLn ("serial number 18 best 3x3: " ++ (show (maxSquare 18))) >>
  putStrLn ("serial number 42 best 3x3: " ++ (show (maxSquare 42))) >>
  putStrLn ("serial number 1955 best 3x3: " ++ (show (maxSquare 1955))) >>
  putStrLn ("serial number 18 area check: " ++
             (show (areaSum (sums 18) 45 33 3))) >>
  putStrLn ("serial number 42 area check: " ++
             (show (areaSum (sums 42) 61 21 3))) >>
  putStrLn ("serial number 1955 area check: " ++
             (show (areaSum (sums 1955) 93 21 3))) >>
  putStrLn ("serial number 18 best square: " ++
             (show (maxSquareK 18))) >>
  putStrLn ("serial number 42 best square: " ++
             (show (maxSquareK 42))) >>
  putStrLn ("serial number 1955 best square: " ++
             (show (maxSquareK 1955)))
  
  
