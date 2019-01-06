#!/usr/bin/env stack
-- stack --install-ghc runghc --package raw-strings-qq-1.1 --package split-0.2.3.3

{-# LANGUAGE QuasiQuotes #-}
import Text.RawString.QQ
import Data.Array.Unboxed (UArray,listArray,(!))
import Data.List (dropWhileEnd,zip5,intercalate)

-- first character is pot 0
inputState = "#.#..#..###.###.#..###.#####...########.#...#####...##.#....#.####.#.#..#..#.#..###...#..#.#....##."

-- sorted version of input
rules = [r|..... => .
....# => .
...#. => #
...## => .
..#.. => #
..#.# => #
..##. => .
..### => .
.#... => #
.#..# => #
.#.#. => .
.#.## => #
.##.. => .
.##.# => .
.###. => #
.#### => #
#.... => .
#...# => .
#..#. => #
#..## => #
#.#.. => .
#.#.# => .
#.##. => #
#.### => .
##... => #
##..# => #
##.#. => #
##.## => .
###.. => #
###.# => #
####. => .
##### => .|]

data CAState =
  CAState Int [Int] [Int]

showCA :: CAState -> String
showCA (CAState i ps ss) =
  (show i) ++ ": " ++ (map intToPlant ps) ++ " " ++ (show ss)

offset (CAState i _ _) = i
plants (CAState _ ps _) = ps
ships (CAState _ _ ss) = ss

startState s = CAState 0 (map plantToInt s) []

plantToInt :: Char -> Int
plantToInt '#' = 1
plantToInt '.' = 0

intToPlant :: Int -> Char
intToPlant 1 = '#'
intToPlant 0 = '.'

ruleOutcome :: String -> Int
ruleOutcome r = plantToInt (r !! 9)

type RuleTable = UArray Int Int

parseRules :: String -> RuleTable
parseRules text =
  listArray (0,31) (map ruleOutcome (lines text))

-- Next state of middle cell given neighbors
nextCell:: RuleTable -> (Int,Int,Int,Int,Int) -> Int
nextCell rt (a,b,c,d,e) = rt ! (16*a + 8*b + 4*c + 2*d + e) 

trim = trimLeft
-- trim x = x

trimLeft :: CAState -> CAState
trimLeft (CAState i (0:cs) ss) = trimLeft (CAState (i+1) cs ss)
trimLeft (CAState i cs ss) = trimRight i (length cs) (reverse cs) ss

---   0123456789ABC        
---   xxxxx.....###
--- i ^           ^ i + l - 1 = end position
---             ^ i + l - 3 = spaceship start position
trimRight :: Int -> Int -> [Int] -> [Int] -> CAState
trimRight i lc (0:cs) ss = trimRight i (lc-1) cs ss
trimRight i lc (1:1:1:0:0:0:cs) ss =
  let ssPos = i + lc - 3
      lc' = lc - 6 in
      trimRight i lc' cs (ssPos:ss)
trimRight i lc cs ss = CAState i (reverse cs) ss

-- Next state of whole CA
nextState rt (CAState i cs ss) =
  let expanded = [0,0,0,0] ++ cs ++ [0,0,0,0]
      neighborhood = zip5 expanded (drop 1 expanded) (drop 2 expanded) (drop 3 expanded) (drop 4 expanded)
      i' = i - 2 in
    trim (CAState i'
          (map (nextCell rt) neighborhood)
          (map (1+) ss))

allStates rt ca = iterate (nextState rt) ca  
nthState rt ca n = (allStates rt ca) !! n

showWithValue ca =
  ( showCA ca) ++ " value " ++ (show $ value ca )

evolution rt ca n =
  intercalate "\n" (map showWithValue (take n (allStates rt ca)))

value ca =
  let is = iterate (1+) (offset ca)
      cells = (zip is (plants ca))
      vals = map (uncurry (*)) cells in
    sum ( vals ++ map ssVal (ships ca))

-- Fortunately, we don't need to handle cycles of length greater than 1
findRepeat n ((CAState i ss cs):(CAState i' ss' cs'):rest) | ss == ss' =
  (n, (CAState i ss cs))
findRepeat n (ca:rest) = findRepeat (n+1) rest
    
ssVal ss = 3*ss + 3

part1 = 
  let gen20 = nthState (parseRules rules) (startState inputState) 20 in
     value gen20

part2 =
  let r = findRepeat 0 (allStates (parseRules rules) (startState inputState))
      ca = snd r
      increasePerRound = (length (ships ca)) * 3 + (sum (plants ca))
      start = fst r
      end = 50000000000
      additionalRounds = end - start
      endValue = (value ca) + ( additionalRounds * increasePerRound )
  in
    putStrLn (show (fst r) ) >>
    putStrLn (showCA (snd r) ) >>
    putStrLn ((show increasePerRound) ++ " value increase per round") >>
    putStrLn ((show endValue) ++ " after " ++ (show additionalRounds) ++ " rounds")
    
main :: IO ()
main =
  putStrLn (show (parseRules rules)) >>
  putStrLn ("Part 1: " ++ (show part1)) >>
  putStrLn "200 generations:" >> 
  putStrLn (evolution (parseRules rules) (startState inputState) 200 ) >>
  putStrLn "Part 2:" >>
  part2
  

