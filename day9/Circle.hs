module Circle where

-- current = head right
data Circle = Circle {
  left :: [Int],  -- stored in reverse order
  right :: [Int] 
}

singleton :: Int -> Circle
singleton x = Circle [] [x]

next :: Circle -> Circle
next (Circle [] (y:[])) = Circle [] (y:[])
next (Circle (x:xs) (y:[])) = Circle [] (reverse (y:x:xs))
next (Circle xs (y:ys)) = Circle (y:xs) ys

prev :: Circle -> Circle
prev (Circle [] ys) =
  let rev = (reverse ys) in
    (Circle (tail rev) [(head rev)])
prev (Circle (x:xs) ys) = Circle xs (x:ys)

insert :: Circle -> Int -> Circle
insert (Circle xs ys) y = Circle xs (y:ys)

remove :: Circle -> (Circle,Int)
remove (Circle xs (y:[])) = (Circle [] (reverse xs), y)
remove (Circle xs (y:ys)) = (Circle xs ys, y)

toList :: Circle -> [Int]
toList (Circle xs ys) = ys ++ (reverse xs)
