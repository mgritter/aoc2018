module TwoDTree (TwoDTree,tree,nearestNeighbor) where

type Location = (Int,Int)

data Axis = XAxis | YAxis

otherAxis XAxis = YAxis
otherAxis YAxis = XAxis

data TwoDTree =
  Empty | 
  TreeNode Axis Location TwoDTree TwoDTree |
  LeafNode Location
  deriving Show

extractCoord :: Axis -> Location -> Int
extractCoord XAxis (x,_) = x
extractCoord YAxis (_,y) = y
  
tree :: Axis -> [Location] -> TwoDTree
tree _ [] = Empty
tree _ (l:[]) = LeafNode l 
tree axis ls =
  TreeNode axis medianPoint
     (tree (otherAxis axis) (beforeMedian axis medianPoint ls))
     (tree (otherAxis axis) (afterMedian axis medianPoint ls))
  where medianPoint = medianOnAxis axis ls

beforeMedian axis medianPoint ls =
  filter (\l -> (extractCoord axis l) < (extractCoord axis medianPoint)) ls

afterOrNotEqual axis a b =
  let a1 = (extractCoord axis a)
      b1 = (extractcoord axis b)
      in ( a1 > b1 ) || ( a1 == b1 && a != b ) 
        
afterMedian axis medianPoint ls =
  filter (\l -> afterOrNotEqual axis l medianPoint ) ls

compareAxis :: Axis -> Location -> Location -> Ordering
compareAxis a l1 l2 = compare (extractCoord a l1) (extractCoord a l2)

medianOnAxis axis ls =
  let sortedList = sortBy (compareAxis axis) ls in
    sortedList !! ((length ls) `div` 2)

manhattanDistance :: Location -> Location -> Int
manhattanDistance (x1,y1) (x2,y2) = abs( x1 - x2 ) + abs( y1 - y2 )

nearestNeighbor :: TwoDTree -> Location -> Maybe Location
nearestNeighbor (LeafNode x) _ = Just x
nearestNeighbor Empty _ = Nothing
nearestNeighbor (TreeNode axis l1 leftTree rightTree) l2 =
  let nn = if (compareAxis

              -- FIXME --

