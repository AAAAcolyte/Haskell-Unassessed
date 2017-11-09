data Shape = Tri Float Float Float | Squ Float | Cir Float | Poly [Vertex]
data Tree a = Empty | Node (Tree a) a (Tree a)
  deriving (Show)
type Vertex = (Float,Float)
type Date = (Int,Int,Int)

tmpTree = Node (Node (Node Empty 'A' Empty) 'B' (Node (Node Empty 'C' Empty) 'D' (Node Empty 'E' Empty)))
 'F' (Node Empty 'G' (Node (Node Empty 'H' Empty) 'I' Empty))
area :: Shape -> Float
area (Tri a b c) = sqrt(s*(s-a)*(s-b)*(s-c))
  where
    s = (a+b+c)/2
area (Squ a) = a*a
area (Cir r) = pi * r^2
area (Poly (v1:v2:v3:ls)) = area (Tri a b c) + area (Poly (v1:v3:ls))
  where
    a = dist v1 v2
    b = dist v1 v3
    c = dist v2 v3
    dist :: Vertex -> Vertex -> Float
    dist (x,y) (x',y') = sqrt((x-x')^2+(y-y')^2)

age :: Date -> Date -> Int
age (y1,m1,d1) (y2,d2,m2)
  | (m1,d1) <= (d2,m2) = y2 - y1
  | otherwise = y2 - y1 - 1

inOrder :: Tree a -> [a]
inOrder Empty
  = []
inOrder (Node t1 x t2)
  = inOrder t1 ++ (x : inOrder t2)

preOrder :: Tree a -> [a]
preOrder Empty
  = []
preOrder (Node t1 x t2)
  = x : (preOrder t1 ++ preOrder t2)

postOrder :: Tree a -> [a]
postOrder Empty
  = []
postOrder (Node t1 x t2)
  = postOrder t1 ++ postOrder t2 ++ [x]
