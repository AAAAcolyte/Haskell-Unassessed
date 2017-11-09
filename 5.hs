data Shape = Tri Float Float Float | Squ Float | Cir Float | Poly [Vertex]
data NewTree a = NewLeaf a | NewNode (NewTree a) (NewTree a)
  deriving (Eq,Show)
data Tree = Leaf | Node Tree Tree
  deriving (Eq,Show)
type Vertex = (Float,Float)
type Date = (Int,Int,Int)
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

makeTrees :: Int -> [Tree]
makeTrees n
  | n == 0 = [Leaf]
  | otherwise = [Node l r | (t, t') <- map makeTrees' [0..n-1],
  l <- t, r <- t']
  where
    makeTrees' k = (makeTrees k, makeTrees (n - k - 1))

build :: [a] -> NewTree a
build [x] = NewLeaf x
build list = NewNode (build leftPart) (build rightPart)
    where
      (leftPart,rightPart) = splitAt midPoint list
      midPoint = div lOfList 2
      lOfList = length list

ends :: NewTree a -> [a]
ends (NewLeaf x) = [x]
ends (NewNode left right) = ends left ++ ends right

swap :: NewTree a -> NewTree a
swap (NewLeaf x) = NewLeaf x
swap (NewNode left right) = NewNode (swap right) (swap left)
