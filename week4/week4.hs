
-- Ex1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = (foldl (\acc x -> acc * (x-2)) 1) . (filter even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)
fun2' :: Integer -> Integer
fun2' = sum . filter (even) . takeWhile (/=1) . iterate (\x -> if even x then x `div` 2 else 3*x+1)

-- Ex2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: Eq a => [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node d l v r)
  | depthOf l < depthOf r =
    let newL = insert x l
    in
      Node ((depthOf newL `max` depthOf r)+1) newL v r
  | otherwise =
    let newR = insert x r
    in
      Node ((depthOf l `max` depthOf newR)+1) l v newR
  where
    depthOf Leaf = -1
    depthOf (Node d _ _ _) = d

