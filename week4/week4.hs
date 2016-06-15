
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

-- Ex3

xor :: [Bool] -> Bool
xor = foldr (\x acc -> if x then not acc else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- Ex4

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = snd $ last $ takeWhile (\(nth, xs) -> nth < length xs) $ iterate cross (0, [2..(2*n+2)])

cross :: (Int, [Integer]) -> (Int, [Integer])
cross (n, xs) = (n+1, filter (\x -> (x `mod` (xs !! n)) /= 0 || x == (xs !! n)) xs)

-- Much better solution from Haskell official website
primes = filterPrime [2..] 
  where filterPrime (p:xs) = 
          p : filterPrime [x | x <- xs, x `mod` p /= 0]
