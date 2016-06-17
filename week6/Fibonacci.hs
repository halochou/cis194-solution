-- Ex1

fib :: Integer -> Integer
fib 1 = 0
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

-- Ex2

fibs2 :: [Integer]
fibs2 =  map fst $ iterate (\(x, y) -> (y, x+y)) (0,1)

-- Ex3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons n next) = n:streamToList next

instance Show a => Show (Stream a) where
  show = unwords . take 20 . (map show) . streamToList

-- Ex4

streamRepeat :: a -> Stream a
streamRepeat n = Cons n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f n = Cons n (streamFromSeed f $ f n)

-- Ex5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap rulerOf $ streamFromSeed (+1) 1
rulerOf :: Integer -> Integer
rulerOf n
  | odd n = 0
  | otherwise = 1 + rulerOf (n `div` 2)
