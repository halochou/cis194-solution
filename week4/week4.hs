
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
-- WIP
fun2' :: Integer -> Integer
fun2' n
  | even n = sum $ takeWhile (>1) $ iterate (`div` 2) n
  | otherwise = fun2' (3 * n + 1)
  
