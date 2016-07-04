{-# OPTIONS_GHC -Wall #-}

-- Ex1

import Data.Char

toDigits :: Integer -> [Integer]
toDigits n 
    | n > 0 = map (toInteger . digitToInt) $ show n
    | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Ex2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =
    reverse $ zipWith (*) (reverse xs) (cycle [1,2])

-- Ex3

sumDigits :: [Integer] -> Integer
sumDigits = sum . (concatMap toDigits)

-- Ex4

validate :: Integer -> Bool
validate = (==0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

-- Ex5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _      = []
hanoi n pa pb pc = (hanoi (n-1) pa pc pb)  ++ [(pa, pb)] ++ (hanoi (n-1) pc pb pa)
