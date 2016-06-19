module Golf where
import Data.List (intersperse)
  
-- Ex1

skips :: [a] -> [[a]]
skips xs = map (takeWithInterval xs) [1..(length xs)]
  where
    takeWithInterval xs n = map snd $ filter ((==0) . (`mod` n) . fst) $ zip [1..] xs

-- Ex2

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map getFst $ filter isMaxima $ zip3 xsMiddle xsShiftL xsShiftR
  where
    xsMiddle = drop 1 xs
    xsShiftL = drop 2 xs
    xsShiftR = xs
    isMaxima (xs,xsL,xsR) = xs >= xsL && xs >= xsR
    getFst (x, _, _) = x

-- Ex3
histogram :: [Integer] -> String
histogram = concat . (intersperse "\n") . reverse . (["0123456789","=========="] ++) . drawGraph
  where
    countOf x = length . filter (==x)
    statOf xs = map (flip countOf $ xs) [0..9]
    genData = takeWhile (any (>0)) . iterate (map pred) . statOf
    drawGraph = (map (map (\x -> if x>0 then '*' else ' '))) . genData
