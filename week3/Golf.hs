module Golf where
import Data.List (intersperse)
  
-- Ex1

skips :: [a] -> [[a]]
skips xs = map (takeWithInterval xs) [1..(length xs)]
  where
    takeWithInterval xs n = map snd $ filter ((==0) . (`mod` n) . fst) $ zip [1..] xs

-- Ex3
histogram :: [Integer] -> String
histogram = concat . (intersperse "\n") . reverse . (["0123456789","=========="] ++) . drawGraph
  where
    countOf x = length . filter (==x)
    statOf xs = map (flip countOf $ xs) [0..9]
    genData = takeWhile (any (>0)) . iterate (map pred) . statOf
    drawGraph = (map (map (\x -> if x>0 then '*' else ' '))) . genData
