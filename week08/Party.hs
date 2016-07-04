{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.Tree
import Data.List (sort)

-- Ex1

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps totalFun) = GL (emp:emps) (empFun emp + totalFun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL emps1 fun1) (GL emps2 fun2) = GL (emps1 ++ emps2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun l@(GL _ fl) r@(GL _ fr)
  | fl > fr = l
  | otherwise = r

-- Ex2

treeFold :: (a -> [b] -> b) -> [b] -> Tree a -> b
treeFold f acc (Node rl []) =  f rl acc
treeFold f acc (Node rl subTrees) = f rl (map (treeFold f acc) subTrees)

-- Ex3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (glCons boss glWithBoss, glWithoutBoss)
  where
    glWithBoss = foldr (mappend . snd) mempty gls
    glWithoutBoss = foldr (mappend . uncurry max) mempty gls

-- Ex4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel [(mempty, mempty)]

-- Ex5

main :: IO ()
main = do
  content <- readFile "company.txt"
  putStrLn $ formatGL . maxFun $ read content


formatGL :: GuestList -> String
formatGL (GL emps fun) = unlines $ headLine : sort (map empName emps)
  where headLine = "Total fun: " ++ show fun
