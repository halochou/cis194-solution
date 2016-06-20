{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.Tree

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

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f acc (Node rl []) = f rl acc
treeFold f acc (Node rl (t:ts)) = treeFold f (treeFold f acc t) (Node rl ts)

-- Ex3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (glCons boss glWithBoss, glWithoutBoss)
  where
    glWithBoss = foldr (mappend . snd) mempty gls
    glWithoutBoss = foldr (mappend . fst) mempty gls

-- Ex4

maxFun :: Tree Employee -> GuestList
maxFun = moreFun . curry . treeFold nextLevel [(mempty, mempty)] 
