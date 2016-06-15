{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import Control.Applicative (liftA2)
import qualified ExprT as E
import Parser (parseExp)
import StackVM

-- Ex1

eval :: E.ExprT -> Integer
eval (E.Lit n) = n
eval (E.Add exprL exprR) = (eval exprL) + (eval exprR)
eval (E.Mul exprL exprR) = (eval exprL) * (eval exprR)

-- Ex2

evalStr :: String -> Maybe Integer
evalStr str = eval <$> parseExp Lit Add Mul str

-- Ex3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr E.ExprT where
  lit = E.Lit
  add = E.Add
  mul = E.Mul

-- Ex4

newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x1) (MinMax x2) = MinMax (max x1 x2)
  mul (MinMax x1) (MinMax x2) = MinMax (min x1 x2)

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 x1) (Mod7 x2) = Mod7 ((x1 + x2) `mod` 7)
  mul (Mod7 x1) (Mod7 x2) = Mod7 ((x1 * x2) `mod` 7)



testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- Ex5
instance Expr Program where
  lit n = [PushI n]
  add x1 x2 = [PushI x1, PushI x2, Add]
  mul x1 x2 = [PushI x1, PushI x2, Mul]

testPg = testExp :: Maybe Program

--compile :: String -> Maybe Program
