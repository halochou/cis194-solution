{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Sized
import Data.Monoid
import Scrabble
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)

  deriving (Eq, Show)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Ex1

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ right = right
left +++ right = Append (tag left <> tag right) left right

-- Ex2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ x) = Just x
indexJ _ (Single _ _) = Nothing
indexJ i (Append total left right)
  | i >= t = Nothing
  | i < l = indexJ i left
  | otherwise = indexJ (i-l) right
  where
    t = getSize $ size total
    l = getSize $ size $ tag left

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl = jl
dropJ _ (Single _ _) = Empty
dropJ i (Append total left right)
  | i > t = Empty
  | i < l = dropJ i left +++ right
  | otherwise = dropJ (i - l) right
  where
    t = getSize $ size total
    l = getSize $ size $ tag left

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 _ = Empty
takeJ _ jl@(Single _ _) = jl
takeJ i jl@(Append total left right)
  | i > t = jl
  | i < l = takeJ i left
  | otherwise = left +++ takeJ (i-l) right
  where
    t = getSize $ size total
    l = getSize $ size $ tag left

-- Ex3

scoreLine :: String -> JoinList Score String
scoreLine "" = Empty
scoreLine x = Single (scoreString x) x

-- Ex4

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = fromLines . lines
    where
      fromLines [a] = Single (scoreString a, Size 1) a
      fromLines xs = fromLines left +++ fromLines right
        where
          (left, right) = splitAt (length xs `div` 2) xs

  line = indexJ
  replaceLine n ln buf = takeJ n buf +++ fromString ln +++ dropJ (n+1) buf
  numLines Empty = 0
  numLines (Single _ _) = 1
  numLines (Append _ l r) = numLines l + numLines r
  value = getScore . fst . tag
