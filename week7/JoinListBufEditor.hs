module Main where

import Sized
import Scrabble
import Buffer
import JoinList
import Editor

initialText :: JoinList (Score, Size) String
initialText = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

main :: IO ()
main = runEditor editor initialText
