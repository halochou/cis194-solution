{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Ex1

parseMessage :: String -> LogMessage
parseMessage ('E':args) =
  LogMessage (Error (read errorLevel)) (read timeStamp) (unwords messages)
  where
    errorLevel:timeStamp:messages = words args

parseMessage ('I':args) =
  LogMessage Info  (read timeStamp) (unwords messages)
  where
    timeStamp:messages = words args

parseMessage ('W':args) =
  LogMessage Warning (read timeStamp) (unwords messages)
  where
    timeStamp:messages = words args

parseMessage message = Unknown message

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Ex2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msg = msg 
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage (Node leftMessageTree nodeLogMessage rightMessageTree)
  | getTimeStamp logMessage <= getTimeStamp nodeLogMessage = Node (insert logMessage leftMessageTree) nodeLogMessage rightMessageTree
  | otherwise = Node leftMessageTree nodeLogMessage (insert logMessage rightMessageTree)
  where
    getTimeStamp :: LogMessage -> Int
    getTimeStamp (LogMessage _ timeStamp _) = timeStamp
    getTimeStamp _ = 0

-- Ex3

build :: [LogMessage] -> MessageTree
build = (foldl (flip insert) Leaf)

-- Ex4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftMsgTree nodeLogMsg rightMsgTree) =
  (inOrder leftMsgTree) ++ [nodeLogMsg] ++ (inOrder rightMsgTree)

-- Ex5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map toMessage) . (filter isErrorOver50) . inOrder . build
  where
    isErrorOver50 (LogMessage (Error level) _ _) | level >= 50 = True
    isErrorOver50 _ = False
    toMessage (LogMessage _ _ msg) = msg
    toMessage _ = ""
