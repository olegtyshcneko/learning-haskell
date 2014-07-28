module LogAnalysis where

import Log
import Data.Char
import Data.List

parseTestDataError = "E 2 562 help help" -- I need to parse this string to message
parseTestDataInfo = "I 147 Some tastefull message"
parseTestDataWarning = "W 2001 Hey, get out from there, please, don't stay"

--rewrite this function: error is not correct
parseMessageType :: String -> Maybe MessageType
parseMessageType ('I':' ':xs) = Just Info
parseMessageType ('W':' ':xs) = Just Warning
parseMessageType ('E':' ':xs) = Just (Error erroNo)
                 where erroNo = head (getAllNumbers xs)
parseMessageType _ = Nothing

--endBy :: Eq a => a -> [a] -> [a]
--endBy e xs = 

--isCorrectMessage
--gets String tells if message is correct or not

--getAllNumbers, given message return all numbers on those message
--write more general functions that finds all numbers on the string (try recursive one)
--I can split by space and then check if every String in [String] is number...

getAllNumbers :: String -> [Int]
getAllNumbers s = map read (getAllNsHelper s "")
              where getAllNsHelper [] acc
                         | null acc = []
                         | otherwise = (reverse acc) : []
                    getAllNsHelper (x:xs) acc
                         | isDigit x = getAllNsHelper xs (x:acc)
                         | otherwise = if not (null acc)
                                          then (reverse acc) : getAllNsHelper xs ""
                                          else getAllNsHelper xs ""

startsWith :: String -> String -> Bool
startsWith str check = take (length check) str == check

--only checks by first letters (need to go deeper)
--not all cases are being checked
isCorrectMessage :: String -> Bool
isCorrectMessage str
                 | not (parseMessageType str == Nothing) = True
                 | otherwise = False
                                 
isError :: MessageType -> Bool
isError (Error _) = True
isError _ = False

parseMessage :: String -> LogMessage
parseMessage s = LogMessage mType timeStamp m
             where (Just mType) = parseMessageType s
                   timeStamp = if isError mType
                                  then (last (getAllNumbers s))
                                  else head (getAllNumbers s)
                   (Just firstLetter) = findIndex isLetter (tail s)
                   m = drop (firstLetter + 1) s
