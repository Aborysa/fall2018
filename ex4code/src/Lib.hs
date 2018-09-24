module Lib
    ( Token(..)
    , Op(..)
    , takeWhile
    , dropWhile
    , break
    , splitOn
    , lex
    , tokenize
    , interpret
    , shunt
    ) where

import Prelude hiding (lex, takeWhile, dropWhile, break)
import Data.Char (isDigit)
import Data.Int
import Data.List hiding (lex, takeWhile, dropWhile, break)



takeWhile, dropWhile :: (a -> Bool) -> [a] -> [a]

takeWhile eq [] = []
takeWhile eq (h:t)
  | eq h = h : takeWhile eq t
  | otherwise = []


dropWhile eq [] = []
dropWhile eq list@(h:t)
  | eq h = dropWhile eq t
  | otherwise = list

break :: (a -> Bool) -> [a] -> ([a], [a])
break _ [] = ([],[])
break eq list@(h:t)
  | eq h = ([], list)
  | otherwise = case break eq t of
    (a, b) -> (h:a, b)




splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []


splitOn token list@(h:t)
    | token==h = splitOn token t
    | otherwise = a : (splitOn token b)
    where (a, b) = break (==token) list


--Does not work for some reason
--splitOn token list = a : splitOn token b
--  where (a, b) = break (==token) (dropWhile (==token) list)



data Token = TokOp Op
           | TokInt Int
           | TokErr
           deriving (Eq, Show)

data Op = Plus
        | Minus
        | Div
        | Mult
        | AddInv
        | Dupe
        deriving (Show, Eq)

lex :: String -> [String]
lex a = splitOn ' ' a

stringToToken "+" = TokOp Plus
stringToToken "--" = TokOp AddInv
stringToToken "#" = TokOp Dupe
stringToToken "-" = TokOp Minus
stringToToken "/" = TokOp Div
stringToToken "*" = TokOp Mult
stringToToken a
  | justDigits == a = TokInt (read a)
  where justDigits = takeWhile isDigit a

stringToToken _ = TokErr




tokenize :: [String] -> [Token]
tokenize a
  | TokErr `elem` parsed = [TokErr]
  | otherwise = parsed
  where parsed = [stringToToken x | x <- a]


doop :: Int -> Int -> Op -> Int

doop a b Plus = a + b
doop a b Minus = a - b
doop a b Mult = a * b
doop a b Div = a `div` b

doopMaybe :: Int -> Op -> Maybe [Int]
doopMaybe a Dupe = Just [a, a]
doopMaybe a AddInv = Just [-a]
doopMaybe _ _ = Nothing


interpret :: [Token] -> [Token]
--interpret (TokInt a:TokOp c:xs) = case c of
--  Just [a1, b1] -> [TokInt a1, TokInt b1]
--  Just [a1] -> [TokInt a1]
--  Nothing -> [a, c] :
interpret a@(TokInt b:TokOp c:xs) = case doopMaybe b c of
  Just r -> interpret ([TokInt x | x <- r] ++ xs)
  Nothing -> a

interpret (TokInt a:TokInt b:TokOp c:xs) = case doopMaybe b c of
  Just r -> interpret ([TokInt x | x <- r] ++ xs)
  Nothing -> interpret (TokInt (doop a b c) : xs)

interpret list@(a:t)
  | ev == list = list
  | otherwise = interpret ev
  where ev = (a : interpret t)
interpret a = a


opLeq :: Token -> Token -> Bool
opLeq a b
  | a == b = False
opLeq _ (TokOp AddInv) = True
opLeq _ (TokOp Dupe) = True
opLeq _ (TokOp Mult) = True
opLeq _ (TokOp Div) = True
opLeq _ (TokOp Minus) = True
opLeq _ (TokOp Plus) = False

shunt :: [Token] -> [Token]
shunt a = interpret $ shuntInternal a [] []

shuntInternal :: [Token] -> [Token] -> [Token] -> [Token]
shuntInternal [] out op = out++(sortBy (\a b -> if opLeq a b then GT else LT) op)
shuntInternal (TokInt h:xs) out op = shuntInternal xs (out++[TokInt h]) op
shuntInternal (TokOp h:xs) out [] = shuntInternal xs out [TokOp h]
shuntInternal i@(TokOp h:xs) out op@(t:rs)
  | opLeq (TokOp h) t = shuntInternal i (out++[t]) rs
  | otherwise = shuntInternal xs out op++[TokOp h]
