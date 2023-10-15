{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module TTT.A3 where

import TTT.A1
import TTT.A2
import Data.List (transpose)

-- Q#01

showInts :: [Int] -> [String]
showInts []     = []                   -- avaliation direction:  ("1" :    ("2" :    ("3" : []))) Left <- Right
showInts (x:xs) = show x : showInts xs -- In Heap's memory => show x1 : show x2 : show x3 : []

-- Just by my own
showIntsTR :: [Int] -> [String]
showIntsTR [] = []
showIntsTR xs = tr xs []
    where tr []     acc = reverse acc
          tr (x:xs) acc = tr xs $! (show x : acc)

_HEADER_ :: String
_HEADER_ =  ' ' : formatLine (showIntsTR _RANGE_)

-- Q#02

showSquares :: [Square] -> [String]
showSquares  []    = []
showSquares (x:xs) = showSquare x : showSquares xs

-- Just by my own
showSquaresTR :: [Square] -> [String]
showSquaresTR [] = []
showSquaresTR xs = tr xs []
    where tr []     acc = reverse acc
          tr (x:xs) acc = tr xs $! (show x : acc)

-- Q#03

formatRows :: Board -> String
formatRows  []    = []
formatRows (r:rs) = formatLine (showSquares r) <> formatRows rs

formatRows' rs = foldr ((<>) . formatLine . showSquares) [] rs

-- Q#04

-- Pattern with List
isColEmpty' :: Row -> Int -> Bool
isColEmpty' [     ] _ = False
isColEmpty' [E,_,_] 0 = True
isColEmpty' [_,E,_] 1 = True
isColEmpty' [_,_,E] 2 = True
isColEmpty' _       _ = False

-- Pattern with ( )
isColEmpty'' :: Row -> Int -> Bool
isColEmpty'' [     ]   _ = False
isColEmpty'' (E:_:_)   0 = True
isColEmpty'' (_:E:_)   1 = True
isColEmpty'' (_:_:[E]) 2 = True
isColEmpty'' _         _ = False

-- (!!)
-- O (ix)
isColEmpty3 :: Row -> Int -> Bool
isColEmpty3 [] _  = False
isColEmpty3 rs ix | ix >= 0 && ix < 3 = (!!) rs ix == E
                  | otherwise = False

-- Recursivity -> Your beatiful is TRUE :) haha
-- O (ix)
isColEmpty :: Row -> Int -> Bool
isColEmpty [] _  = False
isColEmpty xs ix = f xs 0
    where f []     _                     = False
          f (x:xs) i | x == E && i == ix = True
                     | i < ix            = f xs (i+1)
                     |otherwise          = False

-- Q#05

-- Pattern Matching
dropFirstCol :: Board -> Board
dropFirstCol [[  ], _   , _   ] = [[]]
dropFirstCol [_   ,[  ] , _   ] = [[]]
dropFirstCol [_   , _   ,[  ] ] = [[]]
dropFirstCol [_:as, _:bs, _:cs] = [as,bs,cs]

dropLastCol :: Board -> Board
dropLastCol [[  ] ,  _   , _  ] = [[]]
dropLastCol [ _   , [  ] , _  ] = [[]]
dropLastCol [ _   ,  _   ,[  ]] = [[]]
dropLastCol [[a, b, _],
             [c, d, _],
             [e, f, _]] = [a:[b], c:[d], e:[f]]

-- dropLastCol [a:b:_, c:d:_, e:f:_]= [a:[b], c:[d], e:[f]] -- Considering ever 3 elem per row

-- **Note: less code, more readable, forwardy, handle the all previous pattern matching **
dropFirstColMap :: Board -> Board
dropFirstColMap = map (drop 1)

dropLastColMap :: Board -> Board
dropLastColMap  = map (take (_SIZE_ - 1))

-- Q#06

-- getDiag1 []        = []
-- getDiag1 [[],_,_]  = []
-- getDiag1 [_,[],_]  = []
-- getDiag1 [_,_,[]]  = []
-- getDiag1 [[x],_,_] = []
-- getDiag1 [_,[x],_] = []
-- getDiag1 [_,_,[x]] = []
-- getDiag1 [[x,y],_,_] = []
-- getDiag1 [_,[x,y],_] = []
-- getDiag1 [_,_,[x,y]] = []

getDiag1 :: Board -> Line
getDiag1 [[a,_,_]
         ,[_,b,_]
         ,[_,_,c]] = [a,b,c]

getDiag2 :: Board -> Line
getDiag2 [[_,_,c]
         ,[_,b,_]
         ,[a,_,_]] = [c,b,a]

-- Using dropFirstColMap as help function
getDiag1' :: Board -> Line
getDiag1' xs = get xs 0
    where      get _  3          = []
               get ((x:_):ls) ix = x : get (dropFirstColMap (ls <> [[]])) (ix+1)

-- Recursion by Index
getDiag1'' :: Board -> Line
getDiag1'' xs = get xs 0
    where       get [] _      = []
                get (l:ls) ix = (!!) l ix : get ls (ix+1)

-- Recursion ever geting last
getDiag2' :: Board -> Line
getDiag2' xs = get xs
    where get [[],[],[]]  = []
          get [s,t,a:b:c] = c <> get ( [[]] <> ([s]<>[t]) )

-- Recursion by Index
getDiag2'' :: Board -> Line
getDiag2'' xs = get xs 2
    where get []     _  = []
          get (l:ls) ix = (!!) l ix : get ls (ix-1)

b = [[X,O,E],
     [O,E,X],
     [E,X,X]]

getDiag1''' b = [ b !! l !! c | l <-[0..2], c <- [0..2], l==c ] 

getDiag2''' b = [ b !! l !! c | l <-[0,1,2], c <-[2,1,0], 2==l+c ]  

getAllLines :: Board -> [Line]
getAllLines b = concat [b 
                       ,transpose b
                       ,[getDiag1 b]
                       ,[getDiag2 b]
                       ]

-- Q#07

putSquare = undefined

-- Q#08

prependRowIndices = undefined

-- Q#09

isWinningLine_ = undefined

-- Q#10

isValidMove = undefined
