{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TTT.A3 where

import TTT.A1
import TTT.A2    (replaceSquareInRow, formatLine, isMoveInBounds, _RANGE_, _EMPTY_BOARD_, _TIED_BOARD_)
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

formatRows :: Board -> [String]
formatRows  []    = []
formatRows (r:rs) =  formatLine (showSquares r) : formatRows rs

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
-- O (col)
type Col = Int
isColEmpty :: Row -> Col -> Bool
isColEmpty [] _  = False
isColEmpty row col = f row 0
    where f []     _                      = False
          f (r:rs) c | r == E && c == col = True
                     | c < col            = f rs (c+1)
                     | otherwise          = False

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

-- Using List Comprehention the same result
getDiag1''' b = [ b !! l !! c | l <-[0..2], c <- [0..2],  l==c ]

getDiag2''' b = [ b !! l !! c | l <-[0,1,2], c <-[2,1,0], 2==l+c ]

getAllLines :: Board -> [Line]
getAllLines b = concat [b
                       ,transpose b
                       ,[getDiag1 b]
                       ,[getDiag2 b]
                       ]

-- Q#07
-- Patter Matching & List Constructs
putSquare :: Player -> Board -> Move -> Board
putSquare _ [     ]   _    = []
putSquare p [x,y,z] (0, c) = replaceSquareInRow p c x : [y,z]
putSquare p [x,y,z] (1, c) = x : replaceSquareInRow p c y : [z]
putSquare p [x,y,z] (2, c) = [x,y] <>  [replaceSquareInRow p c z]
putSquare _    _    (_, _) = [[]] -- Just in case the board has less than 3 rows

--Recursion
putSquare' :: Player -> Board -> Move -> Board
putSquare' _  []  _     = []
putSquare' p  board (l, c)
    | l < 0 || l > 2       = board
    | otherwise            = next p board (0,c)
 where next p [] (0,0)     = []
       next p (r:b) (i,c)
        | i /= l           = r : next p b (i+1,c)
        | i == l           = replaceSquareInRow p c r : b

-- Q#08

str = [". Learn Haskell", ". Wait for more industry adoption", ". Profit?"]
-- ZipWith
prependRowIndices :: [String] -> [String]
prependRowIndices [] = []
prependRowIndices str = zipWith (\a s -> a : s) ['A'..] str

-- Recursion
prependRowIndices' :: [String] -> [String]
prependRowIndices' [] = []
prependRowIndices' xs = next ('A', xs)
    where next (_, [])   = []
          next (c, s:ss) = [c : s] <> next (succ c, ss)

-- Q#09

isWinningLine :: Player -> Line -> Bool
isWinningLine _ []   = False
isWinningLine p line = all ( == p) line

isWinningLine' :: Player -> Line -> Bool
isWinningLine' _ []   = False
isWinningLine' p line = next False line []
    where next _ []     acc          = and acc
          next b (l:ls) acc | p == l = next b ls (not b : acc)
                            | otherwise = False

-- Q#10

-- Patter Matching & Guards
isValidMove :: Board -> Move -> Bool
isValidMove []            _     = False
isValidMove [l1,l2,l3] mv@(0,c) | isMoveInBounds mv = isColEmpty l1 c | otherwise = False
isValidMove [l1,l2,l3] mv@(1,c) | isMoveInBounds mv = isColEmpty l2 c | otherwise = False
isValidMove [l1,l2,l3] mv@(2,c) | isMoveInBounds mv = isColEmpty l3 c | otherwise = False
isValidMove [l1,l2,l3] mv@(_,_) = False

-- Recursion
isValidMove' :: Board -> Move -> Bool
isValidMove' [] _       = False
isValidMove' bo mv
    | isMoveInBounds mv = matches bo mv 0
    | otherwise = False
 where matches (b:bs) (l,c) i | l == i    = isColEmpty b c
                              | i <  l    = matches bs (l,c) (i+1)
                              | otherwise = False

