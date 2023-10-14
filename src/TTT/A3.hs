module TTT.A3 where

import TTT.A1
import TTT.A2

-- Q#01

showInts :: [Int] -> [String]
showInts []     = []                   -- avaliation direction:  ("1" :    ("2" :    ("3" : []))) Left <- Right
showInts (x:xs) = show x : showInts xs -- In Heap's memory => show x1 : show x2 : show x3 : []

-- Just to my own
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

-- Just to my own
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

isColEmpty = undefined

-- Q#05

dropFirstCol = undefined

dropLastCol = undefined

-- Q#06

getDiag1 = undefined

getDiag2 = undefined

getAllLines = undefined

-- Q#07

putSquare = undefined

-- Q#08

prependRowIndices = undefined

-- Q#09

isWinningLine_ = undefined

-- Q#10

isValidMove = undefined
