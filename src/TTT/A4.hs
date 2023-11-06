module TTT.A4 where

import Data.List (intercalate)
import TTT.A1  
import TTT.A2 
import TTT.A3 ()

-- Q#01

-- map + intercalate
_HEADER_  = ' ' : _SEP_ <> intercalate _SEP_ (map show _RANGE_) <> _SEP_

-- map + foldr
_HEADER_1 = ' ' : foldr (\a b -> a <> b) _SEP_ (map (\x -> _SEP_ <> show x ) _RANGE_)

-- Fuse foldr/map
_HEADER_2 = ' ' : foldr ((\a b -> a <> b) . (\x -> _SEP_ <> show x )) _SEP_ _RANGE_

-- map + foldl
_HEADER_3 = foldl (\b a -> b <> a) (' ': _SEP_) (map (\x -> show x <> _SEP_) _RANGE_)

-- map + concat
_HEADER_4 = concat $ (' ': _SEP_) : map (\x -> show  x <> _SEP_) _RANGE_


-- Q#02

showSquares :: [Square] -> [String]
showSquares = map showSquare -- showSquare => E == "_"


-- Q#03

dropFirstCol :: Board -> Board
dropFirstCol = map (drop 1)


-- Q#04

dropLastCol :: Board -> Board
dropLastCol = map (take $ _SIZE_ -1)


--Q#05

formatRows :: Board -> [String]
formatRows board = map (\x -> formatLine $ showSquares x) board


-- Q#06

-- All
isWinningLine_ :: Square -> Row -> Bool
isWinningLine_ _ [] = False
isWinningLine_ p line = all ( == p) line

-- Filter + null
isWinningLine_' :: Square -> Row -> Bool
isWinningLine_' _ [] = False
isWinningLine_' p line = null $ filter ( /= p) line

-- Q#07

-- Foldr + &&
isWinningLine :: Square -> Line -> Bool
isWinningLine _ []   = False
isWinningLine p line = foldr (\a b -> (p == a) && b) True line

-- List Comprehention
isWinningLine' :: Square -> Line -> Bool
isWinningLine' _ []   = False
isWinningLine' p line = null [ p | sqr <- line, sqr /= p]


-- Q#08

_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]

_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]

hasWon :: Player -> Board -> Bool
hasWon = undefined

-- Q#09

getGameState = undefined

playMove = undefined

-- Q#10

prependRowIndices = undefined

-- Q#11

formatBoard = undefined
