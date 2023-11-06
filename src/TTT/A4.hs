module TTT.A4 where

import Data.List (intercalate)
import TTT.A1
import TTT.A2
import TTT.A3 (getAllLines, putSquare)
import TTT.A3 qualified (prependRowIndices)

-- Q#01

-- map + intercalate
_HEADER_  = ' ' : _SEP_ <> intercalate _SEP_ (map show _RANGE_) <> _SEP_

-- map + foldr
_HEADER_1 = ' ' : foldr (\a b -> a <> b) _SEP_ (map (\x -> _SEP_ <> show x ) _RANGE_)
--          ' ' : foldr ((<>) . (\x -> _SEP_ <> show x )) _SEP_ _RANGE_

-- Fuse foldr/map
_HEADER_2 = ' ' : foldr ((\a b -> a <> b) . (\x -> _SEP_ <> show x )) _SEP_ _RANGE_
--          ' ' : foldr ((<>) . (\x -> _SEP_ <> show x )) _SEP_ _RANGE_

-- map + foldl
_HEADER_3 = foldl (\b a -> b <> a) (' ': _SEP_) (map (\x -> show x <> _SEP_) _RANGE_)
--          foldl (<>) (' ': _SEP_) (map (\x -> show x <> _SEP_) _RANGE_)

-- map + concat
_HEADER_4 = concat $ (' ': _SEP_) : map (\x -> show  x <> _SEP_) _RANGE_

-- Q#02

showSquares :: [Square] -> [String]
showSquares = map showSquare -- showSquare => E == "_"

-- Q#03

dropFirstCol :: Board -> Board
dropFirstCol = map (drop 1)          -- <== dropFirstColMap (A3) **

-- Q#04

dropLastCol :: Board -> Board
dropLastCol = map (take $ _SIZE_ -1) -- <== dropLastColMap (A3)  **

--Q#05

formatRows :: Board -> [String]
formatRows = map (formatLine . showSquares)

-- Q#06

-- All
isWinningLine_ :: Square -> Row -> Bool
isWinningLine_ _ []   = False
isWinningLine_ p line = all ( == p) line

-- Filter + null
isWinningLine_' :: Square -> Row -> Bool
isWinningLine_' _ []   = False
isWinningLine_' p line = null $ filter ( /= p) line -- not (any ( /= p) line)

-- any + not
isWinningLine_'' :: Square -> Row -> Bool
isWinningLine_'' _ []   = False
isWinningLine_'' p line = not (any ( /= p) line)

-- Q#07

-- Foldr + &&
isWinningLine :: Square -> Line -> Bool
isWinningLine _ []   = False
isWinningLine p line = foldr (\a b -> p == a && b) True line

-- List Comprehention
isWinningLine' :: Square -> Line -> Bool
isWinningLine' _ []   = False
isWinningLine' p line = null [ p | sqr <- line, sqr /= p]

-- Q#08

-- any | elem | comprehention | foldr
hasWon :: Player -> Board -> Bool
hasWon p []    = False
-- hasWon p board = any (== [p,p,p]) $ getAllLines board
-- hasWon p board = [p,p,p] `elem` getAllLines board
-- hasWon p board = or [True | line <- getAllLines board , [p,p,p] == line]
hasWon p board = foldr (\line acc -> [p,p,p] == line || acc) False (getAllLines board)

-- Q#09

-- Guards
getGameState :: Board -> GameState
getGameState board | hasWon X board = XWon
                   | hasWon O board = OWon
                   | isTied   board = Tie
                   | otherwise      = Prg

-- Case
getGameState' :: Board -> GameState
getGameState' board = 
    case hasWon X board of 
        True  -> XWon
        False -> case hasWon O board of
                    True -> OWon
                    False -> case isTied board of
                                True  -> Tie
                                False -> Prg

_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]

_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]
                
t = _TIED_BOARD_
x = _X_WIN_
o = _O_WIN_

playMove :: Player -> Board -> Move -> (GameState, Board)
playMove p b m = let newboard = putSquare p b m
                 in  (getGameState newboard, newboard)

_TEST_ = [ [E, E, E]
         , [E, E, E]
         , [E, E, E]]

-- Q#10
prependRowIndices :: [String] -> [String]
prependRowIndices =  TTT.A3.prependRowIndices  -- ** As the same of one implementation in (A3) before
--                   zipWith (\c s -> c : s) ['A'..] 

-- Q#11
formatBoard :: Board -> String
formatBoard =  unlines . (_HEADER_ :) . prependRowIndices . formatRows 
--             unlines . (:) _HEADER_ . prependRowIndices . formatRows
--             unlines . (_HEADER_ :) . prependRowIndices $ formatRows _EMPTY_BOARD_  
