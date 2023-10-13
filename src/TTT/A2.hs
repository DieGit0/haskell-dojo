{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal pattern" #-}
module TTT.A2 where

import Data.List (intercalate)
import TTT.A1
import Data.Char (isAlpha)

-- Q#01
promptPlayer :: Player -> String
promptPlayer p = let player = showSquare p
                     msg    = concat  ["Player ", player, "'s turn: enter a row and column position (ex. A1)"]
                 in  msg

-- Q#02
_RANGE_ :: [Int]
_RANGE_ = [0.. _SIZE_ - 1]

-- Q#03
isDigit :: Char -> Bool
isDigit c = let n = ['0'..'9']
            in  c `elem` n

readDigit :: Char -> Int
readDigit c | isDigit c = read [c]
            | otherwise = -1

-- Q#04
_EMPTY_ROW_ :: [Square]
_EMPTY_ROW_ = replicate _SIZE_ E -- Empty

_EMPTY_BOARD_ :: [[Square]]
_EMPTY_BOARD_ = [_EMPTY_ROW_]

-- Q#05
isTied :: Board -> Bool
isTied board | E `notElem` row = True
             | otherwise       = False
    where row = concat board

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]
  ]

-- Q#06
indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings ls = zip ['A'..] ls

-- Q#07
formatLine :: [String] -> String
formatLine str = let lineBoard = intercalate _SEP_ str
                 in  _SEP_ <> lineBoard <> _SEP_

-- Q#08
isMoveInBounds :: Move -> Bool
isMoveInBounds (r,c) = (r >= 0 && c >= 0) && (r <= _SIZE_ && c <= _SIZE_)

-- Q#09
stringToMove :: String -> Move
stringToMove []                       = _INVALID_MOVE_
stringToMove [x]                      = _INVALID_MOVE_
stringToMove (c:d:xs) | not (null xs) = _INVALID_MOVE_
                      | not validStr  = _INVALID_MOVE_
                      | otherwise     = (convertRowIndex c, readDigit d)

  where validStr = isAlpha c && isDigit d

-- Q#10
type Col = Int
replaceSquareInRow :: Player -> Col -> Row -> Row
replaceSquareInRow _   _  []       = []
replaceSquareInRow _   _  [x]      = []
replaceSquareInRow _   _  (_:_:[]) = [] -- *List that has less than 3 elements
replaceSquareInRow p   0  r = let (a,b) = splitAt 0 r in (p : tail b)
replaceSquareInRow p   1  r = let (a,b) = splitAt 1 r in (a <> [p] <> [last b]) -- (drop 1 b)
replaceSquareInRow p   2  r = let (a,b) = splitAt 2 r in (a <> [p] )
replaceSquareInRow _   _  r = r
  -- where newRow = splitAt 1 r in (a <> [p] <> [last b]) 
-- replaceSquareInRow p c r | c >= 0 = let (a,b)  = splitAt c r
--                                         newRow = (a <> [p] <> [last b]) -- Note: The Row should have 3 values * tail b*
--                                     --  (x:y:z:_) = newRow
--                                         [x,y,z,_] = newRow
--                                     in  [x,y,z]
--                          | otherwise = r

rsX :: Col -> Row -> Row
rsX = replaceSquareInRow X

rsO :: Col -> Row -> Row
rsO = replaceSquareInRow O