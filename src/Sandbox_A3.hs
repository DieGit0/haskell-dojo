module Sandbox_A3 where

import TTT.A1
import TTT.A2
import TTT.A3

newLine = putStrLn []

main = do
    putStrLn "Q#1:"
    print $ showInts [1,2,3] -- Ok
    print $ showInts []      -- Ok
    print   _HEADER_         -- Ok
    newLine
    putStrLn "Q#2:"
    print $ showSquares [X,O,X]
    print $ showSquares []
    newLine
    putStrLn "Q#3:"
    print $ formatRows _EMPTY_BOARD_
    print $ formatRows _TIED_BOARD_
    newLine
    putStrLn "Q#4 :"
    let e = head _EMPTY_BOARD_
    print $ isColEmpty' e  2  -- True
    print $ isColEmpty''(last _TIED_BOARD_) 1 -- False
    print $ isColEmpty3 [] 0 -- False
    print $ isColEmpty  e  3 -- False
    newLine
    putStrLn "Q#5:"
    let t = _TIED_BOARD_
    print $ dropFirstCol    t -- [[O,O],[X,X],[X,O]]
    print $ dropFirstColMap t -- [[O,O],[X,X],[X,O]]
    newLine
    print $ dropLastCol     t -- [[X,O],[O,X],[O,X]]
    print $ dropLastColMap  t -- [[X,O],[O,X],[O,X]]
    newLine
    putStrLn "Q#6:"
    let tb = _TIED_BOARD_
    print $ tb
    print $ getDiag1    _TIED_BOARD_ -- [X,X,O]
    print $ getDiag1'   _TIED_BOARD_ -- [X,X,O]
    print $ getDiag1''  _TIED_BOARD_ -- [X,X,O]
    print $ getDiag1''' _TIED_BOARD_
    newLine
    print $ getDiag2    _TIED_BOARD_ -- [O,X,O]
    print $ getDiag2'   _TIED_BOARD_ -- [O,X,O]
    print $ getDiag2''  _TIED_BOARD_ -- [O,X,O]
    print $ getDiag2''' _TIED_BOARD_ -- [X,X,O]
    newLine
    print $ getAllLines _TIED_BOARD_
    newLine
    putStrLn "Q#7:"