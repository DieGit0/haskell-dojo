module Sandbox_A3 where

import TTT.A1
import TTT.A2
import TTT.A3
import GHC.Natural (naturalFromInteger)

newLine = putStrLn []

main = do
    putStrLn "Q#1:"
    print $ showInts [1,2,3] -- Ok
    print $ showInts []      -- Ok
    print   _HEADER_         -- Ok
    newLine
    putStrLn "Q#2:"
    print $ showSquares [X,O,X] -- Ok
    print $ showSquares []      -- Ok
    newLine
    putStrLn "Q#3:"
    print $ formatRows _EMPTY_BOARD_ -- Ok
    print $ formatRows _TIED_BOARD_  -- Ok
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
    print $ dropFirstCol    t -- [[O,O],[X,X],[X,O]] -- Ok
    print $ dropFirstColMap t -- [[O,O],[X,X],[X,O]] -- Ok
    newLine
    print $ dropLastCol     t -- [[X,O],[O,X],[O,X]] -- Ok
    print $ dropLastColMap  t -- [[X,O],[O,X],[O,X]] -- Ok
    newLine
    putStrLn "Q#6:"
    let tb = _TIED_BOARD_
    print   tb
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
    print $ getAllLines _TIED_BOARD_ -- [[X,O,O],[O,X,X],[O,X,O],[X,O,O],[O,X,X],[O,X,O],[X,X,O],[O,X,O]]
    newLine
    putStrLn "Q#7:"
    print $ putSquare  X _EMPTY_BOARD_ (1,1) -- Ok
    newLine
    print $ putSquare' X _EMPTY_BOARD_ (1,1) -- Ok
    newLine
    print $ putSquare  X [] (0,0) -- Ok
    print $ putSquare' X [] (0,0) -- Ok
    newLine
    print $ putSquare  X _EMPTY_BOARD_ (0,0) -- Ok 
    print $ putSquare' X _EMPTY_BOARD_ (0,0) -- Ok
    print $ putSquare  X _EMPTY_BOARD_ (0,1) -- Ok
    print $ putSquare' X _EMPTY_BOARD_ (0,1) -- Ok
    print $ putSquare  X _EMPTY_BOARD_ (2,2) -- Ok
    print $ putSquare' X _EMPTY_BOARD_ (2,2) -- Ok
    print $  map (\l -> putSquare   X _EMPTY_BOARD_ (l,0)) [0..2] -- Ok
    print $  map (\l -> putSquare   X _EMPTY_BOARD_ (l,0)) [0..2] -- Ok
    print $  map (\l -> putSquare   X _EMPTY_BOARD_ (l,0)) [0..2] -- Ok
    newLine
    putStrLn "Q#8:"
    let str = [". Learn Haskell", ". Wait for more industry adoption", ". Profit?"]
    print $ prependRowIndices  str -- Ok
    newLine
    print $ prependRowIndices' str -- Ok
    newLine
    putStrLn "Q#9:"
    print $ isWinningLine X [X,X,X] -- True   Ok
    print $ isWinningLine X [O,O,O] -- False  Ok
    print $ isWinningLine O [O,O,X] -- False  Ok
    print $ isWinningLine X []      -- False  Ok
    newLine
    putStrLn "Q#10:"
    let e = _EMPTY_BOARD_
    print $ isValidMove' e (0,0)  -- True  Ok
    print $ isValidMove' e (3,3)  -- False Ok
    print $ isValidMove' [] (0,0) -- False Ok
    print $ isValidMove' _TIED_BOARD_ (1,1) -- False Ok
    newLine
    print $ isValidMove e (0,0)  -- True  Ok
    print $ isValidMove e (3,3)  -- False Ok
    print $ isValidMove [] (0,0) -- False Ok
    print $ isValidMove _TIED_BOARD_ (1,1) -- False Ok
    newLine


