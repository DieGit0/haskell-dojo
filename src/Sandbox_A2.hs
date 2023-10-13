module Sandbox_A2 where

-- Note: cabal repl from the root dir && :l source code

import TTT.A1
import TTT.A2

newLine = putStrLn []
main = do
        putStrLn "Q#1:"
        putStrLn $ promptPlayer X
        putStrLn $ promptPlayer O
        putStrLn $ promptPlayer E
        newLine
        putStrLn "Q#2:"
        print    $ _RANGE_ == [0, 1, 2]
        print    $ _RANGE_ == [0..3]
        newLine
        putStrLn "Q#3:"
        -- c <- getChar
        print    $ readDigit '3'
        print    $ readDigit '9'
        print    $ readDigit '$'
        newLine
        putStrLn "Q#4:"
        print      _EMPTY_ROW_
        print      _EMPTY_BOARD_
        print    $ elem X _EMPTY_ROW_
        print    $ elem O _EMPTY_ROW_
        print    $ all (== _EMPTY_ROW_) _EMPTY_BOARD_
        newLine
        putStrLn "Q#5:"
        print      _EMPTY_BOARD_
        print      _TIED_BOARD_
        print    $ isTied _EMPTY_BOARD_
        print    $ isTied _TIED_BOARD_
        newLine
        putStrLn "Q#6:"
        print    $ indexRowStrings ["Learn Haskell", "Wait for more industry adoption", "Profit?"]
        newLine
        putStrLn "Q#7:"
        print    $ formatLine ["_X_", "_O_", "_X_"]
        newLine
        putStrLn "Q#8:"
        print    $ isMoveInBounds (0, 0)   -- True
        print    $ isMoveInBounds (2, 2)   -- True
        print    $ isMoveInBounds (3, 3)   -- False
        print    $ isMoveInBounds (-1, -1) -- False
        newLine
        putStrLn "Q#9:"
        print    $ stringToMove ""    -- False
        print    $ stringToMove "B1"  -- True
        print    $ stringToMove "B11" -- False
        print    $ stringToMove "a3"  -- True
        print    $ stringToMove "3A"  -- False
        newLine
        putStrLn "Q#10:"
        let e = head _EMPTY_BOARD_
            t = last _TIED_BOARD_
        print e
        print t
        newLine
        print $ rsX 0 e -- [X,E,E]
        print $ rsO 1 t -- [O,O,O]
        print $ rsX 3 e -- [E,E,E]
        print $ rsO (-1) t -- [O,X,O]
        print $ rsX 0 [] --[]
        newLine
        print $ map (\n -> rsX (n) e) [(-3),(-2)..3] -- OK
        newLine
        print $ map (\n -> rsO (n) e) [(-3),(-2)..3] -- OK
        newLine
        print $ map (\n -> rsX (n) t) [(-3),(-2)..3] -- Ok
        newLine
        print $ map (\n -> rsO (n) t) [(-3),(-2)..3] -- Ok





