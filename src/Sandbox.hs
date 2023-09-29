module Sandbox where

import TTT.A1

main = do
        putStrLn "Q#1:"
        print _SIZE_ -- 3
        --print (:t _SIZE_)
        let s = _SIZE_
        print $ s + 1 -- 4
        print $ s - 1 -- 2
        print s       -- 3

        putStrLn "\nQ#2:"
        let d = _DISPLAY_LOGO_
        print d             -- True
        print $ not d       -- False
        print $ d && not d  -- False
        print $ d || not d  -- True

        putStrLn "\nQ#3:"
        let f = convertRowIndex
        print $ f 'A'      -- 0
        print $ f 'a'      -- 0
        print $ f 'b'      -- 1
        print $ f 'B' - 1  -- 0

        putStrLn "\nQ#4:"
        let m = _INVALID_MOVE_
        print $ fst m -- -1
        print $ snd m -- -1

        putStrLn "\nQ#5:"
        print $ head _SEP_ -- '_'
        print $ last _SEP_ -- '_'
        print $ _SEP_ !! 1 -- '|'

        putStrLn "\nQ#6:"

        print (X :: Square)
--        print (:t X) -- X :: Square
        print (O :: Square)
--        print (:t O) -- O :: Square
        print (X == O) -- False

        putStrLn "\nQ#7:"
        print XWon
        print OWon
        print Tie
        print Prg
        print (XWon == XWon)
        print (XWon == OWon)
        print (Tie  == Prg )

        putStrLn "\nQ#8:"
        print (X :: Player)
        print (O :: Player)
        print ( [X,O,X,O] :: Row)    -- = [Square]
        print ( [X,O,X,O] :: Line)   -- = [Square]
        print ([[X,O,X,O]
               ,[O,X,O,X]
               ,[X,O,X,O]] ::Board)  -- = [Row]
        print ((10,6) :: Move)       -- = (Int, Int)

        putStrLn "\nQ#9:"
        print $ getFirstPlayer True   -- X
        print $ getFirstPlayer False  -- O
        print $ getFirstPlayer_ True  -- X
        print $ getFirstPlayer_ False -- O

        putStrLn "\nQ#10:"
        putStrLn $ showGameState XWon
        putStrLn $ showGameState OWon
        putStrLn $ showGameState Tie
        putStrLn $ showGameState Prg

        putStrLn "\nQ#11:"

        putStrLn "\nQ#12:"


