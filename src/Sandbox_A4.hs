module Sandbox_A4 where

import TTT.A1
import TTT.A2
import TTT.A3 ()
import TTT.A4

newLine = putStrLn []

main = do
    putStrLn "Q#1:"
    newLine
    print _HEADER_  -- map + intercalate
    print _HEADER_1 -- map + foldr
    print _HEADER_2 -- Fuse foldr/map
    print _HEADER_3 -- map + foldl
    print _HEADER_4 -- map + concat 
    newLine
    putStrLn "Q#2:"
    newLine
    print $ showSquares [X,O,X] 
    print $ showSquares [X,O,X] == ["X","O","X"]
    print $ showSquares []
    print $ showSquares []      == []
    newLine
    putStrLn "Q#3:"
    newLine
    let t = _TIED_BOARD_
    print $ dropFirstCol t
    print $ dropFirstCol t == [[O,O],[X,X],[X,O]]
    newLine
    putStrLn "Q#4:"
    newLine
    print $ dropLastCol  t 
    print $ dropLastCol  t == [[X,O],[O,X],[O,X]]
    newLine
    putStrLn "Q#5:"
    newLine
    print $ formatRows _EMPTY_BOARD_
    print $ formatRows _EMPTY_BOARD_ == ["_|___|___|___|_","_|___|___|___|_","_|___|___|___|_"]
    newLine
    print $ formatRows _TIED_BOARD_
    print $ formatRows _TIED_BOARD_  == ["_|_X_|_O_|_O_|_","_|_O_|_X_|_X_|_","_|_O_|_X_|_O_|_"]
    newLine
    putStrLn "Q#6:"
    newLine
    -- All
    print $ isWinningLine_ X [X,X,X]
    print $ isWinningLine_ X [O,O,O]
    print $ isWinningLine_ O [O,O,X]
    print $ isWinningLine_ X []
    newLine
    -- Filter
    print $ isWinningLine_' X [X,X,X]
    print $ isWinningLine_' X [O,O,O]
    print $ isWinningLine_' O [O,O,X]
    print $ isWinningLine_' X []
    newLine
    -- any + not
    print $ isWinningLine_'' X [X,X,X]
    print $ isWinningLine_'' X [O,O,O]
    print $ isWinningLine_'' O [O,O,X]
    print $ isWinningLine_'' X []
    newLine 
    putStrLn "Q#7:"
    newLine
    -- Foldr
    print $ isWinningLine X [X,X,X]
    print $ isWinningLine X [O,O,O]
    print $ isWinningLine O [O,O,X]
    print $ isWinningLine X []
    newLine
    -- List Comprehention
    print $ isWinningLine' X [X,X,X]
    print $ isWinningLine' X [O,O,O]
    print $ isWinningLine' O [O,O,X]
    print $ isWinningLine' X []
    newLine
    putStrLn "Q#8:"
    newLine
    let _X_WIN_ = [ [X, O, O]
                  , [O, X, O]
                  , [O, O, X]
                  ]

    let _O_WIN_ = [ [O, X, O]
                  , [X, X, O]
                  , [X, O, O]
                  ]
                  
    let t = _TIED_BOARD_
        x = _X_WIN_
        o = _O_WIN_
    print $ hasWon X t  -- False
    print $ hasWon O t  -- False
    print $ hasWon X x  -- True
    print $ hasWon O x  -- False
    print $ hasWon O o  -- True
    print $ hasWon X o  -- False
    print $ hasWon X [] -- False
    newLine
    putStrLn "Q#9:"
    newLine
    print $ getGameState _X_WIN_
    print $ getGameState _O_WIN_
    print $ getGameState _TIED_BOARD_
    print $ getGameState _EMPTY_BOARD_
    newLine
    let _TEST_ = [ [E, E, E]
                 , [E, E, E]
                 , [E, E, E]
                 ]
    putStrLn "Line:"
    let (g1, x1) = playMove X _TEST_ (0,0)
        (g2, x2) = playMove X  x1    (0,1)
        (g3, x3) = playMove X  x2    (0,2)
    print g3 -- XWon
    print x3 -- [[X,X,X],[E,E,E],[E,E,E]]
    newLine
    putStrLn "Diagonal1:"
    let (g1, x1) = playMove X _TEST_ (0,0)
        (g2, x2) = playMove X  x1    (1,1)
        (g3, x3) = playMove X  x2    (2,2)
    print g3 -- XWon
    print x3 -- [[X,E,E],[E,X,E],[E,E,X]]
    newLine
    putStrLn "Diagonal2:"
    let (g1, x1) = playMove X _TEST_ (0,2)
        (g2, x2) = playMove X  x1    (1,1)
        (g3, x3) = playMove X  x2    (2,0)
    print g3 -- XWon
    print x3 -- [[E,E,X],[E,X,E],[X,E,E]]
    newLine
    putStrLn "Q#10:"
    newLine
    putStr $ formatBoard _TIED_BOARD_
    newLine
    putStr $ formatBoard _EMPTY_BOARD_
    newLine

