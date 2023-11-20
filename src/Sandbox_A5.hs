module Sandbox_A5 where

import TTT.A1
import TTT.A2
import TTT.A3
import TTT.A4
import TTT.A5

--newLine = putStrLn []

main = do
    newLine
    putStrLn "Q#1:"
    printBoard _TIED_BOARD_
    newLine
    printBoard _EMPTY_BOARD_
    newLine
    putStrLn "Q#2:"
    newLine
    printLogo
    newLine
    putStrLn "Q#3:" --
    newLine
    print =<< firstPlayer
    newLine
    putStrLn "Q#4:"
    getMove _EMPTY_BOARD_
    newLine
    putStrLn "Q#5:"
    newLine
    play _EMPTY_BOARD_
    newLine
    putStrLn "Q#6:"
    newLine
    runTTT
    newLine
    putStrLn "Q#7:"
    newLine
    printLogoDo
    newLine
    putStrLn "Q#8:" --
    newLine
    firstPlayerDo >>= print
    newLine
    putStrLn "Q#9:"
    newLine
    getMoveDo _EMPTY_BOARD_
    newLine
    putStrLn "Q#10:"
    newLine
    playDo _EMPTY_BOARD_
    newLine
