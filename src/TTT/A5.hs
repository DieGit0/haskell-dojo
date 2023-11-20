module TTT.A5 where

import Control.Monad (when)
import System.Random.Stateful (globalStdGen, uniformM)
import TTT.A1
import TTT.A2
import TTT.A3
import TTT.A4

newLine = putStrLn []

-- Q#01

printBoard :: Board -> IO ()
printBoard = putStr . formatBoard

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/ttt-logo.txt"

printLogo :: IO ()
printLogo = putStrLn =<< readFile _LOGO_PATH_

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Player
firstPlayer = _RANDOM_BOOL_ >>= \bool -> return $ getFirstPlayer bool

firstPlayer' :: IO ()
firstPlayer' = do 
                bool <- _RANDOM_BOOL_
                putStrLn . showSquare $ getFirstPlayer bool

-- Q#04

getMove :: Board -> IO Move
getMove board = do 
    strMove <- getLine
    let move = stringToMove strMove
    if isValidMove board move 
        then  return move
        else  putStrLn "Invalid move! Try again" >> getMove board

-- Q#05

play :: Board -> IO ()
play board = do 
            when _DISPLAY_LOGO_ printLogo
            newLine
            printBoard board
            player <- firstPlayer
            loop player board
    where   loop :: Player -> Board -> IO ()
            loop p b = do
                 newLine
                 putStrLn $ promptPlayer p
                 move     <-getMove      b
                 let (state,newBoard) = playMove p b move
                 checkState state newBoard
                 where checkState st nb 
                        | st == Prg = do
                            putStrLn $ showGameState st
                            printBoard nb 
                            let nP = switchPlayer p
                            loop nP nb
                        | otherwise = do 
                            putStrLn $ showGameState st
                            printBoard nb
                        -- if st == Prg 
                        -- then 
                        --     do
                        --     printBoard nb 
                        --     let newP = switchPlayer p
                        --     loop newP  nb
                        -- else 
                        --     do 
                        --     print st
                        --     printBoard nb

-- Q#06

runTTT :: IO ()
runTTT = play _EMPTY_BOARD_

-- Q#07

printLogoDo :: IO ()
printLogoDo = do
    logo <- readFile _LOGO_PATH_
    putStrLn logo 

-- Q#08

firstPlayerDo :: IO Player
firstPlayerDo = do 
    bool   <- _RANDOM_BOOL_
    return $  getFirstPlayer bool

-- Q#09
-- Just did it with 'do' already :)
getMoveDo :: Board -> IO Move
getMoveDo b = getMove b

-- Q#10
-- Just did it with 'do' already :)
playDo :: Board -> IO ()
playDo b = play b
