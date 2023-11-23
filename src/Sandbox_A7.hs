module Sandbox_A7 where

import HM.A7
import HM.A6
import HM.Provided
import Data.IntMap (update)

newLine = putStrLn []

main = do
    putStrLn "Q#1:"
    newLine

    let game = Game "MySecret" "MyGuess" ['X','Y', 'Z'] _CHANCES_
    putStrLn $ secret  game
    putStrLn $ guess   game
    putStrLn $ move    game
    print    $ chances game
 
    newLine
    putStrLn "Q#2:"
    newLine

    print $ repeatedMove 'A' game
    print $ repeatedMove 'G' game

    newLine
    putStrLn "Q#3:"

    putStrLn "\n** Note : See the source code **\n" 

    putStrLn "Q#4:"
    newLine

    putStrLn "[Update Game:]"

    let newGame = makeGame "ARARA"
    print newGame 
    print $ updateGame 'A' newGame --7
    print $ updateGame 'B' $ updateGame 'A' newGame -- 6
    print $ updateGame 'C' $ updateGame 'B' $ updateGame 'A' newGame -- 5
    print $ updateGame 'D' $ updateGame 'C' $ updateGame 'B' $ updateGame 'A' newGame -- 4
    print $ updateGame 'E' $ updateGame 'D' $ updateGame 'C' $ updateGame 'B' $ updateGame 'A' newGame -- 3
    print $ updateGame 'F' $ updateGame 'E' $ updateGame 'D' $ updateGame 'C' $ updateGame 'B' $ updateGame 'A' newGame -- 2
    print $ updateGame 'G' $ updateGame 'F' $ updateGame 'E' $ updateGame 'D' $ updateGame 'C' $ updateGame 'B' $ updateGame 'A' newGame -- 1
    print $ updateGame 'H' $ updateGame 'G' $ updateGame 'F' $ updateGame 'E' $ updateGame 'D' $ updateGame 'C' $ updateGame 'B' $ updateGame 'A' newGame -- 0
    print $ updateGame 'I' $ updateGame 'H' $ updateGame 'G' $ updateGame 'F' $ updateGame 'E' $ updateGame 'D' $ updateGame 'C' $ updateGame 'B' $ updateGame 'A' newGame -- (-1)

    putStrLn "[Update Game Win:]"
    print $ updateGame 'R' $ updateGame 'A' newGame
 
    newLine
    putStrLn "Q#5:"
    newLine

    let game = makeGame "ARARA"
    print game

    newLine
    putStrLn "Q#6:"
    newLine

    print InvalidChars
    print InvalidLength
    print NotInDict
    print InvalidMove
    print RepeatMove
    print GameOver
  
    newLine
    putStrLn "Q#7:"
    newLine

    print $ toMaybe True  "[Somenting]"
    print $ toMaybe False "[ Nothing ] "

    newLine
    putStrLn "Q#8:"

    let _dict_ = ["lemonad", "caffe", "haskell"]

    print $ map hasValidChars     ["", "_", "S3cr37", ['1'..'5'], "Secret"]
    newLine
    print $ map isValidLength     ["", "AB", "ABC", ['A'..'T'], ['A'..'U']]
    newLine
    print $ map (isInDict _dict_) ["XYZ", "hAskELL", "caFFe", "leMonad"]
    newLIne

    putStrLn "Q#9:"
    newLine
    putStrLn "validateNoDict:"
    print $ validateNoDict "_"
    print $ validateNoDict "3"
    print $ validateNoDict "*"
    print $ validateNoDict ""
    print $ validateNoDict "S"
    print $ validateNoDict "Se"
    print $ validateNoDict "ABCDEABCDEABCDEABCDEA" -- 21 Chars
    print $ validateNoDict "Sec"
    print $ validateNoDict "ABCDEABCDEABCDEABCDE"  -- 20 Chars
    newLine

    putStrLn "Q#10:"
    newLine
    putStrLn "validateWithDict:"
    print $ validateWithDict _dict_ "_"
    print $ validateWithDict _dict_ "3"
    print $ validateWithDict _dict_ "*"
    print $ validateWithDict _dict_ ""
    print $ validateWithDict _dict_ "S"
    print $ validateWithDict _dict_ "Se"
    print $ validateWithDict _dict_ "ABCDEABCDEABCDEABCDEA" -- 21 Chars
    print $ validateWithDict _dict_ "Somenthing..."
    print $ validateWithDict _dict_ "else"
    print $ validateWithDict _dict_ "HASKELL"
    print $ validateWithDict _dict_ "CAFFE"
    print $ validateWithDict _dict_ "LeMonad"

    newLine
    putStrLn "Q#11:"
    newLine

    putStrLn "[Process Turn: Game Over]"

    print $ processTurn 'A' newGame -- 7
    print $ processTurn 'B' =<< processTurn 'A' newGame -- 6
    print $ processTurn 'C' =<< processTurn 'B' =<< processTurn 'A' newGame -- 5
    print $ processTurn 'D' =<< processTurn 'C' =<< processTurn 'B' =<< processTurn 'A' newGame -- 4
    print $ processTurn 'E' =<< processTurn 'D' =<< processTurn 'C' =<< processTurn 'B' =<< processTurn 'A' newGame -- 3
    print $ processTurn 'F' =<< processTurn 'E' =<< processTurn 'D' =<< processTurn 'C' =<< processTurn 'B' =<< processTurn 'A' newGame -- 2
    print $ processTurn 'G' =<< processTurn 'F' =<< processTurn 'E' =<< processTurn 'D' =<< processTurn 'C' =<< processTurn 'B' =<< processTurn 'A' newGame  -- 1
    print $ processTurn 'H' =<< processTurn 'G' =<< processTurn 'F' =<< processTurn 'E' =<< processTurn 'D' =<< processTurn 'C' =<< processTurn 'B' =<< processTurn 'A' newGame -- 0

    newLine
    putStrLn "[Process Turn: Win]"

    print $ processTurn 'R' =<< processTurn 'A' newGame