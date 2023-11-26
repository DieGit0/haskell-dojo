module Sandbox_A8 where

import HM.A8
import HM.A7 hiding (validateNoDict, validateWithDict)
import HM.A6
import HM.Provided

newLine = putStrLn []

main = do
    putStrLn "Q#1:"
    newLine

    getUpperChar >>= \x -> putStrLn $ " => " ++ [x]

    newLine
    putStrLn "Q#2:"
    newLine

    dict <- _DICT_
    print $ take 10 dict ++ drop (length dict - 10) dict  

    newLine

    print =<< isDictNonEmpty

    newLine
    putStrLn "Q#3:"
    newLine

    print $ makeGameIfValid (Right "Segredo")
    print $ makeGameIfValid (Left GameOver)

    newLine
    putStrLn "Q#4:"
    newLine
    
    mbDict <- getDict
    let head'= maybe ["Nothing"] (take 10) mbDict 
        feet = drop (length dict - 10) dict  
    print $ concat [head',feet]
 
    newLine
    putStrLn "Q#5:"
    newLine
 
    print $ validateNoDict "AB"
    print $ validateNoDict "ABC3"
    print $ validateNoDict "ABC"
    newLine
    dict <- _DICT_
    print $ validateWithDict dict "AB"
    print $ validateWithDict dict "ABC3"
    print $ validateWithDict dict "zooplankton"

    newLine
    putStrLn "Q#6:"
    newLine

    let game = makeGame "Coffe"
    playGame game

    newLine
    putStrLn "Q#7:"
    newLine

    startGame validateNoDict

    dict <- _DICT_
    startGame (validateWithDict dict)

    newLine
    putStrLn "Q#8:"

    putStrLn "Run the Main.hs"
