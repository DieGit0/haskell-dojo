module Sandbox_A6 where

import HM.A6
import HM.A7 -- Note: to Display a new custom GameException msg

main = do
    putStrLn "Q#1:"
    print (7        :: Chances)
    print ("String" :: Guess)
    print ('Z'      :: Move) 
    print ("String" :: Secret)
    print (["Word1", "Word2", "Word3"] :: Dictionary)
    newLIne
    putStrLn "Q#2:"
    -- Note: ** New Instance of GameException to be displayed in Q#07 instance Show GameException **
    print InvalidChars
    print InvalidLength
    print NotInDict
    print InvalidMove
    print RepeatMove
    print GameOver
    newLIne
    putStrLn "Q#3:"
    print $ lengthInRange "AB"  -- False
    print $ lengthInRange "ABC" -- True
    print $ lengthInRange ['A'..'T'] -- len 20 True
    print $ lengthInRange ['A'..'U'] -- len 21 False
    newLIne
    putStrLn "Q#4:"
    print $ invalidMove 'A' -- False
    print $ invalidMove 'Z' -- False
    print $ invalidMove 'Ψ' -- False
    print $ invalidMove 'Σ' -- False
    print $ invalidMove 'ث' -- False
    print $ invalidMove 'ش' -- False
    print $ map invalidMove ['0'.. '9'] -- True
    newLIne
    putStrLn "Q#5:"
    print $ revealLetters 'A' "ABC"   "xxx"
    print $ revealLetters 'B' "ABC"   "xxx"
    print $ revealLetters 'C' "ABC"   "xxx"
    print $ revealLetters 'A' "ARARA" "xxx"
    print $ revealLetters 'X' "ABC"   "xxx"
    newLIne
    putStrLn "Q#6:"
    print $ updateChances 'A' "ABACAXI" 7
    print $ updateChances 'Z' "ABACAXI" 7
    newLIne
    putStrLn "Q#7:"
    setSecret