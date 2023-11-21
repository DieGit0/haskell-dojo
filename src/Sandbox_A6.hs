module Sandbox_A6 where

import HM.A6

main = do
    putStrLn "Q#1:"
    print (7        :: Chances)
    print ("String" :: Guess)
    print ('Z'      :: Move) 
    print ("String" :: Secret)
    print (["Word1", "Word2", "Word3"] :: Dictionary)
    newLIne
    putStrLn "Q#2:"
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
    print $ invalidMove 'A'
    print $ invalidMove 'Z'
    print $ invalidMove 'Ψ'
    print $ invalidMove 'Σ'
    print $ invalidMove 'ث'
    print $ invalidMove 'ش'
    print $ map invalidMove ['0'.. '9']
    newLIne
    putStrLn "Q#5:"
    print $ revealLetters 'A' "ABC"   "xxx"
    print $ revealLetters 'B' "ABC"   "xxx"
    print $ revealLetters 'C' "ABC"   "xxx"
    print $ revealLetters 'A' "ARARA" "xxx"
    print $ revealLetters 'X' "ABC"   "xxx"
    newLIne
    putStrLn "Q#6:"
    print $ updateChances 'A' "ABACAXI"
    print $ updateChances 'Z' "ABACAXI"
    newLIne
    putStrLn "Q#7:"
    setSecret