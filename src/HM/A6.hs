{-# LANGUAGE BlockArguments #-}
module HM.A6 where

import Data.Char     (isAlpha)
import Control.Monad (when)
import HM.Provided

newLIne = putStrLn []

-- Q#01

-- Synonym	Existing Type/Synonym
type Chances    = Int
type Guess      = String
type Move       = Char
type Secret     = String
type Dictionary = [String]

-- Q#02
--a custom [variant] type called GameException...
data GameException = 
      InvalidChars
    | InvalidLength
    | NotInDict
    | InvalidMove
    | RepeatMove
    | GameOver
    -- deriving Show

-- Q#03

lengthInRange :: Secret -> Bool
lengthInRange s = let (min,max) = _LENGTH_
                      sLen = length s
                  in  sLen >= min && sLen <= max

-- Q#04

invalidMove :: Move -> Bool
invalidMove = not . isAlpha

-- Q#05 

revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters _ [] _ = []
revealLetters _ _ [] = []
revealLetters m s g | validMove   = "Invalid Move [A-Z]"
                    | not inRange = "Invalid Range [3-20]" 
                    | otherwise   =  showLetter
    where showLetter  
                    | moveMatch  = zipWith (\a b -> if a == m then a else b) s g
                    | otherwise  = g
          moveMatch              = m `elem` s
          (validMove, inRange) = (invalidMove m , lengthInRange s)

-- Q#06

updateChances :: Move -> Secret -> Chances -> Chances
updateChances m s c = let exist = m `elem` s
                      in
                      if exist 
                      then c
                      else c-1

updateChances' :: Move -> Secret -> Chances -> Chances
updateChances' m s c | exist     = c
                     | otherwise = c-1
        where exist = m `elem` s

-- Q#07

setSecret :: IO String
setSecret = do  
    showInput False
    putStr "Enter a secret word:\t" 
    sec <- getLine
    showInput True
    _SPACE_ 
    return sec
