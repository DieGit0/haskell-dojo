{-# LANGUAGE TypeSynonymInstances #-}
module HM.A7 where

import HM.A6
import HM.Provided
import Data.Char (isAlpha, toLower, toUpper)
import Data.List (intersperse, sort)
import System.Directory (doesFileExist)

-- Q#01

data Game = Game { secret  :: Secret
                 , guess   :: Guess
                 , move    :: [Move]
                 , chances :: Chances
                 } --deriving Show

-- Q#02

repeatedMove :: Move -> Game -> Bool
repeatedMove m g = m `elem` guess g

-- Q#03

initGuess :: Guess -> Guess
initGuess =  map (const '_')

makeGame :: Secret -> Game
makeGame s = let sec = map toUpper s
                 gss = initGuess   s 
             in  Game sec gss [] _CHANCES_

-- ** Note: Before implementation of: instance Show Game && showGameHelper function **
-- ===============================================================================
--  makeGame "ARARA"
--  updateGame 'A' (makeGame "ARARA")
--  updateGame 'R' $ (makeGame "ARARA")
-- Game {secret = "ARARA", guess = "_____", move = "", chances = 7}
-- Game {secret = "ARARA", guess = "A_A_A", move = "A", chances = 7}
-- Game {secret = "ARARA", guess = "_R_R_", move = "R", chances = 7}

--  makeGame "ARARA"
--  updateGame 'A' (makeGame "ARARA")
--  updateGame 'R' $ Game {secret = "ARARA", guess = "A_A_A", move = "A", chances = 7}
-- Game {secret = "ARARA", guess = "_____", move = "", chances = 7}
-- Game {secret = "ARARA", guess = "A_A_A", move = "A", chances = 7}
-- Game {secret = "ARARA", guess = "ARARA", move = "RA", chances = 7}
-- ===============================================================================

-- Q#04

-- revealLetters :: Move -> Secret -> Guess -> Guess
-- updateChances :: Move -> Secret -> Chances 

updateGame :: Move -> Game -> Game
updateGame mv game = let updtGuess = revealLetters mv s g
                         updtChan  = updateChances mv s c
                     in  game { guess   = updtGuess
                              , move    = mv:m
                              , chances = updtChan
                              }
    where (s, g, m, c) = (secret game, guess game, move game, chances game)

-- Q#05

instance Show Game where
  show :: Game -> String
  show (Game s g m c) = showGameHelper' hideSecret g m c
    where hideSecret  = map (const '*') s

-- showGameHelper :: String -> [Char] -> Int -> String
-- showGameHelper game moves chances =
--   unlines
--     [ _STARS_,
--       "\tSecret Word:\t" ++ intersperse ' ' game ++ "\n",
--       "\tGuessed    :\t" ++ intersperse ' ' (sort moves) ++ "\n",
--       "\tChances    :\t" ++ show chances,
--       _STARS_
--     ]
  
-- Just my personal customization (for while):
showGameHelper' :: Secret -> Guess -> [Move] -> Int -> String
showGameHelper' s g m c =
  unlines
    [ _STARS_,
      "\tSecret Word:\t" ++ intersperse ' ' s        ++ "\n",
      "\tGuessed    :\t" ++ intersperse ' ' g        ++ "\n",
      "\tMoves      :\t" ++ intersperse ' ' (sort m) ++ "\n",
      "\tChances    :\t" ++ show c,
      _STARS_
    ]

-- Q#06

instance Show GameException where
    show :: GameException -> String
    show InvalidChars  = "Invalid Chars: The word must contain only characters"
    show InvalidLength = concat ["Invalid Length: The word must be between ", lb, " and ", ub, " characters"]
      where
        lb = show $ fst _LENGTH_
        ub = show $ snd _LENGTH_
    show NotInDict     = "This word does not exist in the Dictionary"
    show InvalidMove   = "This is a Invalid Move"
    show RepeatMove    = "I noticed you've used this move before, try again"
    show GameOver      = "Sorry! Game Over :/"


-- Q#07

toMaybe :: Bool -> a -> Maybe a
toMaybe bool value | bool      = Just value 
                   | otherwise = Nothing

-- Q#08

validateSecret :: (Secret -> Bool) -> GameException -> Secret -> Either GameException Secret
validateSecret f e s | f s       = Right s
                     | otherwise = Left  e

-- Q#09

hasValidChars :: Secret -> Either GameException Secret
hasValidChars = validateSecret (all isAlpha) InvalidChars


isValidLength :: Secret -> Either GameException Secret
isValidLength = validateSecret lengthInRange InvalidLength

isInDict :: Dictionary -> Secret -> Either GameException Secret
isInDict dx s = validateSecret inDict NotInDict sec
  where sec      = map toLower  s
        inDict _ = any (== sec) dx

_dict_ = ["abacus", "abaft", "abalone", "haskell"]

-- Q#10

validateNoDict :: Secret -> Either GameException Secret
validateNoDict s = case hasValidChars s of
                    Right sec -> isValidLength sec
                    leftError -> leftError


validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict dict sec = case validateNoDict sec of
                             Left  e -> Left e
                             Right s -> isInDict dict s

-- Q#11

processTurn :: Move -> Game -> Either GameException Game
processTurn mv gm | invalidMove  mv    = Left InvalidMove
                  | repeatedMove mv gm = Left RepeatMove
                  | not hasChances     = Left GameOver 
                  | otherwise          = Right newGame
  where newGame    = updateGame mv gm
        hasChances = chances newGame > 0












