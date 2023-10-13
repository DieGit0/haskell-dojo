module TTT.A1 where

import Data.Char (toUpper)

-- Q#01
_SIZE_  :: Int
_SIZE_ = 3

-- Q#02
_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

-- Q#03
convertRowIndex :: Char -> Int
convertRowIndex c = fromEnum (toUpper c) - 65
--convertRowIndex = (-65 +) . fromEnum . toUpper

-- Q#04
_INVALID_MOVE_ :: (Int,Int)
_INVALID_MOVE_ = (-1,-1)

-- Q#05
_SEP_ :: String
_SEP_ = "_|_"

-- Q#06
data Square = X | O | E -- deriving (Show, Eq)

instance Show Square where
  show X = "X"
  show O = "O"
  show E = "E"

instance Eq Square where
  (==) X X = True
  (==) O O = True
  (==) E E = True
  (==) E _ = False
  (==) O _ = False
  (==) X _ = False

-- Q#07

data GameState =  XWon | -- "X Won the Game" |
                  OWon | -- "O won the game" |
                  Tie  | -- "Game is a tie"  |
                  Prg    -- "The game is in progress"
                  deriving (Show, Eq)

-- Q#08
--Synonym	Existing Type/Synonym
type Player = Square
type Row    = [Square]
type Line   = [Square]
type Board  = [Row]
type Move   = (Int, Int)

-- Q#09
getFirstPlayer :: Bool -> Square
getFirstPlayer bool = if bool 
                       then X 
                       else O

getFirstPlayer_ :: Bool -> Square
getFirstPlayer_ bool | bool      = X
                     | otherwise = O 

-- Q#10
showGameState :: GameState -> String
showGameState gst = case gst of 
                    XWon -> "X Won the Game" 
                    OWon -> "O won the game"
                    Tie  -> "Game is a tie"
                    Prg  -> "The game is in progress"

-- Q#11
switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X
switchPlayer _ = E

-- Q#12
showSquare :: Square -> String
showSquare X     = "X" 
showSquare O     = "O" 
showSquare E = "_" 