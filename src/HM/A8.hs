module HM.A8 where

import Control.Monad (when)
import Data.Char (toUpper)
import HM.A6
import HM.A7 hiding (validateNoDict, validateWithDict)
import HM.Provided
import System.Directory (doesFileExist)
import Control.Applicative ((<*>))

-- Q#01

-- getChar >>= return . toUpper
-- return . toUpper =<< getChar 
-- do c <- getChar; return $ toUpper c

getUpperChar :: IO Char
getUpperChar = fmap toUpper getChar

-- Q#02

_DICT_ ::IO Dictionary
_DICT_ = do
  fileExists <- doesFileExist _DICT_FILE_
  if fileExists 
    then fmap lines $ readFile _DICT_FILE_
    else pure []

isDictNonEmpty :: IO Bool
isDictNonEmpty = fmap (not . null) _DICT_ -- not . null <$> _DICT_ 

-- Q#03

makeGameIfValid :: Either GameException Secret -> Either GameException Game
makeGameIfValid = fmap makeGame
-- makeGameIfValid game = case game of
--   Left  e -> Left  e
--   Right s -> Right (makeGame s) 


-- Q#04

-- toMaybe        :: Bool -> a -> Maybe a
-- isDictNonEmpty :: IO Bool
-- _DICT_         :: IO Dictionary

getDict  :: IO (Maybe Dictionary)
getDict' :: IO (Maybe Dictionary)
--   f => (a -> b)               -> f  a    -> f b
-- 1 fmap (Bool -> a -> Maybe a) -> IO Bool
-- 2 fmap IO (a -> Maybe a) => f b
-- 3 ???  IO (a -> Maybe a) -> IO Dictionary
--  >>=  X  (Dictionary -> Maybe Dictionary) X  => bind by both sides and apply (a -> m a) to dict inside an Applicative pure
getDict'= toMaybe <$> isDictNonEmpty >>=  \ama -> pure . ama  =<< _DICT_

-- (<*>)  :: Applicative f => 
--       f  (a -> b      ) -> f a           => IO (a -> Maybe a) -> IO Dictionary ->
--       f  (a -> Maybe a) -> f Dictionary  => use a functor applicative to 'extract' Dictionary from IO and apply it to IO (a -> m a) 
-- ==>   f (Dictionary -> Maybe Dictionary) -> f b
-- ==>   IO (Maybe Dictionary)
getDict = toMaybe <$> isDictNonEmpty <*> _DICT_


-- Q#05

validateNoDict :: Secret -> Either GameException Secret
validateNoDict s = hasValidChars s >>= isValidLength

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict dict sec = validateNoDict sec >>= isInDict dict


-- Q#06

playGame :: Game  -> IO ()
playGame game = do promptGuess
                   move <- getUpperChar
                   _SPACE_ 
                   case processTurn move game of
                      Left GameOver  -> print GameOver  >> putStrLn ("The corret word was: " ++ secret game)
                      Left  warning  -> print warning   >> playGame game
                      Right newGame  -> print newGame >> 
                              if  guess newGame == secret newGame
                                  then putStrLn "** [YEAH! YOU WON!!] **\n** [CONGRATULATIONS] **\n"
                                  else playGame newGame

-- Q#07

-- validateNoDict   :: Secret               -> Either GameException Secret
-- validateWithDict :: Dictionary -> Secret -> Either GameException Secret
-- makeGameIfValid  :: Either 
--                       GameException 
--                       Secret 
--                  -> Either GameException Game

startGame :: (Secret -> Either GameException Secret) -> IO ()
startGame validate = do sec <- setSecret 
                        let x = validate sec
                        case makeGameIfValid x of
                          Left  error -> print error >> startGame validate
                          Right game  -> print game  >> playGame game
-- Q#08

runHM :: IO ()
runHM = do
  maybeDict <- getDict 
  case maybeDict of
    Just dict -> startGame $ validateWithDict dict
    Nothing   -> do 
      putStrLn "Missing dictionary! Continue without dictionary? [Y/N]"
      input <- getUpperChar
      when (input == 'Y') $ startGame validateNoDict

  
