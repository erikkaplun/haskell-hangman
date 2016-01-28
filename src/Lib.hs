{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax   #-}

module Lib where

import           Intrusive

import           Control.Monad (forever)
import           Data.Char     (isSpace, toLower, toUpper)
import           Data.List     (intercalate)
import           Data.List     (sort)

maxMisses = 7

playGame ∷ IO ()
playGame = do
  clearScreen
  putStrLn "Let's get started...\n"

  forever $ do phrase ← inputPhrase
               clearScreen

               playPhrase maxMisses phrase

playGame' =
  let
    io1 :: () -> IO String
    io1 = \()     -> do clearScreen
                        putStrLn "Let's get started...\n"
                        inputPhrase
    io2 :: String -> IO ()
    io2 = \phrase -> playPhrase maxMisses phrase
  in forever_ (io1 >>> io2)

playGame'' :: IO ()
playGame'' = runIOPipe playGame'

playPhrase ∷ Int → String → IO ()
playPhrase maxMisses phrase = go $ beginG maxMisses phrase
  where
    go game = do
      clearScreen
      if not (isFinishedG game) then do
        displayState game
        guess ← inputGuess
        go $ addGuessG guess game
      else
        putStrLn $ renderOutcome game

    displayState = mapM_ putStrLn . renderState

clearScreen = putStrLn $ replicate 30 '\n'

-----------------------
-- Input & Rendering --
-----------------------

inputPhrase = do
  putStrLn "Enter a word, phrase or sentence:"
  phraseG ← getLine
  case phraseG of "" → inputPhrase
                  x  → return x

inputGuess = do
  gs ← trim <$> getLine
  case gs of []  → inputGuess
             [c] → return (One c)
             gs  → return (All gs)

-----------------------------

type Image = [[Char]]

renderState ∷ Game → Image
renderState game@Game{..} = [renderPhrase, "", renderHangar]
  where
    renderPhrase = renderPos <$> phraseG
    renderPos c | c `elem` guesses' = toUpper c
                | c == ' '          = ' '
                | otherwise         = '_'
    guesses' = concatMap (\case One c → [c]; _ → []) guessesG

    renderHangar = breadcrumbs ++ replicate (0 `max` maxMissesG - n) '-' ++ "|"
    n = length (missesG game)
    breadcrumbs = intercalate "," $ (\case One c → [c]; All s → s) <$> missesG game

renderOutcome ∷ Game → String
renderOutcome game@Game{phraseG}
  | isGuessedG game = "you won! "                    ++ phrase ++ "\n"
  | isFailedG  game = "you lost :( the phraseG was " ++ phrase ++ "\n"
  where phrase = toUpper <$> phraseG

----------------
-- Game Logic --
----------------

data Game = Game { maxMissesG ∷ Int
                 , phraseG    ∷ Phrase
                 , guessesG   ∷ [Guess]  -- semantically, it's a set, but list is more convenient
                 }

type Phrase = String

data Guess = One Char
           | All String
           deriving (Eq, Show)

beginG ∷ Int → String → Game
beginG maxMissesG phraseG = Game maxMissesG phraseG []

addGuessG guess game@Game{..}
  | guess `elem` guessesG = game
  | otherwise             = game { guessesG = guess : guessesG }

missesG Game{phraseG,guessesG} = reverse $ filter noMatch guessesG
  where noMatch = not . (`matchesG` phraseG)

isFinishedG game@Game{..} = isGuessedG game || isFailedG game
isGuessedG  game@Game{..} = null $ getUnguessedG game
isFailedG   game@Game{..} = not (isGuessedG game) && n >= maxMissesG
  where n = length (missesG game)

getUnguessedG Game{guessesG,phraseG} = foldr tryGuess phrase' guessesG
  where tryGuess (One c) acc | c `elem` phraseG = filter (/= c) acc
                             | otherwise       = acc
        tryGuess (All s) acc | s == phraseG     = []
                             | otherwise       = acc

        phrase' = filter (/= ' ') phraseG

matchesG (All x) phraseG = x   ==   phraseG
matchesG (One x) phraseG = x `elem` phrase'
  where phrase' = filter (/= ' ') phraseG

---------------
-- Utilities --
---------------

trim = reverse . drop . reverse . drop
  where drop = dropWhile isSpace
