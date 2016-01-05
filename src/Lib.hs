{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax   #-}

module Lib where

import           Control.Monad (forever)
import           Data.Char     (toLower, toUpper, isSpace)
import           Data.List     (intercalate)
import           Data.List     (sort)


someFunc ∷ IO ()
someFunc = do
  clearScreen
  putStrLn "Let's get started...\n"

  let maxMissesG = 7

  forever $ do phraseG ← inputWord
               clearScreen
               playWord maxMissesG phraseG


playWord ∷ Int → String → IO ()
playWord maxMissesG phraseG = go $ beginG maxMissesG phraseG
  where go game = do
          clearScreen
          if isFinishedG game
            then putStrLn $ outcomeG game
            else do displayState game
                    g ← inputGuess
                    go $ addGuessG g game

        displayState = putStrLn . intercalate "\n" . render

clearScreen = putStrLn $ replicate 30 '\n'

-----------------------------
-----------------------------

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

addGuessG g game@Game{..} | g `elem` guessesG = game
                        | otherwise        = game { guessesG = g : guessesG }

missesG Game{phraseG,guessesG} = reverse $ filter noMatch guessesG
  where noMatch = not . (`matchesG` phraseG)

isFinishedG game@Game{..} = isGuessedG game || isFailedG game
isGuessedG  game@Game{..} = null $ getUnguessedG game
isFailedG   game@Game{..} = not (isGuessedG game) && n >= maxMissesG
  where n = length (missesG game)

outcomeG game@Game{phraseG} | isGuessedG game = "you won!! " ++ (toUpper <$> phraseG) ++ "\n"
                         | isFailedG  game = "you lost :( the phraseG was " ++ (toUpper <$> phraseG) ++ "\n"

getUnguessedG Game{guessesG,phraseG} = foldr tryGuess phrase' guessesG
  where tryGuess (One c) acc | c `elem` phraseG = filter (/= c) acc
                             | otherwise       = acc
        tryGuess (All s) acc | s == phraseG     = []
                             | otherwise       = acc

        phrase' = filter (/= ' ') phraseG

matchesG (All x) phraseG = x   ==   phraseG
matchesG (One x) phraseG = x `elem` phrase'
  where phrase' = filter (/= ' ') phraseG

------------------------------
------------------------------

inputWord = do
  putStrLn "Enter a word, phrase or sentence:"
  phraseG ← getLine
  case phraseG of "" → inputWord
                  x  → return x

inputGuess = do
  gs ← trim <$> getLine
  case gs of []  → inputGuess
             [c] → return (One c)
             gs  → return (All gs)

-----------------------------

type Image = [[Char]]

render ∷ Game → Image
render game@Game{..} = [renderPhrase, "", renderHangar]
  where
    renderPhrase = renderPos <$> phraseG
    renderPos c | c `elem` guesses' = toUpper c
                | c == ' '          = ' '
                | otherwise         = '_'
    guesses' = concatMap (\case One c → [c]; _ → []) guessesG

    renderHangar = breadcrumbs ++ replicate (0 `max` maxMissesG - n) '-' ++ "|"
    n = length (missesG game)
    breadcrumbs = intercalate "," $ (\case One c → [c]; All s → s) <$> missesG game

------------------------
-----------------------

trim = reverse . drop . reverse . drop
  where drop = dropWhile isSpace
