{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax   #-}

module Lib where

import           Control.Monad (forever)
import           Data.Char     (toLower, toUpper)
import           Data.List     (intercalate)
import           Data.List     (sort)
import           Data.Set

someFunc ∷ IO ()
someFunc = do
  clearScreen

  putStrLn "Let's get started..."
  putStrLn ""

  let maxN = 7

  forever $ do phrase <- getWord
               clearScreen
               play maxN phrase

data Guess = One Char | All String deriving Eq
type Phrase = String

data State = State { maxN    :: Int
                   , phrase  :: Phrase
                   , guesses :: [Guess] }

wrongGuesses State{phrase,guesses} = Prelude.filter (not . (`match` phrase)) guesses

guess g st@State{..} | g `elem` guesses = st
                     | otherwise        = st { guesses = g:guesses }

match (All x) phrase = x   ==   phrase
match (One x) phrase = x `elem` phraseNoSpaces
  where phraseNoSpaces = Prelude.filter (/= ' ') phrase

begin ∷ Int → String → State
begin maxN phrase = State maxN phrase []

finished st@State{..} = guessed st || failed st
guessed  st@State{..} =        Prelude.null $ unguessed st
failed   st@State{..} = (not . Prelude.null $ unguessed st) && n >= maxN where n = length (wrongGuesses st)

unguessed State{guesses,phrase} = Prelude.foldr tryGuess phraseNoSpaces guesses
  where tryGuess (One c) acc | c `elem` phrase = without c acc
                             | otherwise     = acc
        tryGuess (All s) acc | s == phrase     = []
                             | otherwise     = acc

        phraseNoSpaces = Prelude.filter (/= ' ') phrase

without x = Prelude.filter (/= x)



render ∷ State → Image
render st@State{..} = [ renderWord (justOnes guesses) phrase
                      , ""
                      , renderHangar st ]

justOnes = concatMap (\x -> case x of (One c) -> [c]; _ -> [])

displayState ∷ State → IO ()
displayState = putStrLn . intercalate "\n" . render

type Image = [[Char]]

renderWord guesses phrase = renderPosition <$> phrase
  where renderPosition c | c `elem` guesses = toUpper c
                         | otherwise        = '_'

renderHangar :: State -> String
renderHangar st@State{..} = sort wrong ++ replicate (0 `max` maxN - n) '-' ++ "|"
  where n     = length wrong
        wrong = justOnes $ wrongGuesses st


play ∷ Int → String → IO ()
play maxN phrase = go (begin maxN phrase)
  where go st = do clearScreen
                   if finished st
                     then congratulate st
                     else do displayState st
                             g <- getGuess
                             go $ guess g st

getGuess ∷ IO Guess
getGuess = do gs <- stripInput <$> getLine
              case gs of []  -> getGuess
                         [c] -> return (One c)
                         gs  -> return (All gs)

stripInput :: String -> String
stripInput s = reverse $ dropWhile (== ' ') $ reverse $ dropWhile (== ' ') s

congratulate st@State{phrase} | guessed st = putStrLn $ "you won!! " ++ (toUpper <$> phrase) ++ "\n"
                            | failed st  = putStrLn $ "you lost :( the phrase was " ++ (toUpper <$> phrase) ++ "\n"

getWord = do putStrLn "Enter a phrase:"
             phrase <- getLine
             case phrase of "" -> getWord
                            x  -> return x

clearScreen = putStrLn $ replicate 30 '\n'

