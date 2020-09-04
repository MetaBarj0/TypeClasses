module Anagram
  ( areAnagrams,
    AnagramReport (..),
    AnagramResult (..),
  )
where

import Data.Char (isPunctuation, isSpace, toLower)
import Data.List (sort)

data AnagramResult = IsAnagram | IsNotAnagram deriving (Show)

type AnagramError = String

newtype AnagramReport = AnagramReport {getAnagramReport :: Either AnagramError AnagramResult} deriving (Show)

-- instance Semigroup AnagramReport where
--   AnagramReport (Left message) <> _ = AnagramReport (Left message)
--   _ <> AnagramReport (Left message) = AnagramReport (Left message)
--   AnagramReport (Right IsNotAnagram) <> _ = AnagramReport (Right IsNotAnagram)
--   _ <> AnagramReport (Right IsNotAnagram) = AnagramReport (Right IsNotAnagram)
--   AnagramReport (Right IsAnagram) <> AnagramReport (Right IsAnagram) = AnagramReport (Right IsAnagram)
--
-- instance Monoid AnagramReport where
--   mempty = AnagramReport (Right IsAnagram)
--   mappend = (<>)

areAnagrams :: String -> String -> AnagramReport
areAnagrams left right
  | rightLength /= leftLenght = AnagramReport (Left "Sentence lengths differ.")
  | otherwise = areAnagramsOnNormalized leftNormalized rightNormalized
  where
    leftLenght = length leftNormalized
    rightLength = length rightNormalized
    leftNormalized = normalizeString left
    rightNormalized = normalizeString right

normalizeString :: String -> Maybe String
normalizeString "" = Nothing
normalizeString string =
  let normalizationResult = lowercase $ removeUnnecessaryCharacters string
   in case normalizationResult of
        "" -> Nothing
        _ -> Just normalizationResult

lowercase :: String -> String
lowercase = map toLower

removeUnnecessaryCharacters :: String -> String
removeUnnecessaryCharacters = filter (\c -> not (isPunctuation c) && not (isSpace c))

areAnagramsOnNormalized :: Maybe String -> Maybe String -> AnagramReport
areAnagramsOnNormalized Nothing _ = AnagramReport (Left "First sentence is invalid")
areAnagramsOnNormalized _ Nothing = AnagramReport (Left "Second sentence is invalid")
areAnagramsOnNormalized (Just left) (Just right) =
  if sortedLeft == sortedRight
    then AnagramReport (Right IsAnagram)
    else AnagramReport (Right IsNotAnagram)
  where
    sortedLeft = sort left
    sortedRight = sort right
