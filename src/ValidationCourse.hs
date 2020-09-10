module ValidationCourse
  ( Anagram.areAnagrams,
    Anagram.AnagramReport (..),
    Anagram.AnagramResult (..),
    Validation.makeUser,
    DisplayableUserValidation (..),
  )
where

import Anagram
import Validation

-- Hidden stuff

-- polymorphic infered
function x y = if x > y then x + 10 else y

-- monomorphic defined
function2 :: Integer -> Integer -> Integer
function2 x y = if x > y then x + 10 else y

function3 :: Integer -> Integer -> Integer
function3 x y = case x `compare` y of
  GT -> x + 10
  _ -> y
