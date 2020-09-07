module Validation
  ( makeUser,
  )
where

import Data.Char (isAlphaNum, isSpace)
makeUser :: String -> String -> Either Error User
makeUser name password = do
  validatedName <- validateName name
  validatedPassword <- validatePassword password
  return $ User validatedName validatedPassword

validateName :: String -> Either Error Name
validateName name =
  stripSpace name
    >>= isOnlyAlphaNum
    >>= (Right . Name)
    >>= nameMustHaveValidLength

validatePassword :: String -> Either Error Password
validatePassword password =
  stripSpace password
    >>= isOnlyAlphaNum
    >>= (Right . Password)
    >>= passwordMustHaveValidLength


newtype Password = Password {passwordAsString :: String} deriving (Show)

newtype Error = Error {errorAsString :: String} deriving (Show)

newtype Name = Name {nameAsString :: String} deriving (Show)

data User
  = User
      { userNameAsName :: Name,
        userPasswordAsPassword :: Password
      }
  deriving (Show)

passwordMustHaveValidLength :: Password -> Either Error Password
passwordMustHaveValidLength password
  | length (passwordAsString password) > 20 = Left $ Error "The password must not have more than 20 characters"
  | otherwise = Right password

isOnlyAlphaNum :: String -> Either Error String
isOnlyAlphaNum password
  | not $ all isAlphaNum password = Left $ Error "The password must not contain any non alphanumerical characters"
  | otherwise = Right password

stripSpace :: String -> Either Error String
stripSpace [] = Left $ Error "The password must not be empty"
stripSpace (x : xs) = if isSpace x then stripSpace xs else Right $ stripReversed $ reverse $ x : xs
  where
    stripReversed :: String -> String
    stripReversed (x : xs) = if isSpace x then stripReversed xs else reverse (x : xs)

nameMustHaveValidLength :: Name -> Either Error Name
nameMustHaveValidLength name
  | length (nameAsString name) > 15 = Left $ Error "The user name must not have more than 15 characters"
  | otherwise = Right name
