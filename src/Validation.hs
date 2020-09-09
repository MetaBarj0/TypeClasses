module Validation
  ( makeUser,
  )
where

import Data.Char (isAlphaNum, isSpace)
import Data.Validation

newtype Error = Error {errorAsStringList :: [String]} deriving (Show)

instance Semigroup Error where
  Error left <> Error right = Error $ left <> right

newtype Name = Name {nameAsString :: String} deriving (Show)

newtype Password = Password {passwordAsString :: String} deriving (Show)

data User
  = User
      { userNameAsName :: Name,
        userPasswordAsPassword :: Password
      }
  deriving (Show)

makeUser :: String -> String -> Validation Error User
makeUser name password =
  User
    <$> validateName name
    <*> validatePassword password

data InputType = NameInput | PasswordInput

inputTypeAsString :: InputType -> String
inputTypeAsString inputType =
  case inputType of
    NameInput -> "name"
    PasswordInput -> "password"

validateName :: String -> Validation Error Name
validateName name =
  stripSpace name NameInput
    *> isOnlyAlphaNum name NameInput
    *> nameMustHaveValidLength (Name name)

validatePassword :: String -> Validation Error Password
validatePassword password =
  stripSpace password PasswordInput
    *> isOnlyAlphaNum password PasswordInput
    *> passwordMustHaveValidLength (Password password)

passwordMustHaveValidLength :: Password -> Validation Error Password
passwordMustHaveValidLength password
  | length (passwordAsString password) > 20 = Failure $ Error ["The password must not have more than 20 characters"]
  | otherwise = Success password

isOnlyAlphaNum :: String -> InputType -> Validation Error String
isOnlyAlphaNum input inputType
  | not $ all isAlphaNum input = Failure $ Error ["The specified " ++ inputTypeAsString inputType ++ " must not contain any non alphanumerical characters"]
  | otherwise = Success input

stripSpace :: String -> InputType -> Validation Error String
stripSpace [] inputType = Failure $ Error ["The specified " ++ inputTypeAsString inputType ++ " must not be empty"]
stripSpace (x : xs) inputType = if isSpace x then stripSpace xs inputType else Success $ stripReversed $ reverse $ x : xs
  where
    stripReversed :: String -> String
    stripReversed (x : xs) = if isSpace x then stripReversed xs else reverse (x : xs)

nameMustHaveValidLength :: Name -> Validation Error Name
nameMustHaveValidLength name
  | length (nameAsString name) > 15 = Failure $ Error ["The user name must not have more than 15 characters"]
  | otherwise = Success name
