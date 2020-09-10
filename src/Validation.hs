module Validation
  ( makeUser,
    DisplayableUserValidation (..),
  )
where

import Data.Char (isAlphaNum, isSpace)
import Data.Validation

newtype Error = Error {errorAsStringList :: [String]}

instance Show Error where
  show (Error errors) = foldr (\val acc -> acc ++ "- " ++ val ++ "\n") "" errors

-- needed for Validation type
instance Semigroup Error where
  Error left <> Error right = Error $ left <> right

newtype Name = Name {nameAsString :: String}

instance Show Name where
  show = nameAsString

newtype Password = Password {passwordAsString :: String}

instance Show Password where
  show = passwordAsString

data User
  = User
      { userNameAsName :: Name,
        userPasswordAsPassword :: Password
      }

instance Show User where
  show (User name password) =
    "User name :" ++ show name ++ "\n"
      ++ "Password :"
      ++ show password

makeUser :: String -> String -> Validation Error User
makeUser name password =
  User
    <$> validateName name
    <*> validatePassword password

newtype DisplayableUserValidation = DisplayableUserValidation (Validation Error User)

instance Show DisplayableUserValidation where
  show (DisplayableUserValidation (Failure err)) = "There are errors :\n" ++ show err
  show (DisplayableUserValidation (Success user)) = "User successfully created :\n" ++ show user

data InputType = NameInput | PasswordInput

instance Show InputType where
  show NameInput = "name"
  show PasswordInput = "password"

validateName :: String -> Validation Error Name
validateName name =
  case stripSpace name NameInput of
    Failure err -> Failure err
    Success stripped ->
      isOnlyAlphaNum stripped NameInput
        *> nameMustHaveValidLength (Name stripped)

validatePassword :: String -> Validation Error Password
validatePassword password =
  case stripSpace password PasswordInput of
    Failure err -> Failure err
    Success stripped ->
      isOnlyAlphaNum stripped PasswordInput
        *> passwordMustHaveValidLength (Password stripped)

passwordMustHaveValidLength :: Password -> Validation Error Password
passwordMustHaveValidLength password
  | length (passwordAsString password) > 20 = Failure $ Error ["The password must not have more than 20 characters"]
  | otherwise = Success password

isOnlyAlphaNum :: String -> InputType -> Validation Error String
isOnlyAlphaNum input inputType
  | not $ all isAlphaNum input = Failure $ Error ["The specified " ++ show inputType ++ " must not contain any non alphanumerical characters"]
  | otherwise = Success input

stripSpace :: String -> InputType -> Validation Error String
stripSpace [] inputType = Failure $ Error ["The specified " ++ show inputType ++ " must not be empty"]
stripSpace (x : xs) inputType = if isSpace x then stripSpace xs inputType else Success $ stripReversed $ reverse $ x : xs
  where
    stripReversed :: String -> String
    stripReversed (x : xs) = if isSpace x then stripReversed xs else reverse (x : xs)

nameMustHaveValidLength :: Name -> Validation Error Name
nameMustHaveValidLength name
  | length (nameAsString name) > 15 = Failure $ Error ["The user name must not have more than 15 characters"]
  | otherwise = Success name
