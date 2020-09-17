module Reader
  ( ABConfig (..),
    welcomeMessageR,
    Reader (..),
  )
where

import Data.Char as Char

data ABConfig
  = ABConfig
      { don'tUseLetterE :: Bool,
        don'tUseLetterL :: Bool
      }

-- neat approach on filters
toUpperStr :: ABConfig -> String -> String
toUpperStr cfg str =
  filter passesFilters (fmap Char.toUpper str)
  where
    passesFilters :: Char -> Bool
    passesFilters c = all (\f -> f c) filters
    filters :: [Char -> Bool]
    filters =
      [ if don'tUseLetterL cfg then (/= 'L') else const True,
        if don'tUseLetterE cfg then (/= 'E') else const True
      ]

type MotD = String

type Username = String

welcomeMessage :: ABConfig -> Username -> MotD -> String
welcomeMessage cfg username motd =
  "Welcome, "
    ++ toUpperStr cfg username
    ++ ". The message of the day is : "
    ++ toUpperStr cfg motd

type FirstName = String

type LastName = String

type NickName = String

fullName :: ABConfig -> FirstName -> LastName -> NickName -> Username
fullName cfg first last nick =
  upperFirstName ++ " '" ++ upperNickName ++ "' " ++ upperLastName
  where
    upperFirstName = toUpperStr cfg first
    upperLastName = toUpperStr cfg last
    upperNickName = toUpperStr cfg nick

newtype Reader cfg a = Reader {runReader :: cfg -> a}

instance Functor (Reader cfg) where
  -- Perfectly fits the bill but... specific as composition acts on functions only
  -- fmap f (Reader run) = Reader (f . run)
  -- Below is more generic, think in terms of Functor for functorized value, though it's always a function
  fmap f (Reader run) = Reader (fmap f run)

instance Applicative (Reader cfg) where
  pure x = Reader (const x)
  (<*>) (Reader forRunFn) (Reader runFn) = Reader (\config -> let a = runFn config in forRunFn config a)

instance Monad (Reader cfg) where
  return = pure
  (>>=) (Reader run) nextFn =
    Reader
      ( \config ->
          let a = run config
              Reader f = nextFn a
           in f config
      )

ask :: Reader cfg cfg
ask = Reader id

asks :: (cfg -> a) -> Reader cfg a
asks = Reader

fullNameR :: FirstName -> LastName -> NickName -> Reader ABConfig Username
fullNameR first last nick = do
  upperFirst <- toUpperStrR first
  upperNick <- toUpperStrR nick
  upperLast <- toUpperStrR last
  return $ upperFirst ++ " '" ++ upperNick ++ "' " ++ upperLast

toUpperStrR :: String -> Reader ABConfig String
toUpperStrR str = do
  filters <-
    asks
      ( \config ->
          [ if don'tUseLetterE config then (/= 'E') else const True,
            if don'tUseLetterL config then (/= 'L') else const True
          ]
      )
  return $ filter (\c -> all (\f -> f c) filters) (fmap Char.toUpper str)

welcomeMessageR :: Username -> MotD -> Reader ABConfig String
welcomeMessageR username motd = do
  usernameR <- toUpperStrR username
  motdR <- toUpperStrR motd
  return $
    "Welcome, "
      ++ usernameR
      ++ ". The message of the day is : "
      ++ motdR

-- ultra interesting, need to check if the prime version is equivalent
local :: (cfg -> cfg') -> Reader cfg' a -> Reader cfg a
local transform (Reader f) = Reader $ f . transform

local' :: (cfg -> cfg') -> Reader cfg' a -> Reader cfg a
local' transform (Reader f) = do
  cfg <- ask
  return $ f $ transform cfg

-- applied hlint recommandations, I can see Functor properties used
local'' :: (cfg -> cfg') -> Reader cfg' a -> Reader cfg a
local'' transform (Reader f) = f . transform <$> ask
