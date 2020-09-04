module Myfmap where

db :: [(Integer, String)]
db = [(0, "Seb"), (1, "Mapie"), (2, "Eloane"), (3, "Camille")]

coucou :: Integer -> Maybe String
coucou entry = foo ("Coucou " ++) (lookup entry db)

foo :: (String -> String) -> (Maybe String -> Maybe String)
foo f Nothing = Nothing
foo f (Just string) = Just (f string) -- lifting context, Just go from right to left :D

