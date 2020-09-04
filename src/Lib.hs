module Lib
  ( someFunc,
    spell,
    toAge
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

spell :: Integer -> String
spell i = case i of
  1 -> "One"
  2 -> "Two"
  3 -> "Three"
  4 -> "Four"
  _ -> "..."

toAge :: String -> Integer
toAge name = case name of
  "Mapie" -> 22
  "Eloane" -> 14
  "Camille" -> 4
  "Seb" -> 37
  _ -> -1
