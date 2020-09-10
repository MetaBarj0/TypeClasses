module Main where

import ValidationCourse

main :: IO ()
{-
main = do
  putStrLn "Enter a first sentence:"
  left <- getLine
  putStrLn "Enter a second sentence:"
  right <- getLine
  putStrLn "Reporting if those 2 sentences are anagrams..."
  printFriendlyReport $ areAnagrams left right

printFriendlyReport :: AnagramReport -> IO ()
printFriendlyReport (AnagramReport (Left message)) = putStrLn $ "Cannot verify if those sentences are anagrams : " ++ show message
printFriendlyReport (AnagramReport (Right IsAnagram)) = putStrLn "Provided sentences are anagrams"
printFriendlyReport (AnagramReport (Right IsNotAnagram)) = putStrLn "Provided sentences are not anagrams"
-}

main = do
  putStrLn "Please, enter a user name."
  name <- getLine
  putStrLn "Please, enter a password."
  password <- getLine
  print $ DisplayableUserValidation $ makeUser name password
