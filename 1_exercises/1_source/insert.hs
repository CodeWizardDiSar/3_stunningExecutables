module Insert where

import ForEverybody
  
insertExercise = putStrLn "insertExercise"
insertSubject  = putStrLn "insertSubject"

insert = do
  newline
  printStrings [ "1: Insert Exercise"
               , "2: Insert Subject" 
               ]
  newline
  n <- getNumber
  case n of 
    1 -> insertExercise
    2 -> insertSubject
