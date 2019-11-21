{-# LANGUAGE LambdaCase #-} 

module Main where
import Types
import General
import Add
import FileToSubs
import SubsToFile

showToDo = putStrLn "showToDo"
change   = putStrLn "change"
moveDone = putStrLn "moveDone"
showDone = putStrLn "showDone"

main = do
  newline
  printString "Welcome to exercises manager" 
  newline 
  printStrings [ "1: Show To Do baby"
               , "2: Add exercise plz"
               , "3: I want to change something"
               , "4: Move exercise to Done baby"
               , "5: Show done so I feel good about myself"
               ] 
  newline 
  getNumber >>=
   \case 
    1 -> showToDo
    2 -> add
    3 -> change
    4 -> moveDone
    5 -> showDone
