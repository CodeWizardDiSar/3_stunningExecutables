module Main where

import ForEverybody
import Insert

showToDo = putStrLn "showToDo"
edit     = putStrLn "edit"
moveDone = putStrLn "moveDone"
showDone = putStrLn "showDone"

main = do
  newline
  printString "Welcome to exercises manager"
  newline
  printStrings [ "1: Show to do"
               , "2: Insert"
               , "3: Edit"
               , "4: Move to done"
               , "5: Show done"
               ]
  newline
  n <- getNumber
  case n of 
    1 -> showToDo
    2 -> insert
    3 -> edit
    4 -> moveDone
    5 -> showDone
