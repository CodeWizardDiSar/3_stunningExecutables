{-# LANGUAGE LambdaCase #-} 

module Main where
import Types
import General
import Add
import FileToSubs

showToDo = putStrLn "showToDo"
edit     = putStrLn "edit"
moveDone = putStrLn "moveDone"
showDone = putStrLn "showDone"

--main = 
--newline >>
--printString "Welcome to exercises manager" >>
--newline >>
--printStrings [ "1: Show to do"
--             , "2: Insert"
--             , "3: Edit"
--             , "4: Move to done"
--             , "5: Show done"
--             ] >>
--newline >>
--getNumber >>=
-- \case 
--  1 -> showToDo
--  2 -> insert
--  3 -> edit
--  4 -> moveDone
--  5 -> showDone

main = fileSubs >>= print
