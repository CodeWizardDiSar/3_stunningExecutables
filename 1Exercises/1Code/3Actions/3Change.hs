{-# LANGUAGE LambdaCase #-} 
module Change where
import Renaming

changeList   = [changeToDo,changeDone,changeMissed]
changeToDo   = printString "change To To Do"
changeDone   = printString "change To Done"
changeMissed = printString "change To Missed"
