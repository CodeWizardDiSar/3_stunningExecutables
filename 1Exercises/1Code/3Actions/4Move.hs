{-# LANGUAGE LambdaCase #-} 
module Move where
import Renaming

moveFromList   = [moveFromToDo,moveFromDone,moveFromMissed]
moveFromToDo   = printString "change To To Do"
moveFromDone   = printString "change To Done"
moveFromMissed = printString "change To Missed"
moveToList   = [moveToToDo,moveToDone,moveToMissed]
moveToToDo   = printString "change To To Do"
moveToDone   = printString "change To Done"
moveToMissed = printString "change To Missed"
