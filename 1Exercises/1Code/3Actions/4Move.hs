module Move where
import Renaming (printString)

moveFromList   = [moveFromToDo,moveFromDone,moveFromMissed]
moveFromToDo   = printString "move From To Do"
moveFromDone   = printString "move From Done"
moveFromMissed = printString "move From Missed"
moveToList   = [moveToToDo,moveToDone,moveToMissed]
moveToToDo   = printString "move To To Do"
moveToDone   = printString "move To Done"
moveToMissed = printString "move To Missed"
