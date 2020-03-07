module Move where
import Renaming (printString)

moveList   = [moveToDo,moveDone,moveMissed]
moveToDo   = printString "move From To Do"
moveDone   = printString "move From Done"
moveMissed = printString "move From Missed"
