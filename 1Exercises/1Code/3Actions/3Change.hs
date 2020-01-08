module Change where
import Renaming (printString)

changeList   = [changeToDo,changeDone,changeMissed]
changeToDo   = printString "change To Do"
changeDone   = printString "change Done"
changeMissed = printString "change Missed"
