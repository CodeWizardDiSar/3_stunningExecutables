module Delete where
import Renaming (printString)

-- Delete list of actions
deleteList   = [deleteToDo,deleteDone,deleteMissed]
deleteToDo   = printString "Delete From To Do"
deleteDone   = printString "Delete From Done"
deleteMissed = printString "Delete From Missed"
