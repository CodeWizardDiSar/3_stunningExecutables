module Show where
import Prelude 
  ( String, IO, (>>), (>>=) )
import Types
  ( HeaderRow, Headers )
import ToString
  ( print )
import Renaming
  ( forEach, (>>>) )
import UsefulFunctions
  ( doSequentially )
import ExercisesFromFile
  ( toDoExercises, doneExercises, missedExercises )
import UsefulForActions
  ( sortChrono )
import Data.Function
  ( (&) )
import Helpers
  ( putTogether, beautify )

showActions :: [ IO () ]
showActions = forEach ( printHeaderRow >> ) [ showToDo, showDone, showMissed, showAll ]

printHeaderRow :: IO ()
printHeaderRow = beautify headerRow & print

showToDo :: IO ()
showToDo = beautify "ToDo" & print >> toDoExercises >>= sortChrono >>> print

showDone :: IO ()
showDone = beautify "Done" & print >> doneExercises >>= print

showMissed :: IO ()
showMissed = beautify "Missed" & print >> missedExercises >>= print

showAll :: IO ()
showAll = doSequentially [ showToDo, showDone, showMissed ]

headerRow :: HeaderRow
headerRow = putTogether headerList

headerList :: Headers
headerList = [ "Subject", "Number", "Name", "Date" ]
