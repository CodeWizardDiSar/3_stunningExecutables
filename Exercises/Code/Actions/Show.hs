module Show where
import Prelude 
  ( String, IO, (>>), (>>=) )
import Types
  ( HeaderRow, ExerciseType( ToDoEx, DoneEx, MissedEx ) )
import ToString
  ( print )
import Renaming
  ( forEach, (.>) )
import UsefulFunctions
  ( doSequentially )
import ExercisesFromFile
  ( toDoExercises, doneExercises, missedExercises )
import UsefulForActions
  ( sortChrono )
import Data.Function
  ( (&) )
import Helpers
  ( glue20CharsEach, beautify )

showActions :: [ IO () ]
showActions =
  forEach ( printBeautified headerRow >> ) [ show ToDoEx, show DoneEx, show MissedEx, showAll ]

printBeautified :: String -> IO ()
printBeautified = beautify .> print

headerRow :: HeaderRow
headerRow = glue20CharsEach [ "Subject", "Number", "Name", "Date" ]

show :: ExerciseType -> IO ()
show = \case
  ToDoEx -> printBeautified "ToDo" >> toDoExercises >>= sortChrono .> print
  DoneEx -> printBeautified "Done" >> doneExercises >>= print
  MissedEx -> printBeautified "Missed" >> missedExercises >>= print

showAll :: IO ()
showAll = doSequentially [ show ToDoEx, show DoneEx, show MissedEx ]
