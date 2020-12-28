module Show where
import Prelude 
  ( Int, String, ($), IO, (>>), (>>=), (++) )
import Types
  ( Exercise ( ToDo, Done, Missed ), Exercises, Date( D ), HopefullySome( IndeedItIs, Nothing )
  , HopefullyExerciseName , Strings, HeaderRow, Headers, ExerciseData( subject, number, name) )
import ToString
  ( toStringForUser )
import Renaming
  ( glue, forEach, (>>>), printString )
import UsefulFunctions
  ( doSequentially )
import Data.List 
  ( intercalate )
import ExercisesFromFile
  ( toDoExercises, doneExercises, missedExercises )
import UsefulForActions
  ( printBeutified, sortChrono )
import Data.Function
  ( (&) )
import Helpers
  ( putTogether )

showActions :: [IO ()]
showActions = forEach ( printHeaderRow >> ) [ showToDo, showDone, showMissed, showAll ]

printHeaderRow :: IO ()
printHeaderRow = printBeutified headerRow

showToDo :: IO ()
showToDo = showTitleGetDo "ToDo" toDoExercises ( sortChrono >>> print )

showDone :: IO ()
showDone = showTitleGetDo "Done" doneExercises print

showMissed :: IO ()
showMissed = showTitleGetDo "Missed" missedExercises print

showAll :: IO ()
showAll = doSequentially [ showToDo, showDone, showMissed ]

type Title = String

showTitleGetDo :: Title -> IO Exercises -> (Exercises -> IO ()) -> IO ()
showTitleGetDo = \t g d->
  printBeutified t >>
  g >>= d

print :: Exercises -> IO ()
print = toStringForUser >>> printString

printEx :: Exercise -> IO ()
printEx = toStringForUser >>> printString

headerRow :: HeaderRow
headerRow = putTogether headerList

headerList :: Headers
headerList = [ "Subject", "Number", "Name", "Date" ]
