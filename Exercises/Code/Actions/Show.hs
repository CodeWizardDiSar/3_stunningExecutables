module Show where
import Prelude 
  ( Int, String, ($), IO, (>>), (>>=), (++) )
import Types
  ( Exercise ( ToDo, Done, Missed ), Exercises, Date( D ), HopefullySome( IndeedItIs, Nothing )
  , HopefullyExerciseName , Strings, HeaderRow, Headers
  , ExerciseData( subjectName, exerciseNumber, exerciseName) )
import ToStringForUser
  ( toStringForUser )
import Renaming
  ( convertIntToString, glue, forEach, (>>>), printString )
import UsefulFunctions
  ( doSequentially )
import Data.List 
  ( intercalate )
import ExercisesFromFile
  ( getToDoExercises, getDoneExercises, getMissedExercises )
import UsefulForActions
  ( beautify, putTogether, printBeutified, sortChrono )
import Data.Function
  ( (&) )

showActions :: [IO ()]
showActions = ( printHeaderRow >> ) `forEach` [ showToDo, showDone, showMissed, showAll ]

printHeaderRow :: IO ()
printHeaderRow = printBeutified headerRow

showToDo :: IO ()
showToDo = showTitleGetDo "ToDo" getToDoExercises ( sortChrono >>> print )

showDone :: IO ()
showDone = showTitleGetDo "Done" getDoneExercises print

showMissed :: IO ()
showMissed = showTitleGetDo "Missed" getMissedExercises print

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
