module Show where
import Prelude 
  ( Int, String, ($), IO, (>>), (>>=) )
import Renaming
  ( convertIntToString, glue, forEach, (>>>), printString, unwrapAnd, andThen )
import Types
  ( Exercise ( ToDo, Done, Missed ), Exercises, Date, HopefullySome( IndeedItIs, Nothing )
  , HopefullyExerciseName, Show, show )
import UsefulFunctions
  ( doSequentially )
import Data.List 
  ( intercalate )
import ExercisesFromFile
  ( getToDoExercises, getDoneExercises, getMissedExercises )
import UsefulForActions
  ( beautify, putTogether, printBeutified, sortChrono )

showActions :: [IO ()]
showActions = ( printHeader >> ) `forEach` [ showToDo, showDone, showMissed, showAll ]

printHeader :: IO ()
printHeader = printBeutified header

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
print = show >>> printString

printEx :: Exercise -> IO ()
printEx = show >>> printString

type Header = String
header :: Header
header = putTogether headerList

headerList :: [ String ]
headerList = [ "Subject", "Number", "Name", "Date" ]

instance Show Exercises where
  show = forEach (show >>> beautify) >>> glue 

instance Show Exercise where
  show = \case
    Done ( sub, num, name ) -> putTogether [ sub, num, show name ]
    Missed ( sub, num, name ) -> putTogether [ sub, num, show name ]
    ToDo ( sub, num, name ) date -> putTogether [ sub, num, show name, show date ]

instance Show HopefullyExerciseName where
  show = \case
    Nothing -> "No Name"
    IndeedItIs n -> n 

instance Show Date where
  show = forEach show >>> intercalate "/"

instance Show Int  where
  show = convertIntToString
