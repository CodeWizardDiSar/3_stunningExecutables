module Show where
import Prelude 
  (Int, String, ($), IO, (>>), (>>=))
import Renaming
  (convertIntToString, glue, forEachIn, (>>>), printString, unwrapAnd, andThen)
import Types
  (Exercise(..), Exercises, Date, HopefullySome(..), HopefullyExerciseName, Show, show)
import UsefulFunctions
  (doSequentially)
import Data.List 
  (intercalate)
import ExercisesFromFile
  (getToDo, getDone, getMissed)
import UsefulForActions
  (beautify, putTogether, printBeutified, sortChrono)

-- Show list of actions
showActions :: [IO ()]
showActions = (printHeader >>)`forEachIn`[showToDo, showDone, showMissed, showAll]

printHeader :: IO ()
printHeader = printBeutified header

showToDo :: IO ()
showToDo = showTitleGetDo "ToDo" getToDo (sortChrono >>> print)

showDone :: IO ()
showDone = showTitleGetDo "Done" getDone print

showMissed :: IO ()
showMissed = showTitleGetDo "Missed" getMissed print

showAll :: IO ()
showAll = doSequentially [showToDo,showDone,showMissed]

type Title = String
showTitleGetDo :: Title -> IO Exercises -> (Exercises -> IO ()) -> IO ()
showTitleGetDo = \t g d->
  printBeutified t >>
  g >>= d

print :: Exercises -> IO ()
print = show >>> printString

printEx :: Exercise -> IO ()
printEx = show >>> printString

header = putTogether headerList

headerList = ["Subject","Number","Name", "Date"]

-- show for Exercises,Exercise,HopefullyExerciseName,Date,Int
instance Show Exercises where
 show = forEachIn (show >>> beautify) >>> glue 

instance Show Exercise where
  show = \case
    Done   (sub,num,name)      -> putTogether [sub, num, show name]
    Missed (sub,num,name)      -> putTogether [sub, num, show name]
    ToDo   (sub,num,name) date -> putTogether [sub, num, show name, show date]

instance Show HopefullyExerciseName where
  show = \case
    Nothing->
      "No Name"
    IndeedItIs n->
      n 

instance Show Date where
  show = forEachIn show >>> intercalate "/"

instance Show Int  where
  show = convertIntToString
