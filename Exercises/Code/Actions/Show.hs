module Show where
import Renaming          (convertIntToString,glue,forEach,and,
                          printString,unwrapAnd,andThen)
import Types             (Exercise(..),Exercises,Date,
                          HopefullySome(..),
                          HopefullyExerciseName,Show,show)
import UsefulFunctions   (doSequentially)
import Prelude           (Int,($),IO)
import Data.List         (intercalate)
import ExercisesFromFile (getToDo,getDone,getMissed)
import UsefulForActions  (beautify,putTogether,printBeutified,
                          sortChrono)

-- Show list of actions
showActions = forEach (printHeader`andThen`)$[showToDo,showDone,
                                           showMissed,showAll]

printHeader    = printBeutified header
showToDo       = showTitleGetDo "ToDo"   getToDo   (sortChrono
                                                    `and`print)
showDone       = showTitleGetDo "Done"   getDone   print
showMissed     = showTitleGetDo "Missed" getMissed print
showAll        = doSequentially [showToDo,showDone,showMissed]

showTitleGetDo = \t g d-> printBeutified t`andThen`g`unwrapAnd`
                          d
print          = show`and`printString
printEx        = show`and`printString :: Exercise->IO ()

header         = putTogether headerList
headerList     = ["Subject","Exercise Number","Exercise Name",
                  "Date"]

-- show for Exercises,Exercise,HopefullyExerciseName,Date,Int
instance Show Exercises where
 show = forEach (show`and`beautify)`and`glue 
instance Show Exercise where
 show = \case
  Done   (sub,num,name)     ->putTogether [sub,num,show name]
  Missed (sub,num,name)     ->putTogether [sub,num,show name]
  ToDo   (sub,num,name) date->putTogether [sub,num,show name,
                                           show date]
instance Show HopefullyExerciseName where
 show = \case Nothing->"No Name";IndeedItIs n->n 
instance Show Date where
 show = forEach show`and`intercalate "/"
instance Show Int  where show = convertIntToString
