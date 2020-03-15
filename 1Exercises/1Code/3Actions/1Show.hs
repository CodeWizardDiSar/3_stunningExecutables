module Show where
import Renaming          (convertIntToString,glue,append,forEach,and)
import Renaming          (printString,unwrapAnd,andThen,doNothing)
import Types             (Exercise(..),Exercises,Date,HopefullySome(..))
import Types             (HopefullyExerciseName,Show,show)
import UsefulFunctions   (doSequentially,tabBefore)
import Prelude           (Int,Bool(..),repeat,take,filter,($),concat)
import Prelude           ((||),(<),(&&),(==),not,IO)
import Data.List         (intercalate)
import Data.Function     ((.),(&))
import ExercisesFromFile (getExercises,getToDo,getDone,getMissed)
import UsefulForActions  (beautify,putTogether,printBeutified,sortChrono)

-- Show list of actions
showList =
 forEach (printHeader`andThen`) $ [showToDo,showDone,showMissed,showAll]

printHeader    = printBeutified header
showToDo       = showTitleGetDo "ToDo"   getToDo   (sortChrono`and`print)
showDone       = showTitleGetDo "Done"   getDone   print
showMissed     = showTitleGetDo "Missed" getMissed print
showAll        = doSequentially [showToDo,showDone,showMissed]

showTitleGetDo = \t g d-> printBeutified t`andThen`g`unwrapAnd`d
print          = show`and`printString
printEx        = show`and`printString :: Exercise->IO ()

header         = putTogether headerList
headerList     = ["Subject","Exercise Number","Exercise Name","Date"]

-- show for Exercises,Exercise,HopefullyExerciseName,Date,Int
instance Show Exercises where show = forEach (show`and`beautify)`and`glue 
instance Show Exercise where
 show = \case
  Done   (sub,num,name)     ->putTogether [sub,num,show name]
  Missed (sub,num,name)     ->putTogether [sub,num,show name]
  ToDo   (sub,num,name) date->putTogether [sub,num,show name,show date]
instance Show HopefullyExerciseName where
 show = \case Nothing->"No Name";IndeedItIs n->n 
instance Show Date where show = forEach show`and`intercalate "/"
instance Show Int  where show = convertIntToString
