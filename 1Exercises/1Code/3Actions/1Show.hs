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

-- Show list of actions
showList =
 forEach (printHeader`andThen`) $ [showToDo,showDone,showMissed,showAll]

printHeader    = printBeutified header
showToDo       = showTitleGetDo "ToDo"   getToDo   (sortChrono`and`print)
showDone       = showTitleGetDo "Done"   getDone   print
showMissed     = showTitleGetDo "Missed" getMissed print
showAll        = doSequentially [showToDo,showDone,showMissed]

printBeutified = beautify`and`printString
showTitleGetDo = \t g d-> printBeutified t`andThen`g`unwrapAnd`d
print          = show`and`printString
printEx        = show`and`printString :: Exercise->IO ()

beautify       = ("\t"`append`)`and`(`append`"\n")
header         = putTogether headerList
putTogether    = forEach ((`append`repeat ' ')`and`take 25)`and`glue
headerList     = ["Subject","Exercise Number","Exercise Name","Date"]

-- Sort Chronologically
sortChrono = \case 
 []    ->[]     
 ex:exs-> sortChrono (filter (before     ex) exs)`append`[ex]`append`
          sortChrono (filter (not.before ex) exs)
before   = \e1 e2->getTDate e2`isBefore`getTDate e2
isBefore = \[d,m,y] [d',m',y']-> y<y'||(y==y'&&m<m')||(y==y'&&m==m'&&d<d')

-- Sort Numerically
--sortNum = \case
-- [] -> []
-- ex:es -> sortNum (filter (lowerThan ex) exs)`append`[ex]`append`
--          sortNum (filter (not.lowerThan ex) exs)
--lowerThan = \ex1 ex2 ->
-- ex1&getData&\(_,n1,_)->ex1&getData&\(_,n2,_)->

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
