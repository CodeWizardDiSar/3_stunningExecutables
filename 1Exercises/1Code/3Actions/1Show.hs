module Show where
import Renaming          (convertIntToString,glue,append,forEach,and)
import Renaming          (printString,unwrapAnd,andThen,doNothing)
import Types             (Exercise(..),Exercises,Date,HopefullySome(..))
import Types             (HopefullyExerciseName,Show,show)
import UsefulFunctions   (doSequentially,tabBefore)
import Prelude           (Int,Bool(..),repeat,take,filter,($),concat)
import Prelude           ((||),(<),(&&),(==),not)
import Data.List         (intercalate)
import Data.Function     ((.),(&))
import ExercisesFromFile (exercises)

-- Show list of actions
showList     = showFiltered`append`[doSequentially showFiltered]
showFiltered =
 [printHeader`andThen`
  printMoreBeautiful "To Do" `andThen`filterAndPrint sortChrono toDo  ,
  printMoreBeautiful "Done"  `andThen`filterAndPrint doNothing  done  ,
  printMoreBeautiful "Missed"`andThen`filterAndPrint doNothing  missed]
printHeader = printMoreBeautiful header
header = forEach ((`append`repeat ' ')`and`take 25) headerList&glue
headerList = ["Subject","Exercise Number","Exercise Name","Date"]
--take 15 ("Subject"`append`repeat' ') 
printMoreBeautiful = \a->printString$"\t"`append`a`append`"\n"
-- Filtering (and sorting for To Do)
filterAndPrint = \f exerciseType->
 exercises`unwrapAnd`(filter exerciseType`and`f`and`show`and`printString)
sortChrono = \case 
 ex:exs -> sortChrono (filter (before     ex) exs)`append`[ex]`append`
           sortChrono (filter (not.before ex) exs)
 []     -> []     
-- Chrono comparison of 2 To Do Exercises and of 2 Dates
before   = \(ToDo _ date1) (ToDo _ date2)->date2`isBefore`date1 
isBefore = \[d,m,y] [d',m',y']->
 y<y' || (y==y' && m<m') || (y==y' && m==m' && d<d')
-- Used for filtering
[toDo,done,missed] = [\case ToDo _ _->True;_->False,
                      \case Done _  ->True;_->False,
                      \case Missed _->True;_->False]
-- show for Exercises,Exercise,HopefullyExerciseName,Date,Int
instance Show Exercises where
 show = forEach (show`and`tabBefore`and`(`append`"\n"))`and`glue 
instance Show Exercise where
 show = \case
  Done   (sub,num,name)     ->putTogether [sub,num,show name]
  Missed (sub,num,name)     ->putTogether [sub,num,show name]
  ToDo   (sub,num,name) date->putTogether [sub,num,show name,show date]
-- For each fill with spaces till you hit 15 chars and glue them
putTogether = forEach ((`append`repeat ' ')`and`take 25)`and`glue
instance Show HopefullyExerciseName where
 show = \case Nothing->"No Name";IndeedItIs n->n 
instance Show Date where show = forEach show`and`intercalate "/"
instance Show Int  where show = convertIntToString
