{-# LANGUAGE LambdaCase,FlexibleInstances #-} 
module Show where
import Renaming          (convertIntToString,glue,append,from,forEach,and)
import Renaming          (printString,unwrapAnd,andThen,inputTo)
import Types             (Show,show,HopefullyExerciseName)
import Types             (Exercises,Date,Exercise(..),HopefullySome(..))
import UsefulFunctions   (doSequentially,tabBefore)
import Prelude           (Int,Bool(..),repeat,take,filter,($),concat)
import Data.List         (intercalate)
import ExercisesFromFile (exercises)
-- Show list of actions
showList     = showFiltered`append`[doSequentially showFiltered]
showFiltered =
 [printMoreBeautiful "To Do" `andThen`filterAndPrint toDo,
  printMoreBeautiful "Done"  `andThen`filterAndPrint done,
  printMoreBeautiful "Missed"`andThen`filterAndPrint missed]
printMoreBeautiful  = \a->printString$concat ["\t",a,"\n"] 
filterAndPrint = \exerciseType->
 exercises`unwrapAnd`(filter exerciseType`and`show`and`printString)
[toDo,missed,done] = 
 [\case ToDo _ _->True;_->False,
  \case Done _  ->True;_->False,
  \case Missed _->True;_->False]
-- Instances of Show for types
--  Exercises,Exercise,HopefullyExerciseName,Date,Int
instance Show Exercises where
 show = forEach (show`and`tabBefore`and`(`append`"\n"))`and`glue 
instance Show Exercise where
 show = \case
  Done   (sub,exNum,exName)        -> putTogether [sub,exNum,show exName]
  Missed (sub,exNum,exName)        -> putTogether [sub,exNum,show exName]
  ToDo   (sub,exNumber,exName) date->
   putTogether [sub,exNumber,show exName,show date]
putTogether = forEach ((`append`repeat ' ')`and`take 15)`and`glue
instance Show HopefullyExerciseName where
 show = \case Nothing->"No Name";IndeedItIs e->e 
instance Show Date where
 show = \(d,m,y)->forEach show [d,m,y]`inputTo`intercalate "/"
instance Show Int where
 show = convertIntToString
