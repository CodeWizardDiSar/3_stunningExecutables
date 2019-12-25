{-# LANGUAGE LambdaCase,TypeSynonymInstances,FlexibleInstances #-} 
module Show where
import Prelude hiding (all,and,Nothing)
import Control.Arrow
import Data.Function
import Renaming
import Useful
import Types
import ExercisesFromFile 
import Menus

showList   = [showToDo,showDone,showMissed,showAll]
showToDo   = filterAndPrint toDo
showDone   = filterAndPrint done
showMissed = filterAndPrint missed
showAll    = filterAndPrint all
filterAndPrint = \exType->exercises`unwrapAnd`(filter exType`and`printExs)
done     = \case (Done   _)  ->True;_->False
missed   = \case (Missed _)  ->True;_->False
toDo     = \case (ToDo   _ _)->True;_->False
all      = \_                ->True
printExs = makeStringFrom`and`printString

instance StringFrom Exercises where
  makeStringFrom =
    forEach (makeStringFrom`and`tabBefore`and`appendNL)`and`glue 
appendNL = (`append`"\n")
instance StringFrom Exercise where
  makeStringFrom = \case
    Done (subjectName,exerciseNumber,exerciseName)->
      (indent`and`glue)
        [subjectName
        ,exerciseNumber
        ,makeStringFrom exerciseName]
    Missed (subjectName,exerciseNumber,exerciseName)   ->
      (indent`and`glue)
        [subjectName
        ,exerciseNumber
        ,makeStringFrom exerciseName]
    ToDo (subjectName,exerciseNumber,exerciseName) da->
      (indent`and`glue)
        [subjectName
        ,exerciseNumber
        ,makeStringFrom exerciseName
        ,makeStringFrom da]
indent             = (fill 15)&forEach
fill               = \i s->take i`from`(s&withInfiniteSpaces)
withInfiniteSpaces = \s->s`append`repeat ' '
instance StringFrom HopefullyExerciseName where
  makeStringFrom = \case
    Nothing     ->"No Name"
    IndeedItIs e->e 
instance StringFrom Date where
  makeStringFrom = \(d,m,y)->glue [makeStringFrom d,"/"
                                  ,makeStringFrom m,"/"
                                  ,makeStringFrom y]
instance StringFrom Int where
  makeStringFrom = convertToString
