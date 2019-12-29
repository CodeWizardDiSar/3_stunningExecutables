{-# LANGUAGE LambdaCase,TypeSynonymInstances,FlexibleInstances #-} 
module Show where
import Prelude hiding (and,Nothing)
import Data.Function ((&))
import Renaming
import Useful (doSequentially,tabBefore)
import Types (StringFrom,makeStringFrom,Date,HopefullyExerciseName)
import Types (Exercises,Exercise(..),HopefullySome(..))
import ExercisesFromFile (exercises)

showList     = showFiltered`append`[showAll]
showFiltered =
  [printStringAndPrint "To Do"  toDo
  ,printStringAndPrint "Done"   done
  ,printStringAndPrint "Missed" missed]
showAll    = doSequentially showFiltered
printStringAndPrint = \a b->printMoreBeautiful a`andThen`filterAndPrint b
printMoreBeautiful  = \a  ->printString$concat ["\n\t",a,"\n"] 
filterAndPrint = \exType->exercises`unwrapAnd`(filter exType`and`printExs)
[done,missed,toDo] = 
  [\case(Done   _)  ->True;_->False
  ,\case(Missed _)  ->True;_->False
  ,\case(ToDo   _ _)->True;_->False]
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
