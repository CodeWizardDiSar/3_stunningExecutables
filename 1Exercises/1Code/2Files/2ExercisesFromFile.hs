module ExercisesFromFile where
import Renaming        (unwrapAnd,wrap,unwrapped,forEach,
                        andThen,readFromFile,printErrorMessage,
                        printString,convertIntFromString,and,
                        splitInLines,splitInWords)
import Types           (FromStringTo,toType,Date,Line,
                        Exercises,HopefullyExerciseName,
                        Exercise(..),HopefullySome(..))
import Prelude         (String,Int,IO,filter,Bool(..))
import FileManagement  (getCurrentDataKeeper,getVersion)
import Data.List.Split (splitOn)
import Data.Function   ((&))

-- exercises from file
getExercises =
 getVersion`unwrapAnd`\case
 "0"-> printString "Who you kiddin?"`andThen`wrap []
 _  ->
  getCurrentDataKeeper`unwrapAnd`readFromFile`unwrapAnd`
  (splitInLines`and`forEach toType`and`wrap)::IO Exercises

-- get To Do, Done, Missed
[getToDo,getDone,getMissed] = [get toDo,get done,get missed]
get = \x->getExercises`unwrapAnd`(filter x`and`wrap)
toDo   = \case ToDo _ _->True;_->False 
done   = \case Done _  ->True;_->False
missed = \case Missed _->True;_->False

-- toType for Exercise,HopefullyExerciseName,Date,Int
instance FromStringTo Exercise where
 toType = 
  splitOn ","`and`\case
   ["d",s,exNum,exName]     -> Done   (s,exNum,exName&toType)
   ["m",s,exNum,exName]     -> Missed (s,exNum,exName&toType)
   ["t",s,exNum,exName,date]-> ToDo   (s,exNum,exName&toType)
                                      (date&toType)
   _                        -> printErrorMessage
                                "Line To Exercise"
instance FromStringTo HopefullyExerciseName where
 toType = \case "_"->Nothing;exName->IndeedItIs exName
instance FromStringTo Date where
 toType = splitOn "/"`and`\case
  [d,m,y]->forEach toType [d,m,y]
  _      ->printErrorMessage "Date"
instance FromStringTo Int where toType = convertIntFromString
