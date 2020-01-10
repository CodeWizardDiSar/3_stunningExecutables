module ExercisesFromFile where
import Renaming        (unwrapAnd,wrap,unwrapped,forEach,andThen)
import Renaming        (readFromFile,printErrorMessage,printString)
import Renaming        (convertIntFromString)
import Renaming        (and,splitInLines,splitInWords)
import Types           (FromStringTo,toType,Date,Line)
import Types           (Exercises,HopefullyExerciseName)
import Types           (Exercise(..),HopefullySome(..))
import Prelude         (String,Int,IO)
import FileManagement  (currentDataKeeper,getVersion)
import Data.List.Split (splitOn)
import Data.Function   ((&))

-- exercises from file
exercises =
 getVersion`unwrapAnd`\case
 "0"->printString "Who you kiddin?"`andThen`wrap []
 _  ->readFromFile`unwrapped`currentDataKeeper`unwrapAnd`
  (splitInLines`and`forEach convertToExercise`and`wrap)::IO Exercises
convertToExercise   = (
 splitOn ","`and`\case
  ["d",subjectName,exerciseNumber,exerciseName]     ->
   Done   (subjectName,exerciseNumber,exerciseName&toType)
  ["m",subjectName,exerciseNumber,exerciseName]     ->
   Missed (subjectName,exerciseNumber,exerciseName&toType)
  ["t",subjectName,exerciseNumber,exerciseName,date]->
   ToDo   (subjectName,exerciseNumber,exerciseName&toType) (date&toType)
  _                                                 ->
   printErrorMessage "Line To Exercise")              ::Line->Exercise
-- toType for HopefullyExerciseName,Date,Int
instance FromStringTo HopefullyExerciseName where
 toType = \case "_"->Nothing;exerciseName->IndeedItIs exerciseName
instance FromStringTo Date where
 toType = splitOn "/"`and`\case
  [d,m,y]->forEach toType [d,m,y]
  _      ->printErrorMessage "Date"
instance FromStringTo Int where
 toType = convertIntFromString
