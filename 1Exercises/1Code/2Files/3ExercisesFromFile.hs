{-# LANGUAGE LambdaCase,FlexibleInstances #-} 
module ExercisesFromFile where
import Prelude hiding (Nothing,and)
import Data.List.Split
import Data.Function
import Control.Arrow
import Types 
import FileManagement
import Renaming

exercises           = readCurrentDK`unwrapAnd`convertToWrappedExs
convertToWrappedExs = convertToExercises`and`wrap
readCurrentDK       = readFile`unwrapped`currentDataKeeper
convertToExercises  = splitInLines`and`forEach convertToExercise
convertToExercise   = splitInWords`and`\case
  ["d",subjectName,exerciseNumber,exerciseName]   ->
    Done   (subjectName,exerciseNumber,exerciseName&toType)
  ["m",subjectName,exerciseNumber,exerciseName]   ->
    Missed (subjectName,exerciseNumber,exerciseName&toType)
  ["t",subjectName,exerciseNumber,exerciseName,date]->
    ToDo   (subjectName,exerciseNumber,exerciseName&toType) (date&toType)
  _                ->printErrorMessage "Line To Exercise"
convertToExercises :: String->Exercises
convertToExercise  :: Line->Exercise

instance FromStringTo HopefullyExerciseName where
  toType = \case
    "_"         ->Nothing
    exerciseName->IndeedItIs exerciseName
instance FromStringTo Date where
  toType = splitOn "/"`and`\case
    [d,m,y]->(toType d,toType m,toType y)
    _      ->printErrorMessage "Date"
instance FromStringTo Int where
  toType = read
