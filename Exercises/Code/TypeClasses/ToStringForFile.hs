module ToStringForFile where
import Prelude
  ( String, Int, ($), (++) )
import Renaming
  ( (>>>), convertIntToString, forEach, glue )
import Types
  ( Strings, ExerciseData( subject, number, name ), ToDoExercise( toDoData, date )
  , DoneExercise( doneData ), MissedExercise( missedData )
  , Date( day, month, year ), HopefullyExerciseName
  , Exercise( ToDo, Done, Missed ), Exercises
  , HopefullySome( IndeedItIs, Nothing ) )
import Data.Function
  ( (&) )
import Data.List.Split
  ( splitOn )
import Data.List
  ( intercalate )

class ToStringForFile a where toStringForFile :: a -> String

instance ToStringForFile Exercises where
  toStringForFile = ( forEach ( toStringForFile >>> ( ++ "\n" ) ) ) >>> glue

instance ToStringForFile Exercise where
  toStringForFile = \case
    ToDo toDoExercise ->
      commaSeperate $ exerciseDataToStrings "t" 
        ( toDoData toDoExercise ) ++ [ toStringForFile $ date toDoExercise ]
    Done doneExercise ->
      commaSeperate $ exerciseDataToStrings "d" $ doneData doneExercise
    Missed missedExercise ->
      commaSeperate $ exerciseDataToStrings "m" $ missedData missedExercise

commaSeperate = intercalate ","

type ExerciseType = String
exerciseDataToStrings :: ExerciseType -> ExerciseData -> Strings
exerciseDataToStrings exerciseType exerciseData =
  [ exerciseType, subject exerciseData, number exerciseData
  , toStringForFile $ name exerciseData ]

instance ToStringForFile HopefullyExerciseName where
  toStringForFile = \case
    IndeedItIs e -> e 
    Nothing -> "_"

instance ToStringForFile Date where
  toStringForFile date =
    forEach toStringForFile [ day date, month date, year date ] & intercalate "/"

instance ToStringForFile Int where
  toStringForFile = convertIntToString
