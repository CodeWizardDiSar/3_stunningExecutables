module ToStringForFile where
import Prelude
  ( String, Int, ($), (++) )
import Renaming
  ( (>>>), convertIntToString, forEach, glue )
import Types
  ( Strings, ExerciseData( subjectName, exerciseNumber, exerciseName )
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
    Done exerciseData -> commaSeperate $ exerciseDataToStrings "d" exerciseData
    Missed exerciseData -> commaSeperate $ exerciseDataToStrings "m" exerciseData
    ToDo exerciseData date -> commaSeperate $ exerciseDataToStrings "t" exerciseData ++
      [ toStringForFile date ]

commaSeperate = intercalate ","

type ExerciseType = String
exerciseDataToStrings :: ExerciseType -> ExerciseData -> Strings
exerciseDataToStrings exerciseType exerciseData =
  [ exerciseType, subjectName exerciseData, exerciseNumber exerciseData
  , toStringForFile $ exerciseName exerciseData ]

instance ToStringForFile HopefullyExerciseName where
  toStringForFile = \case
    IndeedItIs e -> e 
    Nothing -> "_"

instance ToStringForFile Date where
  toStringForFile date =
    forEach toStringForFile [ day date, month date, year date ] & intercalate "/"

instance ToStringForFile Int where
  toStringForFile = convertIntToString
