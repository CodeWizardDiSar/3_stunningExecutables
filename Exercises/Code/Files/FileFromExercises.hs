module StringFromExercises where
import Prelude
  ( String, Int, (++), ($) )
import Types 
  ( ToStringForFile, Date( day, month, year ), toStringForFile, HopefullyExerciseName
  , Exercises , HopefullySome( IndeedItIs, Nothing ), Strings , Exercise( ToDo, Done, Missed )
  , ExerciseData( subjectName, exerciseNumber, exerciseName ) )
import Renaming 
  ( forEach, (>>>), glue, convertIntToString )
import Data.List
  ( intercalate )
import Data.Function
  ( (&) )

exercisesToString :: Exercises -> String
exercisesToString = ( ( toStringForFile >>> ( ++ "\n" ) ) `forEach` ) >>> glue

instance ToStringForFile Exercise where
  toStringForFile = \case
    Done exerciseData -> intercalate "," $ exerciseDataToStrings "d" exerciseData
    Missed exerciseData -> intercalate "," $ exerciseDataToStrings "m" exerciseData
    ToDo exerciseData date -> intercalate "," $ exerciseDataToStrings "t" exerciseData ++
      [ toStringForFile date ]

type ExerciseType = String
exerciseDataToStrings :: ExerciseType -> ExerciseData -> Strings
exerciseDataToStrings exerciseType exerciseData =
  [ exerciseType, subjectName exerciseData, exerciseNumber exerciseData
  , toStringForFile $ exerciseName exerciseData ]

instance ToStringForFile HopefullyExerciseName where
  toStringForFile = \case
    Nothing -> "_"
    IndeedItIs e -> e 

instance ToStringForFile Date where
  toStringForFile date =
    forEach toStringForFile [ day date, month date, year date ] & intercalate "/"

instance ToStringForFile Int where
  toStringForFile = convertIntToString
