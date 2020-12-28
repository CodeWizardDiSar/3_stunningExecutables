module ToStringForUser where
import Prelude
  ( String, Int, ($), (++) )
import Types
  ( Exercises, Exercise( ToDo, Done, Missed ), Strings, HopefullyExerciseName
  , HopefullySome( IndeedItIs, Nothing ), Date( D ), ToDoExercise( ToDoExercise )
  , DoneExercise( DoneExercise ), MissedExercise( MissedExercise )
  , Subject, ExerciseData( subject, number, name ) )
import Renaming
  ( (>>>), forEach, glue, convertIntToString )
import UsefulForActions
  ( beautify, putTogether, printBeutified, sortChrono )
import Data.Function
  ( (&) )
import Data.List 
  ( intercalate )

-- Even more of a pleasure, type classes
class ToStringForUser a where toStringForUser :: a -> String

instance ToStringForUser Exercises where
  toStringForUser = forEach ( toStringForUser >>> beautify) >>> glue 

instance ToStringForUser Exercise where
  toStringForUser = \case
    ToDo ( ToDoExercise toDoData date ) -> putTogether $ exerciseDataToStrings toDoData ++
      [ toStringForUser date ]
    Done ( DoneExercise doneData ) -> putTogether $ exerciseDataToStrings doneData
    Missed ( MissedExercise missedData ) -> putTogether $ exerciseDataToStrings missedData

exerciseDataToStrings :: ExerciseData -> Strings
exerciseDataToStrings exerciseData =
  [ subject exerciseData, number exerciseData, toStringForUser $ name exerciseData ]

instance ToStringForUser HopefullyExerciseName where
  toStringForUser = \case
    IndeedItIs n -> n 
    Nothing -> "No Name"

instance ToStringForUser Date where
  toStringForUser ( D d m y ) = [ d, m, y ] & forEach toStringForUser & intercalate "/"

instance ToStringForUser Int  where
  toStringForUser = convertIntToString
