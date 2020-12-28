module ToStringForUser where
import Prelude
  ( String, Int, ($), (++) )
import Types
  ( Exercises, Exercise( ToDo, Done, Missed ), Strings, HopefullyExerciseName
  , HopefullySome( IndeedItIs, Nothing ), Date( D ), exerciseNumber, exerciseName )
import Renaming
  ( (>>>), forEach, glue, convertIntToString )
import Types
  ( Subject, ExerciseData( subjectName ), Exercise( exerciseData ) )
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
    Done exerciseData -> putTogether $ exerciseDataToStrings exerciseData
    Missed exerciseData -> putTogether $ exerciseDataToStrings exerciseData
    ToDo exerciseData date -> putTogether $ exerciseDataToStrings exerciseData ++
      [ toStringForUser date ]

exerciseDataToStrings :: ExerciseData -> Strings
exerciseDataToStrings exerciseData =
  [ subjectName exerciseData, exerciseNumber exerciseData
  , toStringForUser $ exerciseName exerciseData ]

instance ToStringForUser HopefullyExerciseName where
  toStringForUser = \case
    IndeedItIs n -> n 
    Nothing -> "No Name"

instance ToStringForUser Date where
  toStringForUser ( D d m y ) = [ d, m, y ] & forEach toStringForUser & intercalate "/"

instance ToStringForUser Int  where
  toStringForUser = convertIntToString
