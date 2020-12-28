module ToString where
import Prelude
  ( String, Int, ($), (++), show )
import Renaming
  ( (>>>), forEach, glue )
import Types
  ( Strings, ExerciseData( ED ), ToDoExercise( toDoData, date )
  , DoneExercise( doneData ), MissedExercise( missedData )
  , Date( D, day, month, year ), HopefullyExerciseName
  , Exercise( ToDo, Done, Missed ), Exercises
  , HopefullySome( IndeedItIs, Nothing )
  , ToDoExercise( ToDoExercise )
  , DoneExercise( DoneExercise ), MissedExercise( MissedExercise ) )
import Data.Function
  ( (&) )
import Data.List.Split
  ( splitOn )
import Data.List
  ( intercalate )
import Helpers
  ( beautify, putTogether )

class ToString a where toString :: a -> String

instance ToString Date where
  toString ( D day month year ) = [ day, month, year ] & forEach toString & intercalate "/"

instance ToString Int where
  toString = show

class ToStrings a where toStrings :: a -> Strings

instance ToStrings ExerciseData where
  toStrings ( ED subject number name ) = [ subject, number, toStringForUser $ name ]

class ToStringForFile a where toStringForFile :: a -> String

instance ToStringForFile Exercises where
  toStringForFile = ( forEach ( toStringForFile >>> ( ++ "\n" ) ) ) >>> glue

instance ToStringForFile Exercise where
  toStringForFile = \case
    ToDo toDoEx -> commaSeperate $ "t" : toStrings ( toDoData toDoEx ) ++ 
                     [ toString $ date toDoEx ]
    Done doneEx -> commaSeperate $ "d" : toStrings ( doneData doneEx )
    Missed missedEx -> commaSeperate $ "m" : toStrings ( missedData missedEx )

commaSeperate = intercalate ","

instance ToStringForFile HopefullyExerciseName where
  toStringForFile = \case
    IndeedItIs e -> e 
    Nothing -> "_"

class ToStringForUser a where toStringForUser :: a -> String

instance ToStringForUser Exercises where
  toStringForUser = forEach ( toStringForUser >>> beautify) >>> glue 

instance ToStringForUser Exercise where
  toStringForUser = \case
    ToDo ( ToDoExercise toDoData date ) -> putTogether $ toStrings toDoData ++
      [ toString date ]
    Done ( DoneExercise doneData ) -> putTogether $ toStrings doneData
    Missed ( MissedExercise missedData ) -> putTogether $ toStrings missedData

instance ToStringForUser HopefullyExerciseName where
  toStringForUser = \case
    IndeedItIs n -> n 
    Nothing -> "No Name"
