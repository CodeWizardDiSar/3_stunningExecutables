module ToString where
import Prelude
  ( String, Int, IO, ($), (++), show, id, putStr )
import Renaming
  ( (>>>), forEach, glue )
import Types
  ( Strings, ExerciseData( ED ), ToDoExercise( toDoData, date )
  , DoneExercise( doneData ), MissedExercise( missedData )
  , Date( Date, day, month, year ), HopefullyExerciseName
  , Exercise( ToDo, Done, Missed ), Exercises
  , HopefullySome( IndeedItIs, Nothing )
  , ToDoExercise( ToDoExercise )
  , DoneExercise( DoneExercise ), MissedExercise( MissedExercise )
  , Day( Day ), Month( Month ), Year( Year ) )
import Data.Function
  ( (&) )
import Data.List.Split
  ( splitOn )
import Data.List
  ( intercalate )
import Helpers
  ( beautify, glue20CharsEach )

class ToString a where toString :: a -> String

instance ToString Date where
  toString ( Date ( Day day ) ( Month month) ( Year year ) ) =
    [ day, month, year ] & forEach toString & intercalate "/"

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
  toStringForUser = forEach ( toStringForUser >>> beautify ) >>> glue >>> toStringForUser

instance ToStringForUser Exercise where
  toStringForUser = \case
    ToDo ( ToDoExercise toDoData date ) -> glue20CharsEach $ toStrings toDoData ++
      [ toString date ]
    Done ( DoneExercise doneData ) -> glue20CharsEach $ toStrings doneData
    Missed ( MissedExercise missedData ) -> glue20CharsEach $ toStrings missedData

instance ToStringForUser ExerciseData where
  toStringForUser = toStrings >>> glue20CharsEach >>> beautify

instance ToStringForUser HopefullyExerciseName where
  toStringForUser = \case
    IndeedItIs n -> n 
    Nothing -> "No Name"

instance ToStringForUser String where
  toStringForUser = ( ++ "\n" )

instance ToStringForUser Strings where
  toStringForUser = forEach toStringForUser >>> glue >>> toStringForUser

print :: ToStringForUser a => a -> IO ()
print = toStringForUser >>> putStr
