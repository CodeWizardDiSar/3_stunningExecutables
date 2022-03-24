module TypeClasses.ToString where

import Prelude
  ( String, Int, IO, ($), (++), show, id, putStr )
import Renaming
  ( (.>), forEach, glue )
import Types
  ( Strings, ExData( ED ), ToDoExercise( exData, date )
  , DoneExercise, OtherExercise
  , Date( Date, day, month ), HopefullyExName
  , Exercise( ToDo, Done, Other ), Exercises
  , HopefullySome( IndeedItIsAn, Nothing )
  , ToDoExercise( ToDoExercise )
  , Day, Month )
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
  toString ( Date day month ) =
    [ day, month ] & forEach toString & intercalate "/"

instance ToString Int where
  toString = show

class ToStrings a where toStrings :: a -> Strings

instance ToStrings ExData where
  toStrings ( ED subject number name ) = [ subject, number, toStringForUser $ name ]

class ToStringForFile a where toStringForFile :: a -> String

instance ToStringForFile Exercises where
  toStringForFile = ( forEach ( toStringForFile .> ( ++ "\n" ) ) ) .> glue

instance ToStringForFile Exercise where
  toStringForFile = \case
    ToDo toDoEx -> commaSeperate $ "t" : toStrings ( exData toDoEx ) ++ 
                     [ toString $ date toDoEx ]
    Done doneData -> commaSeperate $ "d" : toStrings doneData
    Other missedData -> commaSeperate $ "m" : toStrings missedData

commaSeperate = intercalate ","

instance ToStringForFile HopefullyExName where
  toStringForFile = \case
    IndeedItIsAn e -> e 
    Nothing -> "_"

class ToStringForUser a where toStringForUser :: a -> String

instance ToStringForUser Exercises where
  toStringForUser = forEach ( toStringForUser .> beautify ) .> glue .> toStringForUser

instance ToStringForUser Exercise where
  toStringForUser = \case
    ToDo ( ToDoExercise exData date ) -> glue20CharsEach $ toStrings exData ++
      [ toString date ]
    Done doneData -> glue20CharsEach $ toStrings doneData
    Other missedData -> glue20CharsEach $ toStrings missedData

instance ToStringForUser ExData where
  toStringForUser = toStrings .> glue20CharsEach .> beautify

instance ToStringForUser HopefullyExName where
  toStringForUser = \case
    IndeedItIsAn n -> n 
    Nothing -> "No Name"

instance ToStringForUser String where
  toStringForUser = ( ++ "\n" )

instance ToStringForUser Strings where
  toStringForUser = forEach toStringForUser .> glue .> toStringForUser

print :: ToStringForUser a => a -> IO ()
print = toStringForUser .> putStr
