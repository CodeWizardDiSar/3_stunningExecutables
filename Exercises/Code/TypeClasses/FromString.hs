module FromString where

import Prelude
  ( String, Int, ($), (++), undefined, read )
import Renaming
  ( (.>), printErrorMessage, forEach, glue )
import Types
  ( Strings, Subject, ExData( ED, subject, number, name ), ToDoExercise( ToDoExercise )
  , DoneExercise, MissedExercise
  , Date( Date, day, month )
  , HopefullyExName , Exercise( ToDo, Done, Missed ), Exercises
  , HopefullySome( IndeedItIs, Nothing ), Day, Month )
import Data.Function
  ( (&) )
import Data.List.Split
  ( splitOn )
import Data.List
  ( intercalate )

class FromString a where fromString :: String -> a

instance FromString Int where
  fromString = read

class FromFileString a where fromFileString :: String -> a

instance FromFileString Exercise where
  fromFileString = splitOn "," .> fromFileStrings

instance FromFileString HopefullyExName where
  fromFileString = \case
    "_" -> Nothing
    exName -> IndeedItIs exName

instance FromFileString Date where
  fromFileString = splitOn "/" .> fromStrings

class FromUserString a where fromUserString :: String -> a

instance FromUserString HopefullyExName where
  fromUserString = \case
    "" -> Nothing
    name -> IndeedItIs name

class FromStrings a where fromStrings :: Strings -> a

instance FromStrings Date where
  fromStrings = \case 
    [ d, m ] -> Date ( fromString d ) ( fromString m )
    _ -> printErrorMessage "Programmer messed up in collecting date info"

class FromFileStrings a where fromFileStrings :: Strings -> a

instance FromFileStrings Exercise where
  fromFileStrings = \case
    [ "t", s, exNum, exName, date ] -> ToDo $ fromFileStrings [ "t", s, exNum, exName, date ]
    [ "d", s, exNum, exName ] -> Done $ fromFileStrings [ s, exNum, exName ]
    [ "m", s, exNum, exName ] -> Missed $ fromFileStrings [ s, exNum, exName ]
    _ -> printErrorMessage "Line To Exercise" 

instance FromFileStrings ToDoExercise where
  fromFileStrings = \case
    [ "t", s, exNum, exName, date ] ->
      ToDoExercise ( ED s exNum (exName & fromFileString) ) ( date & fromFileString )
    _ -> printErrorMessage "Bad: ToDo"

instance FromFileStrings ExData where
  fromFileStrings = \case
    [ s, exNum, exName ] -> ED s exNum (exName & fromFileString)
    _ -> printErrorMessage "Bad: ExerciseData"

class FromUserStrings a where fromUserStrings :: Strings -> a

instance FromUserStrings ToDoExercise where
  fromUserStrings = undefined

instance FromUserStrings ExData where
  fromUserStrings = \case
    [ subject, number, nameString ] -> ED subject number ( nameString & fromUserString )
    _ -> printErrorMessage "Programmer messed up in collecting exercise data from user"
