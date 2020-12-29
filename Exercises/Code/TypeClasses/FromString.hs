module FromString where
import Prelude
  ( String, Int, ($), (++), undefined, read )
import Renaming
  ( (>>>), printErrorMessage, forEach, glue )
import Types
  ( Strings, Subject, ExerciseData( ED, subject, number, name ), ToDoExercise( ToDoExercise )
  , DoneExercise( DoneExercise ), MissedExercise( MissedExercise )
  , Date( Date, day, month, year )
  , HopefullyExerciseName , Exercise( ToDo, Done, Missed ), Exercises
  , HopefullySome( IndeedItIs, Nothing ), Day( Day ), Month( Month ), Year( Year ) )
import Data.Function
  ( (&) )
import Data.List.Split
  ( splitOn )
import Data.List
  ( intercalate )

class FromString a where fromString :: String -> a

instance FromString Int where
  fromString = read

instance FromString Day where
  fromString = fromString >>> Day

instance FromString Month where
  fromString = fromString >>> Month

instance FromString Year where
  fromString = fromString >>> Year

class FromFileString a where fromFileString :: String -> a

instance FromFileString Exercise where
  fromFileString = splitOn "," >>> fromFileStrings

instance FromFileString HopefullyExerciseName where
  fromFileString = \case
    "_" -> Nothing
    exName -> IndeedItIs exName

instance FromFileString Date where
  fromFileString = splitOn "/" >>> fromStrings

class FromUserString a where fromUserString :: String -> a

instance FromUserString HopefullyExerciseName where
  fromUserString = \case
    "" -> Nothing
    name -> IndeedItIs name

class FromStrings a where fromStrings :: Strings -> a

instance FromStrings Date where
  fromStrings = \case 
    [ d, m, y ] -> Date ( fromString d ) ( fromString m ) ( fromString y )
    _ -> printErrorMessage "Programmer messed up in collecting date info"

class FromFileStrings a where fromFileStrings :: Strings -> a

instance FromFileStrings Exercise where
  fromFileStrings = \case
    [ "t", s, exNum, exName, date ] -> ToDo $ fromFileStrings [ "t", s, exNum, exName, date ]
    [ "d", s, exNum, exName ] -> Done $ fromFileStrings [ "d", s, exNum, exName ]
    [ "m", s, exNum, exName ] -> Missed $ fromFileStrings [ "m", s, exNum, exName ]
    _ -> printErrorMessage "Line To Exercise" 

instance FromFileStrings ToDoExercise where
  fromFileStrings = \case
    [ "t", s, exNum, exName, date ] ->
      ToDoExercise ( ED s exNum (exName & fromFileString) ) ( date & fromFileString )
    _ -> printErrorMessage "Bad"

instance FromFileStrings DoneExercise where
  fromFileStrings = \case
    [ "d", s, exNum, exName ] -> DoneExercise $ ED s exNum (exName & fromFileString)
    _ -> printErrorMessage "Bad"

instance FromFileStrings MissedExercise where
  fromFileStrings = \case
    [ "m", s, exNum, exName ] -> MissedExercise $ ED s exNum (exName & fromFileString)
    _ -> printErrorMessage "Bad"

class FromUserStrings a where fromUserStrings :: Strings -> a

instance FromUserStrings ToDoExercise where
  fromUserStrings = undefined

instance FromUserStrings DoneExercise where
  fromUserStrings = fromUserStrings >>> DoneExercise

instance FromUserStrings MissedExercise where
  fromUserStrings = fromUserStrings >>> MissedExercise

instance FromUserStrings ExerciseData where
  fromUserStrings = \case
    [ subject, number, nameString ] -> ED subject number ( nameString & fromUserString )
    _ -> printErrorMessage "Programmer messed up in collecting exercise data from user"

