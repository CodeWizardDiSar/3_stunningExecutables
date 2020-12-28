module FromString where
import Prelude
  ( String, Int, ($), (++) )
import Renaming
  ( (>>>), printErrorMessage, convertIntFromString, convertIntToString, forEach, glue )
import Types
  ( Strings, Subject, ExerciseData( ED, subjectName, exerciseNumber, exerciseName )
  , Date( D, day, month, year )
  , HopefullyExerciseName , Exercise( exerciseData, ToDo, Done, Missed ), Exercises
  , HopefullySome( IndeedItIs, Nothing ) )
import Data.Function
  ( (&) )
import Data.List.Split
  ( splitOn )
import Data.List
  ( intercalate )

class FromString a where fromString :: String -> a
class FromStrings a where fromStrings :: Strings -> a

instance FromString Exercise where
  fromString = splitOn "," >>> fromStrings

instance FromString HopefullyExerciseName where
  fromString = \case
    "_" -> Nothing
    exName -> IndeedItIs exName

instance FromString Date where
  fromString = splitOn "/" >>> \case
    [ d, m, y ] -> D ( fromString d ) ( fromString m ) ( fromString y )
    _ -> printErrorMessage "Date"

instance FromString Int where
  fromString = convertIntFromString

instance FromStrings ExerciseData where
  fromStrings = \case
    [ subjectName, exerciseNumber, exerciseNameString ] ->
      ED subjectName exerciseNumber ( exerciseNameString & stringToHopefullyExerciseName )
    _ -> printErrorMessage "Programmer messed up in collecting exercise info from user"

stringToHopefullyExerciseName :: String -> HopefullyExerciseName
stringToHopefullyExerciseName = \case
  "" -> Nothing
  exerciseName -> IndeedItIs exerciseName

instance FromStrings Date where
  fromStrings = \case 
    [ d, m, y ] -> D ( fromString d ) ( fromString m ) ( fromString y )
    _ -> printErrorMessage "Programmer messed up in collecting date info from user"

instance FromStrings Exercise where
  fromStrings = \case
    [ "d", s, exNum, exName ] -> Done $ ED s exNum (exName & fromString)
    [ "m", s, exNum, exName ] -> Missed $ ED s exNum (exName & fromString)
    [ "t", s, exNum, exName, date ] ->
      ToDo ( ED s exNum (exName & fromString) ) ( date & fromString )
    _ -> printErrorMessage "Line To Exercise" 
