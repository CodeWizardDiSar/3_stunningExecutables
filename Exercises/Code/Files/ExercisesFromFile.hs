module ExercisesFromFile where
import Prelude
  ( Int, filter, Bool( True, False ), IO, (>>=), ($) )
import Types 
  ( Date( D ), HopefullyExerciseName, Exercise( ToDo, Done, Missed )
  , Strings , HopefullySome( IndeedItIs, Nothing ), Exercises, ExerciseData( ED ))
import TypeClasses
  ( fromString, FromString )
import Renaming
  ( unwrapAnd, wrap, forEach, andThen, readFromFile, printErrorMessage, printString
  , convertIntFromString, (>>>), splitInLines )
import FileManagement
  ( getCurrentDataKeeper, getVersion )
import Data.List.Split
  ( splitOn )
import Data.Function 
  ( (&) )

getExercisesFromFile :: IO Exercises
getExercisesFromFile =
  getVersion >>= \case
    "0"-> wrap []
    _  -> getCurrentDataKeeper >>= readFromFile >>= ( splitInLines >>> ( fromString `forEach` )
            >>> wrap )

getExercises :: [ IO Exercises ]
getExercises = [ getToDoExercises, getDoneExercises, getMissedExercises ]
[ getToDoExercises, getDoneExercises, getMissedExercises ] = [ get toDo, get done, get missed ]

get :: ( Exercise -> Bool ) -> IO Exercises
get = \x -> getExercisesFromFile >>= ( filter x >>> wrap )

toDo :: Exercise -> Bool
toDo = \case
  ToDo _ _ -> True
  _ -> False 

done :: Exercise -> Bool
done = \case
  Done _ -> True
  _ -> False

missed :: Exercise -> Bool
missed = \case
  Missed _ -> True
  _ -> False

instance FromString Exercise where
  fromString = splitOn "," >>> stringsToExericise

stringsToExericise :: Strings -> Exercise
stringsToExericise = \case
  [ "d", s, exNum, exName ] -> Done $ ED s exNum (exName & fromString)
  [ "m", s, exNum, exName ] -> Missed $ ED s exNum (exName & fromString)
  [ "t", s, exNum, exName, date ] -> ToDo ( ED s exNum (exName & fromString) ) ( date & fromString )
  _ -> printErrorMessage "Line To Exercise"

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
