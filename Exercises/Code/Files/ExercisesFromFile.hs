module ExercisesFromFile where
import Prelude
  (Int, filter, Bool(..), IO, (>>=) )
import Renaming
  ( unwrapAnd, wrap, forEach, andThen, readFromFile, printErrorMessage, printString
  , convertIntFromString, (>>>), splitInLines )
import Types 
  ( FromStringTo, toType, Date, HopefullyExerciseName, Exercise(..), HopefullySome(..)
  , Exercises)
import FileManagement
  (getCurrentDataKeeper, getVersion)
import Data.List.Split
  (splitOn)
import Data.Function 
  ((&))

getExercisesFromFile :: IO Exercises
getExercisesFromFile =
  getVersion >>= \case
  "0"->
    wrap []
  _  ->
    getCurrentDataKeeper >>= readFromFile >>= ( splitInLines >>> (toType `forEach`) >>> wrap )

getExercises :: [ IO Exercises ]
getExercises = [ getToDoExercises, getDoneExercises, getMissedExercises ]
[ getToDoExercises, getDoneExercises, getMissedExercises ] = [ get toDo, get done, get missed ]

get :: ( Exercise -> Bool ) -> IO Exercises
get = \x -> getExercisesFromFile >>= (filter x >>> wrap)

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

instance FromStringTo Exercise where
  toType = 
    splitOn "," >>> \case
      [ "d", s, exNum, exName] -> Done (s, exNum, exName & toType)
      [ "m", s, exNum, exName] -> Missed (s, exNum, exName & toType)
      [ "t", s, exNum, exName, date] -> ToDo (s, exNum, exName & toType) ( date & toType)
      _ -> printErrorMessage "Line To Exercise"

instance FromStringTo HopefullyExerciseName where
  toType = \case
    "_" -> Nothing
    exName -> IndeedItIs exName

instance FromStringTo Date where
  toType = splitOn "/" >>> \case
    [d,m,y] -> toType `forEach` [d,m,y]
    _ -> printErrorMessage "Date"

instance FromStringTo Int where
  toType = convertIntFromString
