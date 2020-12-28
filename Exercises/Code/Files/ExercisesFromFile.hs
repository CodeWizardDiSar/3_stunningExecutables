module ExercisesFromFile where
import Prelude
  ( filter, Bool( True, False ), IO, (>>=) )
import Types 
  ( Exercise( ToDo, Done, Missed ) , Strings , HopefullySome( IndeedItIs, Nothing )
  , Exercises )
import FromString
  ( fromString )
import Renaming
  ( wrap, forEach, readFromFile, (>>>), splitInLines )
import FileManagement
  ( getCurrentDataKeeper, getVersion )

getExercisesFromFile :: IO Exercises
getExercisesFromFile =
  getVersion >>= \case
    "0"-> wrap []
    _  -> getCurrentDataKeeper >>= readFromFile >>= ( splitInLines >>> ( forEach fromString )
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
