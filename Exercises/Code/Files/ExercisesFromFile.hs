module ExercisesFromFile where
import Prelude
  ( filter, IO, (>>=), (==) )
import Types 
  ( Exercise( ToDo, Done, Missed ), ExerciseType( ToDoEx, DoneEx, MissedEx ), Exercises )
import FromString
  ( fromFileString )
import Renaming
  ( wrap, forEach, readFromFile, (>>>), splitInLines )
import FileManagement
  ( getCurrentDataKeeper, getVersion )

getExercisesFromFile :: IO Exercises
getExercisesFromFile =
  getVersion >>= \case
    "0"-> wrap []
    _  -> getCurrentDataKeeper >>= readFromFile >>=
          splitInLines >>> ( forEach fromFileString ) >>> wrap

getExercises :: [ IO Exercises ]
getExercises = [ toDoExercises, doneExercises, missedExercises ]
[ toDoExercises, doneExercises, missedExercises ] = [ get ToDoEx, get DoneEx, get MissedEx ]

get :: ExerciseType -> IO Exercises
get exerciseType =
  getExercisesFromFile >>= filter ( toExerciseType >>> ( == exerciseType ) ) >>> wrap

toExerciseType :: Exercise -> ExerciseType
toExerciseType = \case 
  ToDo _ -> ToDoEx
  Done _ -> DoneEx
  Missed _ -> MissedEx
