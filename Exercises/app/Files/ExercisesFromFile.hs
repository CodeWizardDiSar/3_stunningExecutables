module Files.ExercisesFromFile where

import Prelude
  ( filter, IO, (>>=), (==) )
import Types 
  ( Exercise( ToDo, Done, Other ), ExerciseType( ToDoEx, DoneEx, OtherEx ), Exercises )
import TypeClasses.FromString
  ( fromFileString )
import VeryUseful.Renaming
  ( wrap, forEach, readFromFile, (.>), splitInLines )
import Files.FileManagement
  ( getCurrentDataKeeper, getVersion )

getExercisesFromFile :: IO Exercises
getExercisesFromFile =
  getVersion >>= \case
    "0"-> wrap []
    _  -> getCurrentDataKeeper >>= readFromFile >>=
          splitInLines .> ( forEach fromFileString ) .> wrap

getExercises :: [ IO Exercises ]
getExercises = [ toDoExercises, doneExercises, missedExercises ]
[ toDoExercises, doneExercises, missedExercises ] = [ get ToDoEx, get DoneEx, get OtherEx ]

get :: ExerciseType -> IO Exercises
get exType =
  getExercisesFromFile >>= filter ( toExerciseType .> ( == exType ) ) .> wrap

toExerciseType :: Exercise -> ExerciseType
toExerciseType = \case 
  ToDo _ -> ToDoEx
  Done _ -> DoneEx
  Other _ -> OtherEx
