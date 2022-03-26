module Actions.Add where

import Prelude
  ( IO )
import Types
  ( ExerciseType( ToDoEx, DoneEx, OtherEx ), Exercises )
import Files.ExercisesFromFile
  ( getExercisesFromFile )
import Actions.UsefulForActions
  ( exsToFileAndUpdate )
import TypeClasses.GetFromUser
  ( getExerciseFromUser, myMZipWith )
import Control.Monad
  ( (>=>) )

addActions :: [ IO () ]
addActions = [ add ToDoEx, add DoneEx, add OtherEx ]

add :: ExerciseType -> IO ()
add = exsAfterAddition >=> exsToFileAndUpdate

exsAfterAddition :: ExerciseType -> IO Exercises
exsAfterAddition exType =
  myMZipWith (:) ( getExerciseFromUser exType ) getExercisesFromFile
