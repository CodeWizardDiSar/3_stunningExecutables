module Delete where

import Prelude   
  ( IO, (>>=) )
import Types
  ( Exercises, ExerciseType( ToDoEx, DoneEx, MissedEx ) )
import Helpers
  ( combine, removeChosen )
import Renaming
  ( wrap, (.>) )
import ExercisesFromFile
  ( toDoExercises, doneExercises, missedExercises )
import UsefulForActions
  ( exsToFileAndUpdate )
import ShowExercises
  ( getChosen )
import Control.Monad
  ( (>=>) )

deleteActions :: [ IO () ]
deleteActions = [ delete ToDoEx, delete DoneEx, delete MissedEx ]

delete :: ExerciseType -> IO ()
delete = exsAfterDeletion >=> exsToFileAndUpdate

exsAfterDeletion :: ExerciseType -> IO Exercises
exsAfterDeletion = \case
  ToDoEx -> combine [ toDoExercises >>= deleteChosen, doneExercises, missedExercises ]
  DoneEx -> combine [ toDoExercises, doneExercises >>= deleteChosen, missedExercises ]
  MissedEx -> combine [ toDoExercises, doneExercises, missedExercises >>= deleteChosen ]

deleteChosen :: Exercises -> IO Exercises
deleteChosen = getChosen >=> ( removeChosen .> wrap )
