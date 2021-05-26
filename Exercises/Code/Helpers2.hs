module Helpers2 where

import Prelude 
  ( IO, (>>=), sequence ) 
import Types
  ( Exercises, ExerciseType( ToDoEx, DoneEx, OtherEx ) )
import ExercisesFromFile
  ( toDoExercises, doneExercises, missedExercises )
import Control.Monad
  ( (>=>) )
import Renaming
  ( (.>), glue, wrap ) 


exsAfter :: ( Exercises -> IO Exercises ) -> ExerciseType -> IO Exercises
exsAfter doToChosen = \case
  ToDoEx -> combine [ toDoExercises >>= doToChosen, doneExercises, missedExercises ]
  DoneEx -> combine [ toDoExercises, doneExercises >>= doToChosen, missedExercises ]
  OtherEx -> combine [ toDoExercises, doneExercises, missedExercises >>= doToChosen ]

combine :: [ IO Exercises ] -> IO Exercises
combine = sequence >=> ( glue .> wrap )

