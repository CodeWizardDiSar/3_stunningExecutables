module Delete where

import Prelude   
  ( IO, (>>=) )
import Types
  ( Exercises, ExerciseType( ToDoEx, DoneEx, MissedEx ) )
import Helpers
  ( removeChosen )
import Helpers2
  ( exsAfter )
import Renaming
  ( wrap, (.>) )
import UsefulForActions
  ( exsToFileAndUpdate )
import ShowExercises
  ( getChosen )
import Control.Monad
  ( (>=>) )

deleteActions :: [ IO () ]
deleteActions = [ delete ToDoEx, delete DoneEx, delete MissedEx ]

delete :: ExerciseType -> IO ()
delete = exsAfter deleteChosen >=> exsToFileAndUpdate

deleteChosen :: Exercises -> IO Exercises
deleteChosen = getChosen >=> ( removeChosen .> wrap )
