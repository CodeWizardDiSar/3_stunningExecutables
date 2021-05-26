module Move where

import Prelude
  ( getLine, IO, (>>=), (>>) )
import Types
  ( Strings, Exercises, Exercise( ToDo, Done, Other ), ExData
  , ToDoExercise( ToDoExercise )
  , ExercisesAndChosen( ExercisesAndChosen, chosen ) 
  , ExerciseType( ToDoEx, DoneEx, OtherEx  ) )
import Helpers 
  ( removeChosen )
import Helpers2
  ( exsAfter )
import Renaming
  ( wrap, (.>) )
import Data.Function
  ( (&) )
import GetFromUser
  ( getFromUser )
import UsefulFunctions   
  ( printStrings )
import UsefulForActions
  ( exsToFileAndUpdate )
import ShowExercises
  ( getChosen )
import Control.Monad
  ( (>=>) )
import ToString
  ( print )

moveActions :: [ IO () ]
moveActions = [ moveFrom ToDoEx, moveFrom DoneEx, moveFrom OtherEx ]

moveFrom :: ExerciseType -> IO ()
moveFrom = exsAfter ( getChosen >=> moveChosen ) >=> exsToFileAndUpdate

moveChosen :: ExercisesAndChosen -> IO Exercises
moveChosen exercisesAndChosen =
  moveOld ( chosen exercisesAndChosen ) >>= ( : removeChosen exercisesAndChosen ) .> wrap

moveOld :: Exercise -> IO Exercise
moveOld = \ex ->
  printStrings [ "Move To?", "\t1: To Do", "\t2: Done", "\t3: Other" ] >>
  getLine >>= \case
    "1" -> moveToToDo ex
    "2" -> moveTo Done ex
    "3" -> moveTo Other ex
    _ -> print "what?" >> moveOld ex

moveToToDo :: Exercise -> IO Exercise
moveToToDo = \case 
  ToDo a -> ToDo a & wrap
  Done a -> getFromUser >>= ( ToDoExercise a .> ToDo .> wrap )
  Other a -> getFromUser >>= ( ToDoExercise a .> ToDo .> wrap )
 
moveTo :: ( ExData -> Exercise ) -> Exercise -> IO Exercise
moveTo = \x -> \case
  ToDo ( ToDoExercise a b ) -> x a & wrap
  Done a -> x a & wrap
  Other a -> x a & wrap
