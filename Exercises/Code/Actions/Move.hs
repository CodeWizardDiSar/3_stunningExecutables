module Move where

import Prelude
  ( (.), not, filter, (-), (!!), (+), elem, Bool( True, False ), (==), sequence, getLine, IO
  , Int, (>>=) , String, (>>) )
import Types
  ( Strings, Exercises, Exercise( ToDo, Done, Missed ), ExData
  , ToDoExercise( ToDoExercise ) , DoneExercise
  , MissedExercise 
  , ExercisesAndChosen( ExercisesAndChosen, chosen ) )
import Helpers 
  ( combine, removeChosen )
import Renaming
  ( wrap, (.>) )
import ExercisesFromFile
  ( toDoExercises, doneExercises, missedExercises )
import Data.Function
  ( (&), ($) )
import FileManagement     
  ( writeToNextDataKeeper, updateVersion )
import GetFromUser
  ( getFromUser )
import UsefulFunctions   
  ( printStrings )
import UsefulForActions
  ( exsToFileAndUpdate )
import ToSubject
  ( toSubjects )
import ShowExercises
  ( subIs, getChosen )
import Control.Monad
  ( (>=>) )
import ToString
  ( print )

moveActions :: [ IO () ]
moveActions = [ moveFrom "todo", moveFrom "done", moveFrom "missed" ]

moveFrom :: String -> IO ()
moveFrom = getAllExs >=> exsToFileAndUpdate

getAllExs :: String -> IO Exercises
getAllExs = \case
  "todo"  -> combine [ toDoExercises >>= move, doneExercises, missedExercises ]
  "done"  -> combine [ toDoExercises, doneExercises >>= move, missedExercises ]
  "missed"-> combine [ toDoExercises, doneExercises, missedExercises >>= move ]

move :: Exercises -> IO Exercises
move = getChosen >=> moveChosen2

moveChosen2 :: ExercisesAndChosen -> IO Exercises
moveChosen2 exercisesAndChosen =
  moveOld ( chosen exercisesAndChosen ) >>= ( : removeChosen exercisesAndChosen ) .> wrap

moveOld :: Exercise -> IO Exercise
moveOld = \ex ->
  printStrings [ "Move To?", "\t1: To Do", "\t2: Done", "\t3: Missed" ] >>
  getLine >>= \case
    "1" -> moveToToDo ex
    "2" -> moveTo Done ex
    "3" -> moveTo Missed ex
    _ -> print "what?" >> moveOld ex

moveToToDo :: Exercise -> IO Exercise
moveToToDo = \case 
  ToDo a -> ToDo a & wrap
  Done a -> getFromUser >>= ( ToDoExercise a .> ToDo .> wrap )
  Missed a -> getFromUser >>= ( ToDoExercise a .> ToDo .> wrap )
 
moveTo :: ( ExData -> Exercise ) -> Exercise -> IO Exercise
moveTo = \x -> \case
  ToDo ( ToDoExercise a b ) -> x a & wrap
  Done a -> x a & wrap
  Missed a -> x a & wrap
