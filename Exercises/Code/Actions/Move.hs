module Move where
import Prelude
  ( (.), not, filter, (-), (!!), (+), elem, Bool( True, False ), (==), sequence, getLine, IO
  , Int, (>>=) , String, (>>) )
import Renaming
  ( printString, wrap, (>>>) )
import ExercisesFromFile
  ( toDoExercises, doneExercises, missedExercises )
import Data.Function
  ( (&), ($) )
import Show
  ( printEx )
import FileManagement     
  ( writeToNextDataKeeper, updateVersion )
import Types
  ( Strings, Exercises, Exercise( ToDo, Done, Missed ), ExerciseData
  , ToDoExercise( ToDoExercise ) , DoneExercise( DoneExercise )
  , MissedExercise( MissedExercise ) )
import Add
  ( dateFromUser )
import UsefulFunctions   
  ( printStrings )
import UsefulForActions
  ( combine, showSubjects, getChoice, exercisesToSubjects, writeExercisesToFile )
import ShowExercises
  ( showExercises, subIs, getChosen )
import Control.Monad
  ( (>=>) )

-- moveFrom list of actions
moveActions :: [ IO () ]
moveActions = [ moveFrom "todo", moveFrom "done", moveFrom "missed" ]

moveFrom :: String -> IO ()
moveFrom = \exType -> getAllExs exType >>= writeExercisesToFile >> updateVersion

getAllExs :: String -> IO Exercises
getAllExs = \case
 "todo"  -> combine [ toDoExercises >>= move, doneExercises, missedExercises ]
 "done"  -> combine [ toDoExercises, doneExercises >>= move, missedExercises ]
 "missed"-> combine [ toDoExercises, doneExercises, missedExercises >>= move ]

move :: Exercises -> IO Exercises
move = getChosen >=> moveChosen

moveChosen :: ( Exercises, Int, Int ) -> IO Exercises
moveChosen = \( exs, subNum, exNum ) ->
 let sub = exercisesToSubjects exs !! ( subNum - 1 )
     ex = filter (subIs sub) exs !! ( exNum - 1 )
 in moveOld ex >>= \newEx -> newEx : ( filter (not . (== ex) ) exs ) & wrap

moveOld :: Exercise -> IO Exercise
moveOld = \ex ->
  printStrings [ "Move To?", "\t1: To Do", "\t2: Done", "\t3: Missed" ] >>
  getLine >>= \case
    "1" -> moveToToDo ex
    "2" -> moveTo ( DoneExercise >>> Done ) ex
    "3" -> moveTo ( MissedExercise >>> Missed ) ex
    _ -> printString "what?" >> moveOld ex

moveToToDo :: Exercise -> IO Exercise
moveToToDo = \case 
  ToDo a -> ToDo a & wrap
  Done ( DoneExercise a ) -> dateFromUser >>= ( ToDoExercise a >>> ToDo >>> wrap )
  Missed ( MissedExercise a ) -> dateFromUser >>= ( ToDoExercise a >>> ToDo >>> wrap )
 
moveTo :: ( ExerciseData -> Exercise ) -> Exercise -> IO Exercise
moveTo = \x -> \case
  ToDo ( ToDoExercise a b ) -> x a & wrap
  Done ( DoneExercise a ) -> x a & wrap
  Missed ( MissedExercise a ) -> x a & wrap
