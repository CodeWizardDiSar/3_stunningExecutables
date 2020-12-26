module Move where
import Prelude
  ( (.), not, filter, (-), (!!), (+), elem, Bool(..), (==), sequence, getLine, IO, Int, (>>=)
  , String, (>>) )
import Renaming
  ( printString, wrap, unwrapAnd, andThen, (>>>) )
import ExercisesFromFile
  ( getToDoExercises, getDoneExercises, getMissedExercises )
import Data.Function
  ( (&), ($) )
import Show
  ( printEx )
import FileManagement     
  ( writeToNextDataKeeper, updateVersion )
import Types
  ( Strings, Exercises, Exercise(..) )
import Add
  ( getDate )
import UsefulFunctions   
  ( printStrings )
import UsefulForActions
  ( combine, showSubjects, getChoice, getSubjects, writeExercisesToFile )
import ShowExercises
  ( showExercises, subIs, getChosen )
import Control.Monad
  ( (>=>) )

-- moveFrom list of actions
moveActions :: [ IO () ]
moveActions = [ moveFrom "todo", moveFrom "done", moveFrom "missed" ]

moveFrom :: String -> IO ()
moveFrom = \exType -> getAllExs exType >>= writeExercisesToFile >> updateVersion

getAllExs = \case
 "todo"  -> combine [ getToDoExercises >>= move, getDoneExercises, getMissedExercises ]
 "done"  -> combine [ getToDoExercises, getDoneExercises >>= move, getMissedExercises ]
 "missed"-> combine [ getToDoExercises, getDoneExercises, getMissedExercises >>= move ]

move = getChosen >=> moveChosen

-- Move Chosen
moveChosen = \(exs,subNum,exNum)->
 let sub = getSubjects exs !! (subNum - 1)
     ex = filter (subIs sub) exs !! (exNum - 1)
 in moveOld ex >>= \newEx -> newEx : ( filter (not . (== ex) ) exs ) & wrap

moveOld = \ex->
  printStrings [ "Move To?", "\t1: To Do", "\t2: Done", "\t3: Missed" ] >>
  getLine >>= \case
  "1" -> moveToToDo ex
  "2" -> moveTo Done ex
  "3" -> moveTo Missed ex
  _ -> printString "what?" >> moveOld ex

moveToToDo = \case 
  ToDo   a b -> ToDo a b & wrap
  Done   a -> getDate >>= (ToDo a >>> wrap)
  Missed a -> getDate >>= (ToDo a >>> wrap)
 
moveTo = \x-> \case
  ToDo   a b -> x a & wrap
  Done   a -> x a & wrap
  Missed a -> x a & wrap
