module Delete where
import Prelude   
  ((.), not, filter, (-), (!!), (==), IO, String, Int, (>>=), (>>))
import Renaming
  (wrap)
import ExercisesFromFile
  (getToDoExercises, getDoneExercises, getMissedExercises)
import Data.Function
  ((&))
import StringFromExercises
  (exercisesToString)
import FileManagement
  (updateVersion)
import Types
  (Strings, Exercises, Exercise(..))
import UsefulForActions
  (combine, writeExercisesToFile, getSubjects)
import ShowExercises
  (getChosen, subIs)
import Control.Monad
  ((>=>))

-- deleteFrom list of actions
deleteActions :: [ IO () ]
deleteActions = [ deleteFrom "todo", deleteFrom "done", deleteFrom "missed" ]

deleteFrom :: String -> IO ()
deleteFrom exerciseType =
  deleteAndGetNewExercises exerciseType >>= writeExercisesToFile >> updateVersion

deleteAndGetNewExercises :: String -> IO Exercises
deleteAndGetNewExercises = \case
 "todo" ->
   combine [ getToDoExercises >>= deleteChosen, getDoneExercises, getMissedExercises ]
 "done" ->
   combine [ getToDoExercises, getDoneExercises >>= deleteChosen, getMissedExercises ]
 "missed" ->
   combine [ getToDoExercises, getDoneExercises, getMissedExercises >>= deleteChosen ]

deleteChosen :: Exercises -> IO Exercises
deleteChosen = getChosen >=> delete

delete :: (Exercises, Int, Int) -> IO Exercises
delete = \(exs, subNum, exNum)->
 let sub = getSubjects exs !! (subNum - 1)
     ex = filter (subIs sub) exs !! (exNum - 1)
 in filter ( not . (== ex) ) exs & wrap
